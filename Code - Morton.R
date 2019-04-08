# read-in data
setwd("/Users/saulforman/Documents/R - Baseball/Spin/")
data <- read.csv("Data - Cole.csv")

# clean data
## turn release point readings into numerics
data$release_pos_x <- as.numeric(as.character(data$release_pos_x))
data <- data[which(!is.na(data$release_pos_x)),]
data$release_pos_z <- as.numeric(as.character(data$release_pos_z))
data <- data[which(!is.na(data$release_pos_z)),]
data$release_pos_y <- as.numeric(as.character(data$release_pos_y))
data <- data[which(!is.na(data$release_pos_y)),]
## turn game date into date object
data$game_date <- as.Date(data$game_date, format = "%Y-%m-%d")
## subset into fastball-only
fb <- data[data$pitch_type == "FF"|
             data$pitch_type == "FT",]

# create time series of velo
## subset to date, speed, and release point
vars1 <- c("game_date","release_speed","release_pos_x",
           "release_pos_y","release_pos_z")
fb_vars <- fb[,vars1]
fb_clean <- na.exclude(fb_vars)
## turn velo into numeric
class(fb_clean$release_speed)
fb_clean2 <- fb_clean
fb_clean2$release_speed <- as.numeric(as.character(fb_clean2$release_speed))
## take mean velo by game
fb_game_speed <- aggregate(fb_clean2, by = list(fb_clean2$game_date),
                     FUN = mean)
vars2 <- c("game_date","release_speed")
fb_speed <- fb_game_speed[,vars2]
colnames1 <- c("game_date","fb_release_speed")
colnames(fb_speed) <- colnames1

# create time series of spin
## subset to date, speed, and release point
vars3 <- c("game_date","release_spin_rate","release_pos_x",
           "release_pos_y","release_pos_z")
fb_vars2 <- fb[,vars3]
## turn spin into numeric
fb_clean3 <- fb_vars2
fb_clean3$release_spin_rate <- as.numeric(as.character(fb_clean3$release_spin_rate))
fb_clean4 <- na.exclude(fb_clean3)
## take mean spin rate by game
fb_game_spin <- aggregate(fb_clean4, by = list(fb_clean4$game_date),
                          FUN = mean)
vars3 <- c("game_date","release_spin_rate")
fb_spin <- fb_game_spin[,vars3]
colnames2 <- c("game_date","fb_release_spin_rate")
colnames(fb_spin) <- colnames2

# visualizing release point
## separate to before and after Astros transition
before <- fb_spin[1:86,]
before <- before[-44,]
game <- fb_game_spin[44,]
after <- fb_game_spin[87:93,]
## plot
library('plotly')
p <- plot_ly(fb_spin, x = ~release_pos_x, y = ~release_pos_y,
             z = ~release_pos_z,
             marker = list(color = ~release_spin_rate, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'X-Axis Release Position'),
                      yaxis = list(title = 'Y-Axis Release Position'),
                      zaxis = list(title = 'Z-Axis Release Position')))
p
## push to plotly
chart_link <- api_create(p, filename = "r-releasepoint", username = "saulo013")
chart_link
Sys.setenv("plotly_username"="saulo013")
Sys.setenv("plotly_api_key"="hqlO37PnbaEnInTK0hsn")

# do the same thing with cb speed
summary(data$pitch_type)
cb <- data[data$pitch_type == "CU",]
cb_vars <- cb[,vars2]
## turn velo into numeric
cb_clean <- cb_vars
cb_clean$release_speed <- as.numeric(as.character(cb_clean$release_speed))
## mean velo by date
cb_game_speed <- aggregate(cb_clean, by = list(cb_clean$game_date),
                           FUN = mean)
cb_speed <- cb_game_speed[,vars2]
## plot velo
plot(cb_speed, type = 'l')
colnames3 <- c("game_date","cb_release_speed")
colnames(cb_speed) <- colnames3

# repeat again with cb spin
cb_vars2 <- cb[,vars3]
cb_clean2 <- cb_vars2
cb_clean2$release_spin_rate <- as.numeric(as.character(cb_clean2$release_spin_rate))
cb_clean3 <- na.exclude(cb_clean2)
cb_game_spin <- aggregate(cb_clean3, by = list(cb_clean3$game_date),
                          FUN = mean)
cb_spin <-cb_game_spin[,vars3]
plot(cb_game_spin, type = 'l')
colnames4 <- c("game_date","cb_release_spin_rate")
colnames(cb_spin) <- colnames4

# identify changepoints
library('dplyr')
morton <- left_join(fb_speed, fb_spin, by = "game_date")
morton <- left_join(morton, cb_speed, by = "game_date")
morton <- left_join(morton, cb_spin, by = "game_date")
## changepoints in fastball velocity
library('TSA')
library('forecast')
library('changepoint')
ts_fb_speed <- ts(morton$fb_release_speed)
plot(ts_fb_speed)
bcp <- bcp(fb_spin[1:86,]$fb_release_spin_rate)
plot(bcp)
which.max(bcp$posterior.mean)

plot(mvalue_fb_speed)

diff1 <- diff(ts_fb_speed)
plot(diff1)
adf.test(diff1)

acf(diff1)
pacf(diff1)
eacf(diff1)

model1 <- arima(diff1,order = c(1,1,1))
model1

model2 <- arima(diff1, order = c(0,1,1))
model2

qqnorm(diff1)
qqline(diff1)

## changepoint
library('changepoint')
library('fpp')
mvalue <- bcp(diff1)
plot(mvalue)

## spin?
ts_fb_release_spin_rate <- ts(morton$fb_release_spin_rate)
plot(ts_fb_release_spin_rate)
install.packages('bcp')
library('bcp')
bcp <- bcp(y = morton$fb_release_spin_rate)
plot(bcp)
which.max(bcp$posterior.prob)
mvalue6 <- cpt.mean(ts_fb_release_spin_rate, method = "BinSeg")
plot(mvalue6, ylab = "Spin Rate - Fastball")

astros_fb_spin <- ts_fb_release_spin_rate[28:59]
mvalue_astros <- cpt.mean(astros_fb_spin)
plot(mvalue_astros)
diff2 <- diff(ts_fb_release_spin_rate)
plot(diff2)
adf.test(diff2)
qqnorm(diff2)
qqline(diff2)
mvalue2 <- cpt.mean(diff2, method = "BinSeg")
plot(mvalue2, ylab = "Differenced Spin Rate - Fastball")

## cb spin?
ts_cb_release_spin_rate <- ts(morton$cb_release_spin_rate)
plot(ts_cb_release_spin_rate)
adf.test(ts_cb_release_spin_rate)
mvalue5 <- cpt.mean(ts_cb_release_spin_rate, method = "BinSeg")
plot(mvalue5)

diff4 <- diff(ts_cb_release_spin_rate)
adf.test(diff4)
plot(diff4)
qqnorm(diff4)
qqline(diff4)
mvalue4 <- cpt.mean(diff4, method = "BinSeg")
plot(mvalue4)
