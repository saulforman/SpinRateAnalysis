setwd("/Users/saulforman/Documents/R - Baseball/Spin")
data <- read.csv("Cole.csv")

# Plotting spin rate by location
## turn game date into date object
data$game_date <- as.Date(data$game_date, format = "%Y-%m-%d")
## subset to fastballs
fb <- data[data$pitch_type == "FF",]
## convert spin to numeric
fb$release_spin_rate <- as.numeric(as.character(fb$release_spin_rate))
fb <- fb[which(!is.na(fb$release_spin_rate)),]
## split to sinkers before/after astros trade
si <- data[data$pitch_type == "FT",]
si <- si[which(!is.na(si$release_spin_rate)),]
si$release_spin_rate <- as.numeric(as.character(si$release_spin_rate))
si_before <- si[which(si$game_year != "2018"),]
si_after <- si[which(si$game_year == "2018"),]
## change zones
fb$plate_x <- as.numeric(as.character(fb$plate_x))
fb <- fb[which(!is.na(fb$plate_x)),]
fb$plate_z <- as.numeric(as.character(fb$plate_z))
fb <- fb[which(!is.na(fb$plate_z)),]
fb_before <- fb[fb$game_year != "2018",]
fb_before$release_speed <- as.numeric(as.character(fb_before$release_speed))
fb_after <- fb[fb$game_year == "2018",]
fb_after$release_speed <- as.numeric(as.character(fb_after$release_speed))
install.packages('tidyverse')
library('tidyverse')
## flip plate_x for lefties
fb <- mutate(fb, adj_plate_x = ifelse(stand == "L", 
                                      -plate_x, plate_x))
## Theme for ggplot
TH <- theme(
  plot.title = element_text(
    colour = "blue",
    size = 18,
    hjust = 0.5,
    vjust = 0.8,
    angle = 0
  )
)
## Function for plotting grid
myplot <- function(sc, ngrid = 10, Spin = FALSE){
  scnew <- filter(sc, adj_plate_x > -1.5,
                  adj_plate_x <  1.5,
                  plate_z > 1, plate_z < 4)
  scnew$cx <- cut(scnew$adj_plate_x, 
                  seq(-1.5, 1.5, length.out = ngrid))
  scnew$cz <- cut(scnew$plate_z, 
                  seq(1, 4, length.out = ngrid))
  scnew %>% group_by(cx, cz) %>% 
    summarize(N = n(),
              Mean_Velocity = mean(release_speed, na.rm=TRUE),
              Mean_Spin_Rate = mean(release_spin_rate, na.rm=TRUE)) -> S1
  
  grid_x <- seq(-1.5, 1.5, length.out = ngrid)
  mid_x <- (grid_x[-ngrid] + grid_x[-1]) / 2
  grid_z <- seq(1, 4, length.out = ngrid)
  mid_z <- (grid_z[-ngrid] + grid_z[-1]) / 2
  S1$x <- rep(mid_x, each = ngrid - 1)
  S1$z <- rep(mid_z, ngrid - 1)

  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  if (Spin == FALSE){
    p <- ggplot(kZone, aes(x, y)) +
      geom_tile(data=S1, 
                aes(x=x, y=z, fill= Mean_Velocity)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") + TH +
      ggtitle("Mean Velocity by Location") +
      coord_fixed() + xlab("Adjusted Plate X") + ylab("Plate Z")
  } else {
    p <- ggplot(kZone, aes(x, y)) +
      geom_tile(data=S1, 
                aes(x=x, y=z, fill= Mean_Spin_Rate)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") + TH +
      ggtitle("Mean Spin Rate by Location") +
      coord_fixed() + xlab("Adjusted Plate X") + ylab("Plate Z")
  }
  p
}
myplot(fb, 13, Spin = TRUE)
myplot(fb_before, 13, Spin = FALSE)
myplot(fb_after, 13, Spin = TRUE)

# Changepoint Analysis
## read-in data
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
## repeat again with cb spin
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
## join data frames for all pitches
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

## plot fastball speed
plot(mvalue_fb_speed)
## take difference and test for stationarity
diff1 <- diff(ts_fb_speed)
plot(diff1)
adf.test(diff1)
## find order of differenced time series
acf(diff1)
pacf(diff1)
eacf(diff1)
## test ARIMA(1,1,1) model
model1 <- arima(diff1,order = c(1,1,1))
model1
## test differenced moving average
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