setwd("/Users/saulforman/Documents/R - Baseball/Cole")
data <- read.csv("Cole.csv")
cb <- data[which(data$pitch_type == "KC"),]
myvars <- c("game_date","release_speed","release_spin_rate")
cb_small <- cb[,myvars]
myvars2 <- c("game_date","release_spin_rate")
cb_spin <- cb[,myvars2]

## clean
class(cb_spin$release_spin_rate)
cb_spin$release_spin_rate <- as.numeric(as.character(cb_spin$release_spin_rate))
mean(na.omit(cb_spin$release_spin_rate))
cb_spin_clean <- na.exclude(cb_spin)
class(cb_spin_clean$game_date)
print(cb_spin_clean$game_date[1])
cb_spin_clean$game_date <- as.Date(cb_spin_clean$game_date,
                                   format = "%Y-%m-%d")

## average by game
cb_spin_game <- aggregate(cb_spin_clean, 
                          by = list(cb_spin_clean$game_date),
                          FUN = mean, na.rm = TRUE)
cb_spin_final <- cb_spin_game[,myvars2]

## create time series
ts <- ts(c(cb_spin_final$release_spin_rate))
plot(ts, type = 'l')
library('TSA')

## stationary?
adf.test(ts)
ts_diff <- diff(ts)
adf.test(ts_diff)
plot(ts_diff)

## create base model for prediction
ts_pirates <- ts_diff[1:83]

## identify model order
acf(ts_pirates)
pacf(ts_pirates)
eacf(ts_pirates)

## ARIMA(0,1,1)
model1 <- arima(ts_pirates,
               order = c(0,1,1),
               include.mean = FALSE,
               method = "CSS")
model1

## ARMA(1,1,1)
model2 <- arima(ts_pirates,
                order = c(1,1,1),
                include.mean = FALSE,
                method = "CSS")
model2
summary(model2)
AIC(model2)

qqnorm(ts_pirates)
qqline(ts_pirates)

plot(ts_pirates^2, type = 'l')
acf(ts_pirates^2)
pacf(ts_pirates^2)
eacf(ts_pirates^2)
Box.test(ts_pirates)

model3 <- garch(ts_pirates, order = c(0,1))
model3
plot(model3$residuals, type = 'l')
AIC(model3)

ts_astros <- ts_diff[84:90]
time <- c(1:90)
values <- predict(model3, time, n.ahead = 7)
plot(x = time[2:90], y = ts_diff)
lines(time[2:90], values[2:90], col='red', lwd=3)
plot.Arima(model2, n.ahead = 7, ylab = "spin rate")
spin_pred_lb <- values$pred - 2*values$se

