```{r}
## this is the code used to produce the time series section
## of the report on the chicago crime dataset.
##
## this code should be ran sequentially and will produce
## the exact same results as reported.


# setup -------------------------------------------------------------------


# import packages
library(tidyverse)
library(lubridate)
library(urca)
library(forecast)

# import data
data("all_crime")
data <- cleanData(all_crime)

# remove columns we are not interested in
data %<>%
  select(Date, Year, Ward)
glimpse(data)


# monthly data ------------------------------------------------------------
data %<>%
  mutate(Month = month(ymd(Date)))

# count crimes per day
count_data <- data %>%
  group_by(Year, Month) %>%
  count()

# make a ts object
ts_data <- ts(count_data$n, start=c(2002,1), frequency = 12)

# remove first few obs
ts_data <- window(ts_data, start=c(2002, 6), end=c(2019,9))

# plot ts
ts_data %>%
  ggplot(aes(x=time(ts_data), y=ts_data)) +
  geom_line() +
  labs(title = "Monthly Crimes in Chicago",
        x = "Time",
        y = "Number of Crimes")
ggsave("ts_monthly.pdf", device = "pdf", path="../figures",
       width = 5.64, height = 2.72)

# exploratory plot
monthly_ts <- autoplot(ts_data) +
  labs(title = "Total Monthly Crimes in Chicago",
       y = "Number of Crimes")
ggtsdisplay(ts_data)
plot(decompose(ts_data)) # check seasonality

# split into train and test set before fitting models
train_data <- window(ts_data, end=c(2016,12))
test_data <- window(ts_data, start=c(2017))

# plot ACF and PACF over training data
summary(ur.kpss(diff(train_data, 12)))
summary(ur.kpss(diff(diff(train_data, 12)))) # unit root test to check for stationarity
                                             # test statistic much smaller than 1pct crit
                                             # value, hence do not reject null hyp of stationarity
#pdf("../figures/ts_dd_monthly.pdf")
ggtsdisplay(diff(diff(train_data, 12)), 
            main = "Doubly Differenced Monthly Crimes in Chicago")
#dev.off()

# based on the above we may consider the following models
# sarima(2, 1, 1)(1, 1, 1)[12]
# sarima(3, 1, 1)(1, 1, 1)[12]
# sarima(2, 1, 1)(1, 1, 2)[12]
# sarima(2, 1, 1)(0, 1, 2)[12]
fit1 <- Arima(train_data, order = c(2, 1, 1), seasonal = list(order=c(1, 1, 1)))
fit2 <- Arima(train_data, order = c(3, 1, 1), seasonal = list(order=c(1, 1, 1)))
fit3 <- Arima(train_data, order = c(2, 1, 1), seasonal = list(order=c(1, 1, 2)))
fit4 <- Arima(train_data, order = c(2, 1, 1), seasonal = list(order=c(0, 1, 2)))
fit5 <- auto.arima(train_data)

fit1_ffn <- function(x, h) {
  forecast(Arima(x, order = c(2, 1, 1), seasonal = list(order=c(1, 1, 1))), h=h)
}

fit2_ffn <- function(x, h) {
  forecast(Arima(x, order = c(3, 1, 1), seasonal = list(order=c(1, 1, 1))), h=h)
}

fit3_ffn <- function(x, h) {
  forecast(Arima(x, order = c(2, 1, 1), seasonal = list(order=c(1, 1, 2))), h=h)
}

fit4_ffn <- function(x, h) {
  forecast(Arima(x, order = c(2, 1, 1), seasonal = list(order=c(0, 1, 2))), h=h)
}

fit5_ffn <- function(x, h) {
  forecast(Arima(x, order = c(1, 1, 1), seasonal = list(order=c(1, 1, 2))), h=h)
}

errors <- function(x) {
  mae <- mean(abs(x), na.rm=TRUE)
  rmse <- sqrt(mean(x^2, na.rm=TRUE))
  return(list(mae=mae, rmse=rmse))
}

err_fit1 <- errors(tsCV(train_data, fit1_ffn))
err_fit2 <- errors(tsCV(train_data, fit2_ffn))
err_fit3 <- errors(tsCV(train_data, fit3_ffn))
err_fit4 <- errors(tsCV(train_data, fit4_ffn))
err_fit5 <- errors(tsCV(train_data, fit5_ffn))

# print results
round(cbind(bind_rows(err_fit1, err_fit2, err_fit3, err_fit4, err_fit5),
      AICc = rbind(fit1$aicc, fit2$aicc, fit3$aicc, fit4$aicc, fit5$aicc),
      BIC = rbind(fit1$bic, fit2$bic, fit3$bic, fit4$bic, fit5$bic)))

# choose bestfit based on AICc and CV errors
bestfit <- fit5

# check residuals of bestfit
checkresiduals(fit5, plot=TRUE)

# plot forecast by bestfit
autoplot(forecast(bestfit, h=33)) + 
  geom_point(test_data, mapping = aes(x = time(test_data), y = test_data),
             pch = 21, color="red") +
  labs(title = "Forecast of the Monthly Crimes in Chicago",
       subtitle = "Forecasts From a SARIMA(1,1,1)(1,1,2)[12] Model",
       y = "Number of Crimes")
ggsave("ts_monthly_forecast.pdf", device = "pdf", path="../figures",
       width = 5.64, height = 2.72)

# report final error on test set of 33 months
bestfit_fc <- forecast(bestfit, h=33)
accuracy(bestfit_fc, test_data)

# report error over 12 months
bestfit_fc <- forecast(bestfit, h=12)
accuracy(bestfit_fc, test_data[1:12])


# weekly data -------------------------------------------------------------
# add day column
data %<>%
  mutate(Week = week(ymd(Date)))

# count crimes per day
count_data <- data %>%
  group_by(Year, Week) %>%
  count() %>%
  filter(Week != 53)

# make a ts object
ts_data <- ts(count_data$n, start=c(2002,1), frequency = 365.25/7)

# remove first and last few obs
ts_data <- window(ts_data, start=c(2003, 1), end=c(2019,9))

count_data %>%
  filter(n<1000)

# exploratory plot
weekly_ts <- autoplot(ts_data) +
  labs(title = "Total Weekly Crimes in Chicago",
       y = "Number of Crimes")
ggtsdisplay(ts_data)
plot(decompose(ts_data)) # check seasonality

# split into train and test set before fitting models
train_data <- window(ts_data, end=c(2016,365.25/7))
test_data <- window(ts_data, start=c(2017))

# plot ACF and PACF over training data
ggtsdisplay(diff(train_data))
summary(ur.kpss(diff(train_data, 52)))
summary(ur.kpss(diff(diff(train_data, 52)))) # unit root test to check for stationarity
# test statistic much smaller than 1pct crit
# value, hence do not reject null hyp of stationarity
ggtsdisplay(diff(diff(train_data, 52)))

fit1 <- Arima(train_data, order = c(0, 1, 2), seasonal = list(order=c(1, 1, 2)))
fit2 <- Arima(train_data, order = c(0, 1, 3), seasonal = list(order=c(1, 1, 2)))
fit3 <- Arima(train_data, order = c(0, 1, 1), seasonal = list(order=c(1, 1, 2)))
fit4 <- Arima(train_data, order = c(1, 1, 2), seasonal = list(order=c(1, 1, 2)))
fit5 <- auto.arima(train_data)

# print results
bind_cols(AICc = rbind(fit1$aicc, fit2$aicc, fit3$aicc, fit4$aicc, fit5$aicc),
      BIC = rbind(fit1$bic, fit2$bic, fit3$bic, fit4$bic, fit5$bic))

# choose bestfit based on AICc and CV errors
bestfit <- fit1

# check residuals of bestfit
checkresiduals(bestfit, plot=TRUE)

fcsts <- forecast(bestfit, h=length(test_data))
# plot forecast by bestfit
autoplot(forecast(bestfit, h=length(test_data))) + 
  geom_point(test_data, mapping = aes(x = time(test_data), y = test_data),
             pch = 21, color="red") +
  geom_line(aes(x=time(test_data), y=fcsts$mean), data=fcsts$mean, col="blue") +
  labs(title = "Forecast of the Weekly Crimes in Chicago",
       subtitle = "Forecasts From a SARIMA(0,1,2)(1,1,2)[52] Model",
       y = "Number of Crimes")
ggsave("ts_weekly_forecast.pdf", device = "pdf", path="../figures",
       width = 5.64, height = 2.72)

# report final error on test set 
bestfit_fc <- forecast(bestfit, h=length(test_data))
accuracy(bestfit_fc, test_data)

# report error over 12 months
bestfit_fc <- forecast(bestfit, h=52)
accuracy(bestfit_fc, test_data[1:52])

# hierarchical time series -------------------------------------------------
library(hts) # hierarchical time series

# count crimes per month by ward
count_data <- data %>%
  group_by(Year, Month, Ward) %>%
  count()

# need to transform data s.t. every column is a diff ward
count_data <- count_data %>%
  ungroup() %>%
  mutate(id = row_number(),
         Ward = as.factor(Ward))

count_data_wide <- pivot_wider(count_data,
                          id_cols=c(Year, Month),
                          names_from = Ward,
                          values_from = n) 

# fit hierarchical time series with ward as bottom level
hts_data <- hts(ts(count_data_wide[,-c(1,2)],
           start = c(2002,01), frequency = 12), 
           nodes = list(50))

# remove first few obs
hts_data <- window(hts_data, start=c(2002, 6), end=c(2019,9))

# split into train and test set before fitting models
train_data <- window(hts_data, end=c(2016,12))
test_data <- window(hts_data, start=c(2017))

# set seed
set.seed(123)

# produce two forecasts using bottom up and optimal approach
hts_train_forecast_bu <- forecast.gts(train_data, fmethod = "arima", h = 33, method = "bu")
hts_train_forecast_opt <- forecast.gts(train_data, fmethod = "arima", h = 33, method = "comb")

# compute accuracy on test set
accuracy(hts_train_forecast_bu, test_data)
accuracy(hts_train_forecast_opt, test_data)

# plot forecasts
forecasts <- aggts(hts_train_forecast_opt, levels=0)
hist <- aggts(train_data, level=0)
top_test_data <- aggts(test_data, 0)
hts_fit <- ts(rbind(hist, forecasts), start=start(hist), frequency=12)
p1 <- autoplot(hist) + 
  geom_point(top_test_data, mapping = aes(x = time(top_test_data), y = top_test_data),
             pch = 21, color="red") +
  geom_line(aes(x = time(forecasts), y = forecasts), data = forecasts, col = "blue") +
  labs(title = "Forecasts for the Total Monthly Crimes in Chicago",
       y = "Number of Crimes")
p1

forecasts <- aggts(hts_train_forecast_opt, levels=1)
hist <- aggts(train_data, level=1)
top_test_data <- aggts(test_data, 1)
hts_fit <- ts(rbind(hist, forecasts), start=start(hist), frequency=12)

fc_data <- as_tibble(forecasts[,1:3]) %>%
  gather(Ward) %>%
  mutate(Date = rep(time(forecasts), 3),
         Ward = paste("Ward", Ward))

top_test_data <- as_tibble(top_test_data[,1:3]) %>%
  gather(Ward) %>%
  mutate(Date = rep(time(top_test_data), 3),
         Ward = paste("Ward", Ward))

p2 <- as_tibble(hist[,1:3]) %>%
  gather(Ward) %>%
  mutate(Date = rep(time(hist), 3),
         Ward = paste("Ward", Ward)) %>%
  ggplot(aes(x = Date, y = value,
             group = Ward)) +
  geom_line() +
  geom_point(aes(x = Date, y = value, group=Ward), top_test_data,
             pch = 21, color="red") +
  geom_line(aes(x = Date, y = value, group = Ward), 
            data = fc_data, col = "blue") +
  facet_grid(. ~ Ward, scales = "free") +
  labs(title = "Forecasts for the Monthly Crimes per Ward",
       y = "Number of Crimes")
p2

library(gridExtra)
#pdf("../figures/ts_hts_forecast.pdf")
#gridExtra::grid.arrange(p1, p2, ncol=1)
#dev.off()

#pdf("../figures/ts_weeknmonth.pdf",width=10,height =3)
grid.arrange(weekly_ts, monthly_ts, ncol=2)
#dev.off()
```
