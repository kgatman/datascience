# checking out the dataset
summary(co2)
# packages used are
# fbasics, tseries, TSA, Stats, Stats4 and Forecast

help(ts.plot)
require(graphics)
ts_c02 <- ts(co2, start = c(1994, 1), frequency = 12)
ts.plot(ts_c02)
# (a)
ts.plot(ts_c02, 
        col = "red", 
        lty = 1, 
        xlab = "Month", 
        ylab = "Value", 
        main = "12-Month Time Series")

# trying to plot this in 12 months but it
# basically looks like the default one

# (b)
acfPlot(ts_c02)
help(acfPlot)
# The ACF plot indicates a monthly seasonality
# of s=12 the ACP value as 1,12,... decay 
# exponentially. 

# (c)
diff_c02 = diff(ts_c02)
ts.plot(diff_c02)
acfPlot(diff_c02)

# (d)

seasonal_c02 <- diff(ts_c02, lag = 12)
ts.plot(seasonal_c02)
acfPlot(seasonal_c02)

# (e)
library(forecast)

sarima_co2_model <- Arima(ts_c02, order = c(0,1,1), seasonal = c(0,1,1))

summary(sarima_co2_model)

# (f)
acf(residuals(sarima_co2_model), main = "ACF of Residuals - ARIMA(0,1,1)(0,1,1)[12]")
pacf(residuals(sarima_co2_model), main = "ACF of Residuals - ARIMA(0,1,1)(0,1,1)[12]")
checkresiduals(sarima_co2_model)
qqnorm(residuals(sarima_co2_model), main = "Q-Q Plot of Residuals");qqline(residuals(sarima_co2_model), col = "red")
