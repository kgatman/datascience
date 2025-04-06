# checking out the dataset
summary(co2)
# packages used are
# fbasics, tseries, TSA, Stats, Stats4 and Forecast

help(ts.plot)
require(graphics)
ts_c02 <- ts(co2, start = c(1, 1), frequency = 12)
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

# The ACF plot indicates a monthly seasonality
# of s=12 the ACP value as 1,12,... decay 
# exponentially. 

# (c)
diff_c02 = diff(ts_c02)
ts.plot(diff_c02)
acfPlot(diff_c02)

# (d)
help("arima")
