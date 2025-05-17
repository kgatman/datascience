####
# Installing the packages
packages <- c("readxl", "ggplot2", "forecast", "tseries", "rugarch", "moments")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
})
lapply(packages, library, character.only = TRUE)

#### 
# loading the Nathan Company Gas Bills dataframe

library(readxl)
gas_bills <- read_excel("~/Downloads/Quarterly gas bills for the Nathan Company.xlsx", 
                        col_types = c("numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))
#Question 1(a)
gas_ts <- ts(as.vector(t(gas_bills[, -1])), start = c(1, 1), frequency = 4)

# Plot
# also labeling the x- and y-axis
# and adding the title

autoplot(gas_ts) + 
  ggtitle("Quarterly Gas Bills for Nathan Company") +
  xlab("Year") + ylab("Gas Bill Amount")

#Question 1(b)
model <- Arima(gas_ts, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 4))
summary(model)

# Residual check
checkresiduals(model)

#print model
print(model)

##############################################################

library(readxl)
crypto <- read_excel("~/Downloads/Cryptocurrency_2022.xlsx")

#Question 2(a)
crypto$date <- as.Date(crypto$date)
ggplot(crypto, aes(x = date, y = close)) + 
  geom_line(color = "blue") +
  ggtitle("Daily Cryptocurrency Closing Prices") +
  xlab("Date") + ylab("Price (USD)")

#Question 2(b)
log_returns <- diff(log(crypto$close))
ts.plot(log_returns)

# Skewness & Excess Kurtosis
sk <- skewness(log_returns)
kurt <- kurtosis(log_returns) - 3
#print the Skewness
cat("Skewness:", sk, "\nExcess Kurtosis:", kurt, "\n")

#Question 2(c)
log_return_dates <- crypto$date[-1]
log_returns_df <- data.frame(date = log_return_dates, returns = log_returns)

ggplot(log_returns_df, aes(x = date, y = returns)) +
  geom_line() +
  ggtitle("Daily Log Returns of Cryptocurrency") +
  xlab("Date") + ylab("Log Return")


#Question 2(d)
t_test <- t.test(log_returns)
t_test

#Question 2(e)
par(mfrow=c(1,2))
acf(log_returns, main = "ACF of Log Returns")
acf(log_returns^2, main = "ACF of Squared Returns")

#Question 2(f)
install.packages("FinTS")
library(FinTS)
arch_test <- ArchTest(log_returns, lags = 10)
arch_test


#Question 2(f)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
  mean.model = list(armaOrder = c(1,0)),
  distribution.model = "sstd"
)

fit <- ugarchfit(spec, log_returns)
show(fit)
