
########################## Question 1 ################

summary(Viscosity)
myData = Viscosity$Yt
ts.plot(myData)
adf.test(myData)
acfPlot(myData)
myData_arma2 = arma(myData, order = c(2,0))
summary(myData_arma2)
forecast(myData_arma2,h=5)
myData_arima2 = Arima(myData,
                      order = c(2,0,0),
                      include.mean = TRUE)
forecast(myData_arima2,h=5)
plot(forecast(myData_arima2,h=5))

########################## Question 2 ################

towels = Paper_towels$Yt
ts.plot(towels)
adf.test(towels)
towels_diff = diff(towels)
ts.plot(towels_diff)
adf.test(towels_diff)
acfPlot(towels_diff)
pacfPlot(towels_diff)

towels_ari1_1 = arma(towels_diff,order = c(1,0))
summary(towels_ari1_1)

towels_ima1_1 = arma(towels_diff,order = c(0,1))
summary(towels_ima1_1)


towels_arima2 = Arima(towels,
                      order = c(1,1,0),
                      include.mean = TRUE)
forecast(towels_arima2,h=10)
plot(forecast(towels_arima2,h=10))


