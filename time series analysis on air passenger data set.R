data<-read.csv("C:\\Users\\sayak\\Downloads\\AirPassengers.csv")
View(data)
colnames(data)
ts_data <- ts(data$X.Passengers, frequency = 12, start = c(1949, 1))

library(ggplot2)
library(forecast)

#Basic time series plot..................
ggplot(data, aes(x = as.Date(paste0(Month, "-01")), y =X.Passengers)) +  
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Monthly Air Passengers (1949-1960)", x = "Year", y = "Passengers") +
  theme_minimal()

#seasonal component...............
decomp <- stl(ts_data, s.window = "periodic")
autoplot(decomp) + theme_minimal()  

#3..............
#arima Models.................
library(tseries)
adf.test(ts_data)  
ts_diff <- diff(ts_data)  

#Auto ARIMA model..................
model <- auto.arima(ts_data)
summary(model)  

#forecast and plot result............
forecast_data <- forecast(model, h = 24)  # Forecast 2 years
autoplot(forecast_data) + 
  labs(title = "ARIMA Forecast for Air Passengers", x = "Year", y = "Passengers") +
  theme_minimal()
