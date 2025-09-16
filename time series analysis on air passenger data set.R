data <- read.csv("C:\\Users\\sayak\\Downloads\\AirPassengers.csv")
View(data)
colnames(data)

# Create time series
ts_data <- ts(data$X.Passengers, frequency = 12, start = c(1949, 1))

library(ggplot2)
library(forecast)
library(tseries)

# Basic time series plot
ggplot(data, aes(x = as.Date(paste0(Month, "-01")), y = X.Passengers)) +  
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Monthly Air Passengers (1949-1960)", x = "Year", y = "Passengers") +
  theme_minimal()

# STL decomposition
decomp <- stl(ts_data, s.window = "periodic")
autoplot(decomp) + theme_minimal()

# --- Apply Log Transformation ---
log_ts <- log(ts_data)

# Stationarity check
adf.test(log_ts)  

# Differencing on log data
log_ts_diff <- diff(log_ts)
adf.test(log_ts_diff)  # Should be stationary now

# Auto ARIMA on log-transformed data
model <- auto.arima(log_ts)
summary(model)

# Forecast on log scale
forecast_log <- forecast(model, h = 24)

# Convert back to original scale
forecast_final <- exp(forecast_log$mean)

# Plot forecast (log scale)
autoplot(forecast_log) +
  labs(title = "Log-ARIMA Forecast for Air Passengers", x = "Year", y = "log(Passengers)") +
  theme_minimal()

# Plot back-transformed forecast
ts.plot(ts_data, forecast_final, log = "y", lty = c(1, 3), col = c("black", "red"))
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = c(1, 3))
