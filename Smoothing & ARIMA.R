# CASE STUDY 2: Blue Mountain Cafe and Golden Crust Bakery
# Time Series Forecasting Analysis

library(fpp2)
library(urca)
library(forecast)

# Load data
setwd("/Users/tiara/Desktop")
data <- read.csv("TS_data_2025.csv")
head(data)

# Check column names
colnames(data)

# Create time series objects (monthly data)
# Use column index numbers to avoid encoding issues
blue <- ts(data[, 2], frequency = 12)  # Second column = Blue Mountain
golden <- ts(data[, 3], frequency = 12)  # Third column = Golden Crust

#
#
#
#
#
#
#
#########################################
# Qs F: SMOOTHING

##### !!!! BLUE MOUNTAIN CAFE !!!! #####

# Examine the data
autoplot(blue) +
  ggtitle("Blue Mountain Cafe - Monthly Customers") +
  ylab("Customers") +
  xlab("Time")

# Check decomposition to identify trend and seasonality
decomp_blue <- decompose(blue)
autoplot(decomp_blue) 
# trend: looks stable with no strong upward/downward, recent rise
# seasonality: looks strong & with consistent pattern

# Formal tests to confirm seasonality
# ACF
ggAcf(blue) +
  ggtitle("ACF – Blue Mountain Café") # -- moderate seasonal spikes

# Seasonal strength test
stl_blue <- stl(blue, s.window = "periodic")
seasonal_strength_blue <- 1 - var(stl_blue$time.series[, "remainder"]) /
  var(stl_blue$time.series[, "remainder"] + stl_blue$time.series[, "seasonal"])
cat("Blue Mountain seasonal strength:", seasonal_strength_blue, "\n") # -- 0.1157 (11.6%) = seasonality moderately present here

# Moderate seasonality (11.6%) observed - choose Holt-Winters to capture both trend and seasonality?

# Split data for model validation
# Hold out last 6 months (observations 236-241) as test set
blue_train <- subset(blue, end = 235)  # First 235 months for training
blue_test <- subset(blue, start = 236)  # Last 6 months for testing

# Compare Holt's vs Holt-Winters to validate seasonality inclusion
holt_blue_train <- holt(blue_train, h = 6)  # Holt's - trend only
hw_blue_train <- hw(blue_train, seasonal = "additive", h = 6)  # Holt-Winters - trend + seasonality

# Evaluate forecast accuracy on test set
acc_holt_blue <- accuracy(holt_blue_train, blue_test)
acc_hw_blue <- accuracy(hw_blue_train, blue_test)

# Compare both methods
cat("\n=== BLUE MOUNTAIN: HOLT'S VS HOLT-WINTERS ===\n")
cat("Holt's (trend only):\n")
print(acc_holt_blue)
cat("\nHolt-Winters (trend + seasonality):\n")
print(acc_hw_blue)

# Holt-Winters outperforms Holt's on RMSE (85.43 vs 91.78), MAE (60.15 vs 62.00), and MAPE (2.18% vs 2.21%)
# Confirms choice of Holt-Winters, capturing seasonality improves forecast accuracy
# MAPE = 2.18% indicates strong forecast performance (under 10% is considered good)

# Visualisation showing forecast vs actual test data
autoplot(subset(blue, start = 200)) +
  autolayer(hw_blue_train, series = "HW Forecast", PI = TRUE) +
  autolayer(blue_test, series = "Actual Test Data", PI = FALSE) +
  scale_color_manual(values = c("HW Forecast" = "blue", "Actual Test Data" = "yellow")) +
  ggtitle("Blue Mountain Café - Forecast vs Actual Test Data (Zoomed)") +
  ylab("Customers") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Data"))

# Model validated successfully, now refit on ALL data for final forecast
# Compare additive vs multiplicative seasonality
hw_blue_add <- hw(blue, seasonal = "additive", h = 6)
hw_blue_mult <- hw(blue, seasonal = "multiplicative", h = 6)

# Compare both methods
autoplot(subset(blue, start = 200)) +
  autolayer(hw_blue_add, series = "HW Additive", PI = FALSE) +
  autolayer(hw_blue_mult, series = "HW Multiplicative", PI = FALSE) +
  ggtitle("Blue Mountain Café - Additive vs Multiplicative HW Forecasts") +
  ylab("Customers") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Forecast"))

# Check which performs better on training data
summary(hw_blue_add)
summary(hw_blue_mult)
# Additive model selected due to lower AIC (3713.93 vs 3730.05), BIC, RMSE, and MAPE
# ^ Confirms constant seasonal amplitude observed in decomposition
hw_blue_final <- hw_blue_add

# Final chosen forecast
hw_blue_final <- hw_blue_add

# Print & plot final forecast values
print(hw_blue_final)

# Display next 6-month forecast
blue_forecast_df <- data.frame(Month = 242:247,
  Forecast = round(hw_blue_final$mean, 0),
  Lower_80 = round(hw_blue_final$lower[,1], 0),
  Upper_80 = round(hw_blue_final$upper[,1], 0),
  Lower_95 = round(hw_blue_final$lower[,2], 0),
  Upper_95 = round(hw_blue_final$upper[,2], 0)
)
print(blue_forecast_df)

#### END ####
#
#
#
#
#
#
##### !!!! GOLDEN CRUST BAKERY !!!! #####

# Examine the data
autoplot(golden) +
  ggtitle("Golden Crust Bakery - Monthly Customers") +
  ylab("Customers") +
  xlab("Time")

# Check decomposition to identify trend and seasonality
decomp_golden <- decompose(golden)
autoplot(decomp_golden) 

# Formal tests to confirm trend and seasonality
ggAcf(golden) +
  ggtitle("ACF – Golden Crust Bakery") # -- ACF shows strong trend-driven autocorrelation

# Seasonal strength test
stl_golden <- stl(golden, s.window = "periodic")
seasonal_strength_golden <- 1 - var(stl_golden$time.series[, "remainder"]) /
  var(stl_golden$time.series[, "remainder"] + stl_golden$time.series[, "seasonal"])
cat("Seasonal strength:", seasonal_strength_golden, "\n") # -- weak seasonality

# Split data for model validation
golden_train <- subset(golden, end = 235)
golden_test <- subset(golden, start = 236)

# Test BOTH Holt's (trend only) and Holt-Winters (trend + seasonality)
holt_golden_train <- holt(golden_train, h = 6)  # Holt's - trend only
hw_golden_train <- hw(golden_train, seasonal = "additive", h = 6)  # HW - trend + seasonality

# Compare accuracy
acc_holt_golden <- accuracy(holt_golden_train, golden_test)
acc_hw_golden <- accuracy(hw_golden_train, golden_test)

cat("\n=== HOLT'S METHOD ===\n")
print(acc_holt_golden)

cat("\n=== HOLT-WINTERS METHOD ===\n")
print(acc_hw_golden)

# Despite weak seasonality (1.14%), HW outperforms Holt's on all metrics
# MAPE = 0.28% indicates exceptional forecast accuracy
# Strong upward trend is highly predictable

# Visualisation showing forecast vs actual test data
autoplot(subset(golden, start = 200)) +
  autolayer(hw_golden_train, series = "HW Forecast", PI = TRUE) +
  autolayer(golden_test, series = "Actual Test Data", PI = FALSE) +
  scale_color_manual(values = c("HW Forecast" = "blue", "Actual Test Data" = "yellow")) +
  ggtitle("Golden Crust Bakery - Forecast vs Actual Test Data (Zoomed)") +
  ylab("Customers") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Data"))

# Model validated successfully, now refit on ALL data for final forecast
# Compare additive vs multiplicative seasonality
hw_golden_add <- hw(golden, seasonal = "additive", h = 6)
hw_golden_mult <- hw(golden, seasonal = "multiplicative", h = 6)

# Compare both methods
autoplot(subset(golden, start = 200)) +
  autolayer(hw_golden_add, series = "HW Additive", PI = FALSE) +
  autolayer(hw_golden_mult, series = "HW Multiplicative", PI = FALSE) +
  ggtitle("Golden Crust Bakery - Additive vs Multiplicative HW Forecasts") +
  ylab("Customers") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Forecast"))

# Check which performs better
summary(hw_golden_add)
summary(hw_golden_mult)
# Additive model selected: lower AIC (2104.88 vs 2279.14), BIC, RMSE, MAE, and MAPE
# Confirms constant (weak) seasonal amplitude despite strong trend
hw_golden_final <- hw_golden_add

# Display next 6-month forecast (rounded to whole customers)
golden_forecast_df <- data.frame(
  Month = 242:247,
  Forecast = round(hw_golden_final$mean, 0),
  Lower_80 = round(hw_golden_final$lower[,1], 0),
  Upper_80 = round(hw_golden_final$upper[,1], 0),
  Lower_95 = round(hw_golden_final$lower[,2], 0),
  Upper_95 = round(hw_golden_final$upper[,2], 0)
)
print(golden_forecast_df)

#### END ####
#
#
#
#
#
#
#
#
#
#
#########################################
##### Qs G: ARIMA

##### !!!! ARIMA - BLUE MOUNTAIN CAFE !!!! #####

# Plot the data first
autoplot(blue) +
  ggtitle("Blue Mountain Cafe - Monthly Customers") +
  ylab("Customers") +
  xlab("Time")

# Check for patterns using decomposition
autoplot(decompose(blue))

# Test for stationarity using unit root test
ur_test_blue <- ur.kpss(blue)
summary(ur_test_blue) # -- Test-statistic is larger than critical values = reject the null hypothesis 
# -- Data is NOT stationary

# Difference the data
blue_diff <- diff(blue)

# Test differenced data for stationary
ur_test_blue_diff <- ur.kpss(blue_diff)
summary(ur_test_blue_diff)

# Plot differenced data to visualize
autoplot(blue_diff) +
  ggtitle("Blue Mountain Café - Differenced Data") +
  ylab("Change in Customers") +
  xlab("Time")

# Confirm differencing order
ndiffs(blue) # Returns 1

# ACF and PACF plots on differenced data (CRUCIAL for choosing p and q!)
acf(diff(blue), main='ACF of Differenced Blue Mountain Data', lag.max = 50)
pacf(diff(blue), main='PACF of Differenced Blue Mountain Data', lag.max = 50)

# Fit various ARIMA models based on ACF/PACF analysis
# ACF cuts off after lag 1, PACF decays → suggests MA model
# Trying 4 different models to compare

# Model 1: ARIMA(0,1,1)
model1_b <- Arima(blue, order=c(0,1,1))
SSE1_b <- sum(model1_b$residuals^2)
model1_b.test <- Box.test(model1_b$residuals, lag = log(length(model1_b$residuals)))

# Model 2: ARIMA(0,1,2) 
model2_b <- Arima(blue, order=c(0,1,2))
SSE2_b <- sum(model2_b$residuals^2)
model2_b.test <- Box.test(model2_b$residuals, lag = log(length(model2_b$residuals)))

# Model 3: ARIMA(1,1,1) 
model3_b <- Arima(blue, order=c(1,1,1))
SSE3_b <- sum(model3_b$residuals^2)
model3_b.test <- Box.test(model3_b$residuals, lag = log(length(model3_b$residuals)))

# Model 4: ARIMA(1,1,2) 
model4_b <- Arima(blue, order=c(1,1,2))
SSE4_b <- sum(model4_b$residuals^2)
model4_b.test <- Box.test(model4_b$residuals, lag = log(length(model4_b$residuals)))

# Compare model performance in a clear table format
df_b <- data.frame(
  row.names = c('AIC', 'BIC', 'SSE', 'Box-test p-value'), 
  c(model1_b$aic, model1_b$bic, SSE1_b, model1_b.test$p.value), 
  c(model2_b$aic, model2_b$bic, SSE2_b, model2_b.test$p.value), 
  c(model3_b$aic, model3_b$bic, SSE3_b, model3_b.test$p.value),
  c(model4_b$aic, model4_b$bic, SSE4_b, model4_b.test$p.value)
)
colnames(df_b) <- c('ARIMA(0,1,1)', 'ARIMA(0,1,2)', 'ARIMA(1,1,1)', 'ARIMA(1,1,2)') 
# p-value from Ljung-Box test: >0.05 means residuals are independent (white noise) which we want
format(df_b, scientific = FALSE)

# Based on the comparison, check residuals of the chosen best model
checkresiduals(model1_b)

# Compare with auto.arima()
fit_auto_blue <- auto.arima(blue, seasonal = FALSE)
summary(fit_auto_blue)
# The manually selected best model matches the auto.arima choice - ARIMA(0,1,1) !! :)

#### END ####
#
#
#
#
#
##
##### !!!! ARIMA - GOLDEN CRUST BAKERY !!!! #####

# Plot the data first
autoplot(golden) +
  ggtitle("Golden Crust Bakery - Monthly Customers") +
  ylab("Customers") +
  xlab("Time")

# Check for patterns using decomposition
autoplot(decompose(golden))

# Test for stationarity using unit root test
ur_test_golden <- ur.kpss(golden)
summary(ur_test_golden) # Test-statistic larger than critical values
# -- Data here also NOT stationary

# Difference the data
golden_diff <- diff(golden)

# Test stationarity of differenced data
ur_test_golden_diff <- ur.kpss(golden_diff)
summary(ur_test_golden_diff)

# Plot the differenced data to visualize
autoplot(golden_diff) +
  ggtitle("Golden Crust Bakery - Differenced Data") +
  ylab("Change in Customers") +
  xlab("Time")

# Confirm differencing order
ndiffs(golden)

# ACF and PACF plots on differenced data
acf(diff(golden), main='ACF of Differenced Golden Crust Data', lag.max = 50)
pacf(diff(golden), main='PACF of Differenced Golden Crust Data', lag.max = 50)

# Fit various ARIMA models based on ACF/PACF analysisx
# Model 1: ARIMA(0,1,1)
model1_g <- Arima(golden, order=c(0,1,1))
SSE1_g <- sum(model1_g$residuals^2)
model1_g.test <- Box.test(model1_g$residuals, lag = log(length(model1_g$residuals)))

# Model 2: ARIMA(0,1,2) 
model2_g <- Arima(golden, order=c(0,1,2))
SSE2_g <- sum(model2_g$residuals^2)
model2_g.test <- Box.test(model2_g$residuals, lag = log(length(model2_g$residuals)))

# Model 3: ARIMA(1,1,1) 
model3_g <- Arima(golden, order=c(1,1,1))
SSE3_g <- sum(model3_g$residuals^2)
model3_g.test <- Box.test(model3_g$residuals, lag = log(length(model3_g$residuals)))

# Model 4: ARIMA(1,1,0) 
model4_g <- Arima(golden, order=c(1,1,0))
SSE4_g <- sum(model4_g$residuals^2)
model4_g.test <- Box.test(model4_g$residuals, lag = log(length(model4_g$residuals)))

# Compare model performance in a clear table format
df_g <- data.frame(
  row.names = c('AIC', 'BIC', 'SSE', 'Box-test p-value'), 
  c(model1_g$aic, model1_g$bic, SSE1_g, model1_g.test$p.value), 
  c(model2_g$aic, model2_g$bic, SSE2_g, model2_g.test$p.value), 
  c(model3_g$aic, model3_g$bic, SSE3_g, model3_g.test$p.value),
  c(model4_g$aic, model4_g$bic, SSE4_g, model4_g.test$p.value)
)
colnames(df_g) <- c('ARIMA(0,1,1)', 'ARIMA(0,1,2)', 'ARIMA(1,1,1)', 'ARIMA(1,1,0)') 
format(df_g, scientific = FALSE)

# Based on the comparison, check residuals of the chosen best model
checkresiduals(model2_g) # All p-values are < 0.05 --> NOT white noise, autocorrelation exists
# -- NONE of these models are adequate for Golden Crust data :(

# Compare with auto.arima()
fit_auto_golden <- auto.arima(golden, seasonal = FALSE)
summary(fit_auto_golden)

# Refit models WITH drift for Golden Crust
# Model 1: ARIMA(0,1,1) with drift
model1_g <- Arima(golden, order=c(0,1,1), include.drift = TRUE)
SSE1_g <- sum(model1_g$residuals^2)
model1_g.test <- Box.test(model1_g$residuals, lag = log(length(model1_g$residuals)))

# Model 2: ARIMA(0,1,2) with drift
model2_g <- Arima(golden, order=c(0,1,2), include.drift = TRUE)
SSE2_g <- sum(model2_g$residuals^2)
model2_g.test <- Box.test(model2_g$residuals, lag = log(length(model2_g$residuals)))

# Model 3: ARIMA(1,1,1) with drift
model3_g <- Arima(golden, order=c(1,1,1), include.drift = TRUE)
SSE3_g <- sum(model3_g$residuals^2)
model3_g.test <- Box.test(model3_g$residuals, lag = log(length(model3_g$residuals)))

# Model 4: ARIMA(1,1,0) with drift
model4_g <- Arima(golden, order=c(1,1,0), include.drift = TRUE)
SSE4_g <- sum(model4_g$residuals^2)
model4_g.test <- Box.test(model4_g$residuals, lag = log(length(model4_g$residuals)))

# Compare again
df_g <- data.frame(
  row.names = c('AIC', 'BIC', 'SSE', 'Box-test p-value'), 
  c(model1_g$aic, model1_g$bic, SSE1_g, model1_g.test$p.value), 
  c(model2_g$aic, model2_g$bic, SSE2_g, model2_g.test$p.value), 
  c(model3_g$aic, model3_g$bic, SSE3_g, model3_g.test$p.value),
  c(model4_g$aic, model4_g$bic, SSE4_g, model4_g.test$p.value)
)
colnames(df_g) <- c('ARIMA(0,1,1)', 'ARIMA(0,1,2)', 'ARIMA(1,1,1)', 'ARIMA(1,1,0)') 
format(df_g, scientific = FALSE)
checkresiduals(model1_g)

# Forecast next 6 months using ARIMA(0,1,1) with drift
forecast_arima_golden <- forecast(model1_g, h = 6)

# Display forecast with rounded values
golden_arima_forecast_df <- data.frame(
  Month = 242:247,
  Forecast = round(forecast_arima_golden$mean, 0),
  Lower_80 = round(forecast_arima_golden$lower[,1], 0),
  Upper_80 = round(forecast_arima_golden$upper[,1], 0),
  Lower_95 = round(forecast_arima_golden$lower[,2], 0),
  Upper_95 = round(forecast_arima_golden$upper[,2], 0)
)

# Compare ARIMA vs Holt-Winters forecasts for Golden Crust
cat("\n=== COMPARISON: ARIMA vs HOLT-WINTERS FORECASTS ===\n")
cat("\nARIMA(0,1,1) with drift Forecast:\n")
print(golden_arima_forecast_df)

cat("\nHolt-Winters Forecast:\n")
print(golden_forecast_df)

# ARIMA with drift: Linear growth (1039 → 1051)
# Holt-Winters: Nearly identical (1039 → 1050)
# Both methods converge due to dominant trend and weak seasonality (1.14%)

##### END ####
