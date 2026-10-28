

library(forecast)
library(tseries)



amd <- read.csv("D:/Download/AMD.csv")



amd$Date <- as.Date(amd$Date, format = "%d/%m/%Y")

amd <- amd[order(amd$Date), ]

cat("\n===== DATA CHECK =====\n")

print(range(amd$Date))
print(dim(amd))

cat("Missing Close =", sum(is.na(amd$Close)), "\n")
cat("Duplicate Date =", sum(duplicated(amd$Date)), "\n")


# เลือกเฉพาะ Date และ Close
amd_close <- amd[, c("Date", "Close")]



plot(
  amd_close$Date,
  amd_close$Close,
  type = "l",
  main = "AMD Closing Price",
  xlab = "Date",
  ylab = "Closing Price (USD)"
)

# frequency = 1 หมายถึงไม่กำหนด Seasonal pattern

z <- ts(
  amd_close$Close,
  frequency = 1
)



adf_original <- adf.test(z)

cat("\n===== ADF ORIGINAL =====\n")
print(adf_original)

log_z <- log(z)

plot(
  log_z,
  type = "l",
  main = "Log-transformed AMD Closing Price",
  xlab = "Trading Day",
  ylab = "Log(Closing Price)"
)

adf_log <- adf.test(log_z)

cat("\n===== ADF AFTER LOG =====\n")
print(adf_log)



diff_log_z <- diff(log_z)

plot(
  diff_log_z,
  type = "l",
  main = "First Difference of Log AMD Closing Price",
  xlab = "Trading Day",
  ylab = "Difference of Log Price"
)

adf_diff <- adf.test(diff_log_z)

cat("\n===== ADF AFTER FIRST DIFFERENCE =====\n")
print(adf_diff)


acf(
  diff_log_z,
  main = "ACF of First Differenced Log AMD Price"
)

pacf(
  diff_log_z,
  main = "PACF of First Differenced Log AMD Price"
)


fit1 <- Arima(log_z, order = c(0, 1, 0))
fit2 <- Arima(log_z, order = c(1, 1, 0))
fit3 <- Arima(log_z, order = c(0, 1, 1))
fit4 <- Arima(log_z, order = c(1, 1, 1))


acc1 <- accuracy(fit1)
acc2 <- accuracy(fit2)
acc3 <- accuracy(fit3)
acc4 <- accuracy(fit4)


comparison <- data.frame(
  
  Model = c(
    "ARIMA(0,1,0)",
    "ARIMA(1,1,0)",
    "ARIMA(0,1,1)",
    "ARIMA(1,1,1)"
  ),
  
  AIC = c(
    AIC(fit1),
    AIC(fit2),
    AIC(fit3),
    AIC(fit4)
  ),
  
  BIC = c(
    BIC(fit1),
    BIC(fit2),
    BIC(fit3),
    BIC(fit4)
  ),
  
  RMSE = c(
    acc1[, "RMSE"],
    acc2[, "RMSE"],
    acc3[, "RMSE"],
    acc4[, "RMSE"]
  ),
  
  MAPE = c(
    acc1[, "MAPE"],
    acc2[, "MAPE"],
    acc3[, "MAPE"],
    acc4[, "MAPE"]
  )
)

cat("\n===== BASIC MODEL COMPARISON =====\n")
print(comparison)



auto_fit <- auto.arima(
  log_z,
  d = 1,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  max.p = 5,
  max.q = 5
)

cat("\n===== AUTO ARIMA RESULT =====\n")

print(summary(auto_fit))

cat("\nARIMA Order:\n")
print(arimaorder(auto_fit))


# Final Model
best_model <- auto_fit


res <- residuals(best_model)


plot(
  res,
  type = "l",
  main = "Residuals of Final ARIMA Model",
  xlab = "Trading Day",
  ylab = "Residual"
)


acf(
  res,
  main = "ACF of Residuals"
)


ord <- arimaorder(best_model)

lb10 <- Box.test(
  res,
  lag = 10,
  type = "Ljung-Box",
  fitdf = ord["p"] + ord["q"]
)

cat("\n===== LJUNG-BOX : LAG 10 =====\n")
print(lb10)


lb20 <- Box.test(
  res,
  lag = 20,
  type = "Ljung-Box",
  fitdf = ord["p"] + ord["q"]
)

cat("\n===== LJUNG-BOX : LAG 20 =====\n")
print(lb20)

checkresiduals(best_model)

fc <- forecast(
  best_model,
  h = 10,
  level = c(80, 95)
)

cat("\n===== FORECAST IN LOG SCALE =====\n")
print(fc)


plot(
  fc,
  main = "Forecast of Log AMD Closing Price",
  xlab = "Future Trading Day",
  ylab = "Log(Closing Price)"
)


forecast_table <- data.frame(
  
  Trading_Day = 1:10,
  
  Forecast_USD =
    round(exp(fc$mean), 2),
  
  Lower_80 =
    round(exp(fc$lower[, 1]), 2),
  
  Upper_80 =
    round(exp(fc$upper[, 1]), 2),
  
  Lower_95 =
    round(exp(fc$lower[, 2]), 2),
  
  Upper_95 =
    round(exp(fc$upper[, 2]), 2)
)

cat("\n===== FORECAST TABLE (USD) =====\n")
print(forecast_table)


cat("\n")
cat("============================================================\n")
cat("                 FINAL RESULTS SUMMARY\n")
cat("============================================================\n")

cat("\nData period:\n")
print(range(amd$Date))

cat("\nNumber of observations =", nrow(amd), "\n")

cat("\nADF p-values\n")
cat("Original       =", adf_original$p.value, "\n")
cat("Log            =", adf_log$p.value, "\n")
cat("First Diff Log =", adf_diff$p.value, "\n")

cat("\nSelected ARIMA Model:\n")
print(arimaorder(best_model))

cat("\nAIC =", AIC(best_model), "\n")
cat("BIC =", BIC(best_model), "\n")

cat("\nLjung-Box p-value (Lag 10) =", lb10$p.value, "\n")
cat("Ljung-Box p-value (Lag 20) =", lb20$p.value, "\n")

cat("\n10-Trading-Day Forecast:\n")
print(forecast_table)

cat("\n============================================================\n")

par(mfrow = c(1, 2))

# ACF
acf_result <- acf(
  diff_log_z,
  lag.max = 40,
  plot = FALSE
)

acf_values <- acf_result$acf[-1]
acf_lags <- 1:40
ci <- 1.96 / sqrt(length(diff_log_z))

plot(
  acf_lags,
  acf_values,
  type = "h",
  lwd = 2,
  main = "ACF",
  xlab = "Lag",
  ylab = "ACF",
  ylim = c(
    min(acf_values, -ci) * 1.2,
    max(acf_values, ci) * 1.2
  )
)

abline(h = 0)
abline(h = c(-ci, ci),
       lty = 2)

# PACF
pacf(
  diff_log_z,
  lag.max = 40,
  main = "PACF"
)

par(mfrow = c(1, 1))
