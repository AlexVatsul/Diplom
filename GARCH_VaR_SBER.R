setwd("E:/МЕХМАТ обучение/4 семестр/очка/НИР/Diplom")

# # Control + Shift + c - комментировать
# install.packages("ggthemes")
# install.packages("gridExtra")
# install.packages("fGarch")


# Загружаем необходимые библиотеки
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
library(xts)
library(zoo)
library(FinTS)
library(fGarch)
library(tseries)

data <- read.csv("portfolio_2015_2025.csv")
colSums(is.na(data))  # количество NA в каждом столбце
head(data)

start_date = "2018-01-01"
end_date = "2025-02-24"


# Преобразуем строки в даты
data$X <- as.Date(data$X, format = "%Y-%m-%d")

data_xts <- xts(data$ret_GAZP, order.by = data$X)

subset_xts <- window(data_xts, start = as.Date(start_date),
                     end = as.Date(end_date))


data_xts_SBER <- xts(data$NLMK, order.by = data$X)

subset_xts_SBER <- window(data_xts_SBER, start = as.Date(start_date),
                     end = as.Date(end_date))



index_data <- index(subset_xts)


rets = subset_xts

ret = subset_xts_SBER

# Преобразование xts в data.frame
df <- data.frame(date = index(rets), value = coredata(rets)[,1])

ggplot(df, aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  labs(title = "Log SBER",
       x = "Дата",
       y = "Цена") +
  theme_minimal()


tsdisplay(rets)



# Стационарность
adf.test(rets)


# Оценка среднего через ARMA
model.arima = auto.arima(rets , max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')
model.arima


model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)

ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')

tsdisplay(ar.res, main = 'Residuals')
tsdisplay(ar.res^2 , main = 'Squared Residuals')


################################################################################
# GARCH моделирование

#################################### GARCH(1,1)

# GARCH(1,1) с нормальным распределением
spec_garch_norm <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,0)),
                              distribution.model = "norm")

# GARCH(1,1) с t-распределением
spec_garch_std <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,0)),
                             distribution.model = "std")


#################################### EGARCH(1,1)

# EGARCH(1,1) с нормальным распределением
spec_egarch_norm <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                               mean.model = list(armaOrder = c(0,0)),
                               distribution.model = "norm")

# EGARCH(1,1) с t-распределением
spec_egarch_std <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,0)),
                              distribution.model = "std")


#################################### GJR-GARCH(1,1)

# GJR-GARCH(1,1) с нормальным распределением
spec_gjr_norm <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(0,0)),
                            distribution.model = "norm")

# GJR-GARCH(1,1) с t-распределением
spec_gjr_std <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(0,0)),
                           distribution.model = "std")


#################################### GARCH-M(GARCH-in-Mean)

# GARCH-M(1,1) с нормальным распределением
spec_garchm_norm <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                               mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = TRUE),
                               distribution.model = "norm")

# GARCH-M(1,1) с t-распределением
spec_garchm_std <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = TRUE),
                              distribution.model = "std")



####################### Обучение моделей разных GARCH###########################

#################################### GARCH(1,1)

# GARCH(1,1) с нормальным распределением
fit_garch_norm <- ugarchfit(spec = spec_garch_norm, data = ar.res)
fit_garch_norm

# GARCH(1,1) с t-распределением
fit_garch_std <- ugarchfit(spec = spec_garch_std, data = ar.res)
fit_garch_std

#################################### EGARCH(1,1)

# EGARCH(1,1) c нормальным распределением
fit_egarch_norm <- ugarchfit(spec = spec_egarch_norm, data = ar.res)
fit_egarch_norm

# EGARCH(1,1) с t-распределением
fit_egarch_std <- ugarchfit(spec = spec_egarch_std, data = ar.res)
fit_egarch_std


#################################### GJR-GARCH(1,1)

# GJR-GARCH(1,1) c нормальным распределением
fit_gjr_norm <- ugarchfit(spec = spec_gjr_norm, data = ar.res)
fit_gjr_norm

# GJR-GARCH(1,1) с t-распределением
fit_gjr_std <- ugarchfit(spec = spec_gjr_std, data = ar.res)
fit_gjr_std


#################################### GARCH-M(GARCH-in-Mean)

# GARCH-M(1,1) с нормальным распределением
fit_garchm_norm <- ugarchfit(spec = spec_garchm_norm, data = ar.res)
fit_garchm_norm


# GARCH-M(1,1) с t-распределением
fit_garchm_std <- ugarchfit(spec = spec_garchm_std, data = ar.res)
fit_garchm_std


#############################Получаем sigma#####################################

#################################### GARCH(1,1)

# GARCH(1,1) с нормальным распределением
sigma_garch_norm <- sigma(fit_garch_norm)


# GARCH(1,1) с t-распределением
sigma_garch_std <- sigma(fit_garch_std)


#################################### EGARCH(1,1)

# EGARCH(1,1) с нормальным распределением
sigma_egarch_norm <- sigma(fit_egarch_norm)


# EGARCH(1,1) с t-распределением
sigma_egarch_std <- sigma(fit_egarch_std)


#################################### GJR-GARCH(1,1)

# GJR-GARCH(1,1) с нормальным распределением
sigma_gjr_norm <- sigma(fit_gjr_norm)


# GJR-GARCH(1,1) с t-распределением
sigma_gjr_std <- sigma(fit_gjr_std)


#################################### GARCH-M(GARCH-in-Mean)

# GARCH-M(1,1) с нормальным распределением
sigma_gjr_garchm_norm <- sigma(fit_garchm_norm)

# GARCH-M(1,1) с t-распределением
sigma_garchm_std <- sigma(fit_garchm_std)

# Абсолютное значение доходности
abs_rets <- abs(rets)


######### Построение sigma из GARCH с нормальным распределением ################

data1 <- data.frame(
  time=index_data,
  value = abs_rets,
  series = "r_t"
)

data2 <- data.frame(
  time = index_data,
  value = sigma_garch_norm,
  series = "GARCH(1,1)"
)

data3 <- data.frame(
  time = index_data,
  value = sigma_egarch_norm,
  series = "EGARCH(1,1)"
)

data4 <- data.frame(
  time = index_data,
  value = sigma_gjr_norm,
  series = "GJR-GARCH(1,1)"
)

data5 <- data.frame(
  time = index_data,
  value = sigma_garchm_std,
  series = "GARCH-M(1,1)"
)


# Объединяем два набора данных
data_combined <- rbind(data1, data2, data3, data4, data5)

ggplot(data_combined, aes(x = time, y = value)) +
  # Тонкая и полупрозрачная серая линия для r_t
  geom_line(data = subset(data_combined, series == "r_t"),
            color = "grey60", size = 0.4, alpha = 0.6) +
  
  # Цветные модели — толще и ярче
  geom_line(data = subset(data_combined, series != "r_t"),
            aes(color = series), size = 1.1) +
  
  scale_color_manual(values = c(
    "GARCH(1,1)" = "#D62728",       # красный
    "EGARCH(1,1)" = "#1F77B4",      # синий
    "GJR-GARCH(1,1)" = "#2CA02C",   # зеленый
    "GARCH-M(1,1)" = "#FF7F0E"      # оранжевый
  )) +
  
  labs(title = "SBER: Сравнение моделей волатильности (норм. распр)",
       x = "Дата", y = "σ", color = "Модель") +
  theme_minimal() +
  theme(legend.position = "bottom")



######### Построение sigma из GARCH с t-распределением ################

data1 <- data.frame(
  time=index_data,
  value = abs_rets,
  series = "r_t"
)

data2 <- data.frame(
  time = index_data,
  value = sigma_garch_std,
  series = "GARCH(1,1)"
)

data3 <- data.frame(
  time = index_data,
  value = sigma_egarch_std,
  series = "EGARCH(1,1)"
)

data4 <- data.frame(
  time = index_data,
  value = sigma_gjr_std,
  series = "GJR-GARCH(1,1)"
)

data5 <- data.frame(
  time = index_data,
  value = sigma_garchm_std,
  series = "GARCH-M(1,1)"
)


# Объединяем два набора данных
data_combined <- rbind(data1, data2, data3, data4, data5)

ggplot(data_combined, aes(x = time, y = value)) +
  # Тонкая и полупрозрачная серая линия для r_t
  geom_line(data = subset(data_combined, series == "r_t"),
            color = "grey60", size = 0.4, alpha = 0.6) +
  
  # Цветные модели — толще и ярче
  geom_line(data = subset(data_combined, series != "r_t"),
            aes(color = series), size = 1.1) +
  
  scale_color_manual(values = c(
    "GARCH(1,1)" = "#D62728",       # красный
    "EGARCH(1,1)" = "#1F77B4",      # синий
    "GJR-GARCH(1,1)" = "#2CA02C",   # зеленый
    "GARCH-M(1,1)" = "#FF7F0E"      # оранжевый
  )) +
  
  labs(title = "SBER: Сравнение моделей волатильности (t. распр)",
       x = "Дата", y = "σ", color = "Модель") +
  theme_minimal() +
  theme(legend.position = "bottom")



########################## Реализованная волатильность #########################

window <- 20
scale_factor <- sqrt(252)*100

# Расчет реализованной волатильности (на основе лог-доходностей)
rv_20 <- sqrt(rollapply(rets^2, width = window, FUN = mean, align = "right", fill = NA)) * sqrt(252) * 100
head(rv_20, 20)


data1 <- data.frame(
  time=index_data[window:length(index_data)],
  value = rv_20[window:length(index_data)],
  series = "Реализованная волатильность"
)

data2 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_garch_norm[window:length(index_data)]*scale_factor,
  series = "GARCH(1,1)"
)

data3 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_egarch_norm[window:length(index_data)]*scale_factor,
  series = "EGARCH(1,1)"
)

data4 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_gjr_norm[window:length(index_data)]*scale_factor,
  series = "GJR-GARCH(1,1)"
)

data5 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_gjr_garchm_norm[window:length(index_data)]*scale_factor,
  series = "GARCH-M(1,1)"
)


# Объединяем два набора данных
data_combined <- rbind(data1, data2, data3, data4, data5)

ggplot(data_combined, aes(x = time, y = value)) +
  # Тонкая и полупрозрачная серая линия для r_t
  geom_line(data = subset(data_combined, series == "r_t"),
            color = "grey60", size = 0.4, alpha = 0.6) +
  
  # Цветные модели — толще и ярче
  geom_line(data = subset(data_combined, series != "r_t"),
            aes(color = series), size = 1.1) +
  
  scale_color_manual(values = c(
    "GARCH(1,1)" = "#D62728",       # красный
    "EGARCH(1,1)" = "#1F77B4",      # синий
    "GJR-GARCH(1,1)" = "#2CA02C",   # зеленый
    "GARCH-M(1,1)" = "#FF7F0E"      # оранжевый
  )) +
  
  labs(title = "SBER: Реализованная волатильность и модели GARCH (норм. распр)",
       x = "Дата", y = "σ", color = "Модель") +
  theme_minimal() +
  theme(legend.position = "bottom")



data1 <- data.frame(
  time=index_data[window:length(index_data)],
  value = rv_20[window:length(index_data)],
  series = "Реализованная волатильность"
)

data2 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_garch_std[window:length(index_data)]*scale_factor,
  series = "GARCH(1,1)"
)

data3 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_egarch_std[window:length(index_data)]*scale_factor,
  series = "EGARCH(1,1)"
)

data4 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_gjr_std[window:length(index_data)]*scale_factor,
  series = "GJR-GARCH(1,1)"
)

data5 <- data.frame(
  time = index_data[window:length(index_data)],
  value = sigma_garchm_std[window:length(index_data)]*scale_factor,
  series = "GARCH-M(1,1)"
)


# Объединяем два набора данных
data_combined <- rbind(data1, data2, data3, data4, data5)

ggplot(data_combined, aes(x = time, y = value)) +
  # Тонкая и полупрозрачная серая линия для r_t
  geom_line(data = subset(data_combined, series == "r_t"),
            color = "grey60", size = 0.4, alpha = 0.6) +
  
  # Цветные модели — толще и ярче
  geom_line(data = subset(data_combined, series != "r_t"),
            aes(color = series), size = 1.1) +
  
  scale_color_manual(values = c(
    "GARCH(1,1)" = "#D62728",       # красный
    "EGARCH(1,1)" = "#1F77B4",      # синий
    "GJR-GARCH(1,1)" = "#2CA02C",   # зеленый
    "GARCH-M(1,1)" = "#FF7F0E"      # оранжевый
  )) +
  
  labs(title = "SBER: Реализованная волатильность и модели GARCH (t. распр)",
       x = "Дата", y = "σ", color = "Модель") +
  theme_minimal() +
  theme(legend.position = "bottom")




#################### Оценка качества для норм распр ####################################

# Приводим волатильности к одинаковому масштабу, умножив на 100 для процентов
# И умножаем на корень из 252 для получения годовой волатильности
window <- 20
scale_factor <- sqrt(252) * 100

# Применяем преобразование для всех моделей
sigma_garch_norm_scaled <- sigma_garch_norm[window:length(index_data)] * scale_factor
sigma_egarch_norm_scaled <- sigma_egarch_norm[window:length(index_data)] * scale_factor
sigma_gjr_norm_scaled <- sigma_gjr_norm[window:length(index_data)] * scale_factor
sigma_garchm_norm_scaled <- sigma_gjr_garchm_norm[window:length(index_data)] * scale_factor

# Для реализованной волатильности тоже умножаем на 100 и на sqrt(252), если нужно
rv_rets_20_scaled <- rv_20[window:length(index_data)]  # Реализованная волатильность уже в нужном масштабе



# Среднеквадратическая ошибка (RMSE)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Средняя абсолютная ошибка (MAE)
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Коэффициент детерминации (R²)
r_squared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r2 <- 1 - (ss_residual / ss_total)
  return(r2)
}

# Средняя абсолютная процентная ошибка (MAPE)
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Преобразуем в массив
rv_rets_20_scaled <- as.numeric(rv_rets_20_scaled)
sigma_garch_norm_scaled <- as.numeric(sigma_garch_norm_scaled)
sigma_egarch_norm_scaled <- as.numeric(sigma_egarch_norm_scaled)
sigma_gjr_norm_scaled <- as.numeric(sigma_gjr_norm_scaled)
sigma_garchm_norm_scaled <- as.numeric(sigma_garchm_norm_scaled)

# Рассчитываем все метрики для каждой модели
metrics <- list()

# GARCH(1,1)
metrics$garch <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_garch_norm_scaled),
  mae = mae(rv_rets_20_scaled, sigma_garch_norm_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_garch_norm_scaled),
  mape = mape(rv_rets_20_scaled, sigma_garch_norm_scaled)
)

# EGARCH(1,1)
metrics$egarch <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_egarch_norm_scaled),
  mae = mae(rv_rets_20_scaled, sigma_egarch_norm_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_egarch_norm_scaled),
  mape = mape(rv_rets_20_scaled, sigma_egarch_norm_scaled)
)

# GJR-GARCH(1,1)
metrics$gjr <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_gjr_norm_scaled),
  mae = mae(rv_rets_20_scaled, sigma_gjr_norm_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_gjr_norm_scaled),
  mape = mape(rv_rets_20_scaled, sigma_gjr_norm_scaled)
)

# GARCH-M(1,1)
metrics$garchm <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_garchm_norm_scaled),
  mae = mae(rv_rets_20_scaled, sigma_garchm_norm_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_garchm_norm_scaled),
  mape = mape(rv_rets_20_scaled, sigma_garchm_norm_scaled)
)

# Выводим все метрики
cat("GARCH(1,1) - RMSE:", metrics$garch$rmse, "MAE:", metrics$garch$mae, "R²:", metrics$garch$r2, "MAPE:", metrics$garch$mape, "\n")
cat("EGARCH(1,1) - RMSE:", metrics$egarch$rmse, "MAE:", metrics$egarch$mae, "R²:", metrics$egarch$r2, "MAPE:", metrics$egarch$mape, "\n")
cat("GJR-GARCH(1,1) - RMSE:", metrics$gjr$rmse, "MAE:", metrics$gjr$mae, "R²:", metrics$gjr$r2, "MAPE:", metrics$gjr$mape, "\n")
cat("GARCH-M(1,1) - RMSE:", metrics$garchm$rmse, "MAE:", metrics$garchm$mae, "R²:", metrics$garchm$r2, "MAPE:", metrics$garchm$mape, "\n")


################################################################################




######################### Оценка качества для t распр ##########################


# Приводим волатильности к одинаковому масштабу, умножив на 100 для процентов
# И умножаем на корень из 252 для получения годовой волатильности
window <- 20
scale_factor <- sqrt(252) * 100

# Применяем преобразование для всех моделей
sigma_garch_std_scaled <- sigma_garch_std[window:length(index_data)] * scale_factor
sigma_egarch_std_scaled <- sigma_egarch_std[window:length(index_data)] * scale_factor
sigma_gjr_std_scaled <- sigma_gjr_std[window:length(index_data)] * scale_factor
sigma_garchm_std_scaled <- sigma_garchm_std[window:length(index_data)] * scale_factor

# Для реализованной волатильности тоже умножаем на 100 и на sqrt(252), если нужно
rv_rets_20_scaled <- rv_20[window:length(index_data)]  # Реализованная волатильность уже в нужном масштабе

# Преобразуем в массив
rv_rets_20_scaled <- as.numeric(rv_rets_20_scaled)
sigma_garchm_std_scaled <- as.numeric(sigma_garchm_std_scaled)
sigma_egarch_std_scaled <- as.numeric(sigma_egarch_std_scaled)
sigma_gjr_std_scaled <- as.numeric(sigma_gjr_std_scaled)
sigma_garchm_std_scaled <- as.numeric(sigma_garchm_std_scaled)

# Рассчитываем все метрики для каждой модели
metrics1 <- list()

# GARCH(1,1)
metrics1$garch <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_garch_std_scaled),
  mae = mae(rv_rets_20_scaled, sigma_garch_std_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_garch_std_scaled),
  mape = mape(rv_rets_20_scaled, sigma_garch_std_scaled)
)

# EGARCH(1,1)
metrics1$egarch <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_egarch_std_scaled),
  mae = mae(rv_rets_20_scaled, sigma_egarch_std_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_egarch_std_scaled),
  mape = mape(rv_rets_20_scaled, sigma_egarch_std_scaled)
)

# GJR-GARCH(1,1)
metrics1$gjr <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_gjr_std_scaled),
  mae = mae(rv_rets_20_scaled, sigma_gjr_std_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_gjr_std_scaled),
  mape = mape(rv_rets_20_scaled, sigma_gjr_std_scaled)
)

# GARCH-M(1,1)
metrics1$garchm <- list(
  rmse = rmse(rv_rets_20_scaled, sigma_garchm_std_scaled),
  mae = mae(rv_rets_20_scaled, sigma_garchm_std_scaled),
  r2 = r_squared(rv_rets_20_scaled, sigma_garchm_std_scaled),
  mape = mape(rv_rets_20_scaled, sigma_garchm_std_scaled)
)

# Выводим все метрики
cat("GARCH(1,1) - RMSE:", metrics1$garch$rmse, "MAE:", metrics1$garch$mae, "R²:", metrics1$garch$r2, "MAPE:", metrics1$garch$mape, "\n")
cat("EGARCH(1,1) - RMSE:", metrics1$egarch$rmse, "MAE:", metrics1$egarch$mae, "R²:", metrics1$egarch$r2, "MAPE:", metrics1$egarch$mape, "\n")
cat("GJR-GARCH(1,1) - RMSE:", metrics1$gjr$rmse, "MAE:", metrics1$gjr$mae, "R²:", metrics1$gjr$r2, "MAPE:", metrics1$gjr$mape, "\n")
cat("GARCH-M(1,1) - RMSE:", metrics1$garchm$rmse, "MAE:", metrics1$garchm$mae, "R²:", metrics1$garchm$r2, "MAPE:", metrics1$garchm$mape, "\n")












############################# Прогноз бек тест VaR #############################

##################################### GARCH_norm(1,1)
var.n.garch_norm = ugarchroll(spec_garch_norm, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                                500, refit.every = 5, window.size = 500, refit.window = "rolling",
                              calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))



# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.garch_norm, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.garch_norm, which = 4, VaR.alpha = 0.05)

report(var.n.garch_norm, VaR.alpha = 0.01)
report(var.n.garch_norm, VaR.alpha = 0.05)



##################################### EGARCH_norm(1,1)
var.n.egarch_norm = ugarchroll(spec_egarch_norm, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                                 500, refit.every = 5, window.size = 500, refit.window = "rolling",
                               calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))


# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.egarch_norm, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.egarch_norm, which = 4, VaR.alpha = 0.05)

report(var.n.egarch_norm, VaR.alpha = 0.01)
report(var.n.egarch_norm, VaR.alpha = 0.05)


################################### GJR-GARCH_norm(1,1)
var.n.gjr_norm = ugarchroll(spec_gjr_norm, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                              500, refit.every = 5, window.size = 500, refit.window = "rolling",
                            calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))

# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.gjr_norm, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.gjr_norm, which = 4, VaR.alpha = 0.05)

report(var.n.gjr_norm, VaR.alpha = 0.01)
report(var.n.gjr_norm, VaR.alpha = 0.05)


################################### GARCH-M_norm(1,1)
var.n.garchm_norm = ugarchroll(spec_garchm_norm, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                                 500, refit.every = 5, window.size = 500, refit.window = "rolling",
                               calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))

# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.garchm_norm, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.garchm_norm, which = 4, VaR.alpha = 0.05)

report(var.n.garchm_norm, VaR.alpha = 0.01)
report(var.n.garchm_norm, VaR.alpha = 0.05)















##################################### GARCH_std(1,1)
var.n.garch_std = ugarchroll(spec_garch_std, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                                500, refit.every = 5, window.size = 500, refit.window = "rolling",
                              calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))



# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.garch_std, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.garch_std, which = 4, VaR.alpha = 0.05)

report(var.n.garch_std, VaR.alpha = 0.01)
report(var.n.garch_std, VaR.alpha = 0.05)



##################################### EGARCH_std(1,1)
var.n.egarch_std = ugarchroll(spec_egarch_std, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                                 500, refit.every = 5, window.size = 500, refit.window = "rolling",
                               calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))


# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.egarch_std, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.egarch_std, which = 4, VaR.alpha = 0.05)

report(var.n.egarch_std, VaR.alpha = 0.01)
report(var.n.egarch_std, VaR.alpha = 0.05)


################################### GJR-GARCH_std(1,1)
var.n.gjr_std = ugarchroll(spec_gjr_std, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                              500, refit.every = 5, window.size = 500, refit.window = "rolling",
                            calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))

# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.gjr_std, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.gjr_std, which = 4, VaR.alpha = 0.05)

report(var.n.gjr_std, VaR.alpha = 0.01)
report(var.n.gjr_std, VaR.alpha = 0.05)


################################### GARCH-M_std(1,1)
var.n.garchm_std = ugarchroll(spec_garchm_std, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                                 500, refit.every = 5, window.size = 500, refit.window = "rolling",
                               calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))

# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.n.garchm_std, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.n.garchm_std, which = 4, VaR.alpha = 0.05)

report(var.n.garchm_std, VaR.alpha = 0.01)
report(var.n.garchm_std, VaR.alpha = 0.05)


























