setwd("E:/МЕХМАТ обучение/4 семестр/очка/НИР/Diplom")

# Control + Shift + c - комментировать

# Загружаем необходимые библиотеки
library(quantmod)
library(xts)
library(dplyr)
library(rugarch)
library(rmgarch)
library(PerformanceAnalytics)
library(quadprog)
library(FinTS)
library(tseries)
library(e1071)
library(forecast)
library(ggplot2)
library(gridExtra)
library(fImport)
library(fPortfolio)
library(zoo)




data <- read.csv("portfolio_2015_2025.csv")
colSums(is.na(data))  # количество NA в каждом столбце

head(data)


# Шаг 3. Пример диверсифицированного портфеля (6–8 активов)
# Можно выбрать такую комбинацию:
#   
# 1. Энергетика: ret_GAZP
# 2. Металлургия: ret_NLMK
# 3. Финансы: ret_SBER
# 4. Телекоммуникации: ret_MTSS
# 5. Розничная торговля: ret_MGNT

ret_GAZP <- data$ret_GAZP # Газпром - энергетика
ret_MGNT <- data$ret_MGNT # Магнит - сеть розничных магазинов
ret_MTSS <- data$ret_MTSS # МТС - телекоммуникационные услуги
ret_NLMK <- data$ret_NLMK # НЛМК(Новолипецкий металлургический комбинат) - металлургия
ret_SBER <- data$ret_SBER # СберБанк - финансы


plot.ts(ret_GAZP)
plot.ts(ret_MGNT)
plot.ts(ret_MTSS)
plot.ts(ret_NLMK)
plot.ts(ret_SBER)



adf.test(ret_GAZP)
adf.test(ret_MGNT)
adf.test(ret_MTSS)
adf.test(ret_NLMK)
adf.test(ret_SBER)


model.arima = auto.arima(ret_GAZP, max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')

model.arima

model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)


# Проверка на то, что остаточные значения не коррелируют. Тест Юнга-Бокса (Ljung-Box test)
# если p-value > 0.05, то остатки ведут себя как белый шум

ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')


tsdisplay(ar.res^2 , main = 'Squared Residuals')


# Model specification
model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(0 , 0)))


model.fit = ugarchfit(spec = model.spec , data = ar.res , solver = 'solnp')
model.fit


par1 = par()  #save graphic parameters

par(mfrow = c(1, 2))
# generate plots using the which argument Figure-12 1. ACF of
# standardised residuals
plot(model.fit, which = 10)
# 2. Conditional SD (vs |returns|)
plot(model.fit, which = 3)



# spec2 with student-t distribution
spec2 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(0, 0)),
                   distribution.model = "std")


class(ret_GAZP)
data$X

ret_GAZP_xts <- xts(ret_GAZP, order.by = as.Date(data$X))


var.t = ugarchroll(spec2, data = ret_GAZP_xts, n.ahead = 1, forecast.length = ndays(ret_GAZP_xts) -
                     500, refit.every = 5, window.size = 500, refit.window = "rolling",
                   calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))


# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.t, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.t, which = 4, VaR.alpha = 0.05)



# backtest for VaR forecasts

report(var.t, VaR.alpha = 0.05)  #default value of alpha is 0.01



spec2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0)),
                    distribution.model = "std")


var.t <- ugarchroll(spec2, data = ret_GAZP_xts, n.ahead = 1, forecast.length = ndays(ret_GAZP_xts) - 500, 
                    refit.every = 5, window.size = 500, refit.window = "rolling", 
                    calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))



# note the plot method provides four plots with option-4 for the VaR
# forecasts 1% Student-t GARCH VaR
par(mfrow = c(1, 2))
plot(var.t, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.t, which = 4, VaR.alpha = 0.05)



# backtest for VaR forecasts

report(var.t, VaR.alpha = 0.05)  #default value of alpha is 0.01




options(scipen = 999)
model.fit@fit$matcoef


quantile(ret_GAZP , 0.05)

# Тест Жарка-Бера проверяет гипотезу о том, что доходность акций соответствует
# нормальному распределению

jarque.bera.test(ret_GAZP)


































head(data$ret_GAZP)
ret_GAZP
head(data)



























ArchTest(ret_GAZP)
ArchTest(ret_MGNT)
ArchTest(ret_MTSS)
ArchTest(ret_NLMK)
ArchTest(ret_SBER)

garch(ret_GAZP, grad="numerical", trace=FALSE)
garch(ret_MGNT, grad="numerical", trace=FALSE)
garch(ret_MTSS, grad="numerical", trace=FALSE)
garch(ret_NLMK, grad="numerical", trace=FALSE)
garch(ret_SBER, grad="numerical", trace=FALSE)

# 1. Найдем ARIMA модель
arima_GAZP <- auto.arima(ret_GAZP)
summary(arima_GAZP)
arima_MGNT <- auto.arima(ret_MGNT)
summary(arima_MGNT)
arima_MTSS <- auto.arima(ret_MTSS)
summary(arima_MTSS)
arima_NLMK <- auto.arima(ret_NLMK)
summary(arima_NLMK)
arima_SBER <- auto.arima(ret_SBER)
summary(arima_SBER)

# 2. Извлекаем параметры ARIMA
arima_order_GAZP <- arimaorder(arima_GAZP)  # Вернет вектор c(p, d, q)
arima_order_MGNT <- arimaorder(arima_MGNT)  # Вернет вектор c(p, d, q)
arima_order_MTSS <- arimaorder(arima_MTSS)  # Вернет вектор c(p, d, q)
arima_order_NLMK <- arimaorder(arima_NLMK)  # Вернет вектор c(p, d, q)
arima_order_SBER <- arimaorder(arima_SBER)  # Вернет вектор c(p, d, q)



# ------------------------------
# 1. ОДНОМЕРНЫЕ GARCH-МОДЕЛИ
# ------------------------------

# 1) Анализ: GAZP

################################################################################
# GARCH(1,1)
spec_garch_GAZP <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(0, 0)),  # ARIMA без фракционности
  distribution.model = "norm"  # Можешь попробовать "std" или "ged"
)

# Подгонка модели
fit_garch_GAZP <- ugarchfit(spec = spec_garch_GAZP, data = ret_GAZP)
fit_garch_GAZP

forecast_garch_GAZP <- ugarchforecast(fit_garch_GAZP, n.ahead = 10)
forecast_garch_GAZP

################################################################################


################################################################################
# EGARCH(1,1)
spec_egarch_GAZP <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(0, 0)),  # ARIMA без фракционности
  distribution.model = "norm"  # Можешь попробовать "std" или "ged"
)

# Подгонка модели
fit_egarch_GAZP <- ugarchfit(spec = spec_egarch_GAZP, data = ret_GAZP)
fit_egarch_GAZP

forecast_egarch_GAZP <- ugarchforecast(fit_egarch_GAZP, n.ahead = 10)
forecast_egarch_GAZP

################################################################################


################################################################################
# GJR GARCH(1,1)
spec_gjr_garch_GAZP <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(0, 0)),  # ARIMA без фракционности
  distribution.model = "norm"  # Можешь попробовать "std" или "ged"
)

# Подгонка модели
fit_gjr_garch_GAZP <- ugarchfit(spec = spec_gjr_garch_GAZP, data = ret_GAZP)
fit_gjr_garch_GAZP

forecast_gjr_garch_GAZP <- ugarchforecast(fit_gjr_garch_GAZP, n.ahead = 10)
forecast_gjr_garch_GAZP

################################################################################
























