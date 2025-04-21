setwd("E:/МЕХМАТ обучение/4 семестр/очка/НИР/Diplom")

# # Control + Shift + c - комментировать
# install.packages("ggthemes")
# install.packages("gridExtra")


# Загружаем необходимые библиотеки
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
library(xts)
library(zoo)


data <- read.csv("portfolio_2015_2025.csv")
colSums(is.na(data))  # количество NA в каждом столбце

head(data)

# Преобразуем строки в даты
data$X <- as.Date(data$X, format = "%Y-%m-%d")

data_xts <- xts(data$ret_GAZP, order.by = data$X)


subset_xts <- window(data_xts, start = as.Date("2020-01-01"),
                     end = as.Date("2022-02-24"))


length(subset_xts)

rets = subset_xts

p1 = qplot(x = 1:length(rets) , y = rets , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets) , color = 'red' , size = 1) + 
  labs(x = '' , y = 'Daily Returns')

p2 = qplot(rets , geom = 'density') + coord_flip() + geom_vline(xintercept = mean(rets) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + labs(x = '')

grid.arrange(p1 , p2 , ncol = 2)


# Стационарность

adf.test(rets)


# Оценка среднего через ARMA


model.arima = auto.arima(rets , max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')
model.arima


model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)

ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')



# GARCH(1,1)

tsdisplay(ar.res^2 , main = 'Squared Residuals')


garch_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0)))

fit_garch = ugarchfit(spec = garch_spec, data = ar.res)
fit_garch

# Получим сигмы
sigma_vals <- sigma(fit_garch)
length(sigma_vals)


# Создаём xts-ряд
sigma_vals_xts <- xts(sigma_vals, order.by = index(subset_xts))

# строим график sigma для GARCH(1,1)
plot.zoo(sigma_vals_xts, main = "Условная волатильность (σ)", 
         col = "darkred", lwd = 2, xlab = "Дата", ylab = "σ")
grid()


# Прогнозируем sigma на 10 дней вперёд
garch_forecast <- ugarchforecast(fit_garch, n.ahead = 10)
garch_forecast

# Извлекаем прогнозируемые значения волатильности
forecast_sigma <- sigma(garch_forecast)

# Создаём xts-ряд для прогнозируемых значений
# Для упрощения можно использовать те же даты, что и в исходных данных, сдвинутые на 10 дней вперёд
forecast_dates <- seq(from = tail(index(subset_xts), 1), by = "days", length.out = 11)[-1]
forecast_sigma_xts <- xts(forecast_sigma, order.by = forecast_dates)


# Строим график прогнозируемой волатильности (σ)
plot.zoo(forecast_sigma_xts, main = "Прогноз условной волатильности (σ) на 10 дней вперёд", 
         col = "darkblue", lwd = 2, xlab = "Дата", ylab = "σ")
grid()


par1 = par()  #save graphic parameters

par(mfrow = c(1, 2))
# generate plots using the which argument Figure-12 1. ACF of
# standardised residuals
plot(fit_garch, which = 10)
# 2. Conditional SD (vs |returns|)
plot(fit_garch, which = 3)


# Пример с Expanding Window
# Прогнозирование с расширяющимся окном

# # Прогнозирование с фиксированным окном
# garch_roll_fixed <- ugarchroll(spec = garch_spec, data = ar.res, 
#                                n.ahead = 1, forecast.length = 500,
#                                refit.every = 1, refit.window = "moving", 
#                                window.size = 100)

















spec2 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)), distribution.model = "std")


var.t = ugarchroll(spec2, data = ar.res, n.ahead = 1,forecast.length = ndays(ar.res) -
                     500, refit.every = 5, window.size = 500, refit.window = "rolling",
                   calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))


par(mfrow = c(1, 2))
plot(var.t, which = 4, VaR.alpha = 0.01)
# 5% Student-t GARCH VaR
plot(var.t, which = 4, VaR.alpha = 0.05)



# backtest for VaR forecasts

report(var.t, VaR.alpha = 0.05)  #default value of alpha is 0.01











