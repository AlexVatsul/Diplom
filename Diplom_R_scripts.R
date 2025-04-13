setwd("E:/МЕХМАТ обучение/4 семестр/очка/НИР/Diplom")

install.packages('tidyverse')
install.packages('rio')
install.packages('fpp3')
install.packages('car')
install.packages('urca')
install.packages('ggplot2')
install.packages('patchwork')
install.packages('rugarch')
install.packages('FinTS')

library(tidyverse) # обработка данных
library(rio) # импорт данных
library(fpp3) # работа с временными рядами
library(car) # тестирование гипотез
library(urca) # тесты на стационарность
library(ggplot2) # графики
library(patchwork) # расположение графиков
library(rugarch) # GARCH-модели
library(FinTS) # ARCH test H0: нет GARCH-эффекта

gazp <- read.csv("datasets/GAZP.csv")

colSums(is.na(gazp))  # количество NA в каждом столбце
# Удалить строки с NA
gazp <- na.omit(gazp)


gazp <- mutate(gazp, trading_day = row_number())
gazp <- as_tsibble(gazp, index = trading_day)

# a) построим график цен
autoplot(gazp, Close)



# b) протестируем ряд цен на стационарность с помощью ADF-теста
summary(ur.df(gazp$Close, type = "drift", selectlags = "AIC"))
gg_tsdisplay(gazp, Close, plot_type = 'partial')

# c) рассчитаем ряд лог-доходностей обыкновенных акций компании Газпром и построим график
gazp$ret <- log(gazp$Close)-log(dplyr::lag(gazp$Close))
gazp <- gazp[-1,]
autoplot(gazp, ret)

# d) проверим ряд доходностей на стационарность
summary(ur.df(gazp$ret, type = "drift", selectlags = "AIC"))

# e) рассчитаем оценки ACF и PACF для ряда доходностей
gg_tsdisplay(gazp, ret, plot_type = 'partial')

# подберем автоматически порядок ARIMA модели для доходностей
arima_gazp <- model(gazp, auto = ARIMA(ret))
arima_gazp

# f) сохраним остатки ARMA-модели
d <- augment(arima_gazp) %>% filter(.model=='auto')
gazp$e <- d$.innov

# проверим остатки модели ARMA на стационарность
summary(ur.df(gazp$e, type = "drift", selectlags = "AIC"))
# проанализируем ACF и PACF остатков модели
gg_tsdisplay(gazp, e, plot_type = 'partial')


# протестируем наличие GARCH-эффекта
ArchTest(gazp$e, lags = 2) # тест на наличие GARCH-эффекта в ряду доходностей


# g) оценим стандартную GARCH-модель
spec_11 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                      variance.model = list(garchOrder = c(1, 1), model = "sGARCH"),
                      distribution.model = "norm")

sgarch_11 <- ugarchfit(spec_11, data = gazp$ret)
sgarch_11


spec_12 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                      variance.model = list(garchOrder = c(1, 2), model = "sGARCH"),
                      distribution.model = "norm")

sgarch_12 <- ugarchfit(spec_12, data = gazp$ret)
sgarch_12


spec_21 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                      variance.model = list(garchOrder = c(2, 1), model = "sGARCH"),
                      distribution.model = "norm")

sgarch_21 <- ugarchfit(spec_21, data = gazp$ret)
sgarch_21


# h) расчет волатильности по GARCH-модели
vol_s <- sgarch_11@fit$sigma
vol_ann_s <- vol_s*(250)^(1/2)*100 # прогноз sigma в годовом выражении
plot(vol_ann_s, type = "l", xlab = "", ylab = "vol_ann_s")


# j) потсроим прогноз волатильности по GARCH-модели на 2 шага вперед
vol_s_forec <- ugarchforecast(sgarch_11, gazp$ret, n.ahead = 2)
vol_s_forec


# Рассчитаем VaR
qplot(gazp$ret, geom = 'density') + geom_density(fill = 'lightblue', bins = 30) +
  labs(x = "Daily Returns")

quantile(gazp$ret, 0.05)
quantile(gazp$ret, 0.05) * 10000000

# Пример: VaR на основе GARCH(1,1) с нормальным распределением
sigma_forecast <- sigma(vol_s_forec)  # Прогнозируемая волатильность
VaR_95 <- qnorm(0.05) * sigma_forecast * 10000000  # Для 10 млн рублей
VaR_95






# Из видео









