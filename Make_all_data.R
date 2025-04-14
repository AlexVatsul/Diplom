setwd("E:/МЕХМАТ обучение/4 семестр/очка/НИР/Diplom")

# Control + Shift + c - комментировать

# install.packages('tidyverse')
# install.packages('rio')
# install.packages('fpp3')
# install.packages('car')
# install.packages('urca')
# install.packages('ggplot2')
# install.packages('patchwork')
# install.packages('rugarch')
# install.packages('FinTS')
# install.packages('quantmod')
# install.packages('xts')
# install.packages('dplyr')
# install.packages('rugarch')
# install.packages('rmgarch')
# install.packages('PerformanceAnalytics')
# install.packages('quadprog')
# install.packages('tseries')
# install.packages('e1071')
# install.packages("forecast")
# install.packages("ggplot2")

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


# ------------------------------
# 1. ПОДГОТОВКА ДАННЫХ
# ------------------------------


# Загрузка CSV файлов для каждого актива (замени пути на свои)
# CHMF <- read.csv("datasets/CHMF.csv")
GAZP <- read.csv("datasets/GAZP.csv")
# LKOH <- read.csv("datasets/LKOH.csv")
# MAGN <- read.csv("datasets/MAGN.csv")
MGNT <- read.csv("datasets/MGNT.csv")
MTSS <- read.csv("datasets/MTSS.csv")
NLMK <- read.csv("datasets/NLMK.csv")
# NVTK <- read.csv("datasets/NVTK.csv")
# PLZL <- read.csv("datasets/PLZL.csv")
# POLY <- read.csv("datasets/POLY.csv")
# ROSN <- read.csv("datasets/ROSN.csv")
SBER <- read.csv("datasets/SBER.csv")
# SNGS <- read.csv("datasets/SNGS.csv")
# TATN <- read.csv("datasets/TATN.csv")


# Преобразуем данные в формат xts
# CHMF_xts <- xts(CHMF$Close, order.by = as.Date(CHMF$Date))
GAZP_xts <- xts(GAZP$Close, order.by = as.Date(GAZP$Date))
# LKOH_xts <- xts(LKOH$Close, order.by = as.Date(LKOH$Date))
# MAGN_xts <- xts(MAGN$Close, order.by = as.Date(MAGN$Date))
MGNT_xts <- xts(MGNT$Close, order.by = as.Date(MGNT$Date))
MTSS_xts <- xts(MTSS$Close, order.by = as.Date(MTSS$Date))
NLMK_xts <- xts(NLMK$Close, order.by = as.Date(NLMK$Date))
# NVTK_xts <- xts(NVTK$Close, order.by = as.Date(NVTK$Date))
# PLZL_xts <- xts(PLZL$Close, order.by = as.Date(PLZL$Date))
# POLY_xts <- xts(POLY$Close, order.by = as.Date(POLY$Date))
# ROSN_xts <- xts(ROSN$Close, order.by = as.Date(ROSN$Date))
SBER_xts <- xts(SBER$Close, order.by = as.Date(SBER$Date))
# SNGS_xts <- xts(SNGS$Close, order.by = as.Date(SNGS$Date))
# TATN_xts <- xts(TATN$Close, order.by = as.Date(TATN$Date))



# Отбираем данные с 2015 года по 2025 год
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2025-12-31")



# CHMF_xts <- window(CHMF_xts, start = start_date, end = end_date)
GAZP_xts <- window(GAZP_xts, start = start_date, end = end_date)
# LKOH_xts <- window(LKOH_xts, start = start_date, end = end_date)
# MAGN_xts <- window(MAGN_xts, start = start_date, end = end_date)
MGNT_xts <- window(MGNT_xts, start = start_date, end = end_date)
MTSS_xts <- window(MTSS_xts, start = start_date, end = end_date)
NLMK_xts <- window(NLMK_xts, start = start_date, end = end_date)
# NVTK_xts <- window(NVTK_xts, start = start_date, end = end_date)
# PLZL_xts <- window(PLZL_xts, start = start_date, end = end_date)
# POLY_xts <- window(POLY_xts, start = start_date, end = end_date)
# ROSN_xts <- window(ROSN_xts, start = start_date, end = end_date)
SBER_xts <- window(SBER_xts, start = start_date, end = end_date)
# SNGS_xts <- window(SNGS_xts, start = start_date, end = end_date)
# TATN_xts <- window(TATN_xts, start = start_date, end = end_date)




# Объединяем все активы в одну таблицу
combined_data <- merge(GAZP_xts, MGNT_xts, MTSS_xts, NLMK_xts, SBER_xts)

# Присваиваем понятные имена колонкам
colnames(combined_data) <- c("GAZP", "MGNT", "MTSS", "NLMK", "SBER")

# Выводим итоговую таблицу (первые несколько строк)
head(combined_data)


colSums(is.na(combined_data))  # количество NA в каждом столбце
# Удалить строки с NA
combined_data <- na.omit(combined_data)

# combined_data$ret_CHMF <- log(combined_data$CHMF)-log(dplyr::lag(combined_data$CHMF))
combined_data$ret_GAZP <- log(combined_data$GAZP)-log(dplyr::lag(combined_data$GAZP))
# combined_data$ret_LKOH <- log(combined_data$LKOH)-log(dplyr::lag(combined_data$LKOH))
# combined_data$ret_MAGN <- log(combined_data$MAGN)-log(dplyr::lag(combined_data$MAGN))
combined_data$ret_MGNT <- log(combined_data$MGNT)-log(dplyr::lag(combined_data$MGNT))
combined_data$ret_MTSS <- log(combined_data$MTSS)-log(dplyr::lag(combined_data$MTSS))
combined_data$ret_NLMK <- log(combined_data$NLMK)-log(dplyr::lag(combined_data$NLMK))
# combined_data$ret_NVTK <- log(combined_data$NVTK)-log(dplyr::lag(combined_data$NVTK))
# combined_data$ret_PLZL <- log(combined_data$PLZL)-log(dplyr::lag(combined_data$PLZL))
# combined_data$ret_POLY <- log(combined_data$POLY)-log(dplyr::lag(combined_data$POLY))
# combined_data$ret_ROSN <- log(combined_data$ROSN)-log(dplyr::lag(combined_data$ROSN))
combined_data$ret_SBER <- log(combined_data$SBER)-log(dplyr::lag(combined_data$SBER))
# combined_data$ret_SNGS <- log(combined_data$SNGS)-log(dplyr::lag(combined_data$SNGS))
# combined_data$ret_TATN <- log(combined_data$TATN)-log(dplyr::lag(combined_data$TATN))

combined_data <- combined_data[-1, ]


# Сохраняем результат в новый CSV файл (по желанию)
write.csv(as.data.frame(combined_data), "portfolio_2015_2025.csv")


