
library(quantmod)

universe <- c('AAPL', 'GOOGL', 'TSLA', 'PFE', 'CSCO', 'AUPH', 'V')
i = 1
x <- getSymbols(Symbols = universe[i], from='2016-01-01', auto.assign = FALSE)
plot(x[,4])
plot(dailyReturn(x[,4]))

for(i in 1:7)
{
  price <- getSymbols(Symbols = universe[i], from='2016-01-01', auto.assign = FALSE)
  ret <- dailyReturn(price[,4])
  if(i == 1) universe_ret <- ret 
  if(i != 1) universe_ret <- merge(universe_ret, ret)
}

colnames(universe_ret) <- universe
head(universe_ret)

w <- rep(1/7, 7)

str(universe_ret)

portfolio <- xts(rowSums(universe_ret * w), order.by = index(universe_ret))
colnames(portfolio) <- "Value"

stocks_ret <- 100*250*apply(universe_ret, 2, mean)
stocks_risk <- 100*sqrt(250)*apply(universe_ret, 2, sd)
plot(stocks_risk, stocks_ret, xlab = 'Risk', ylab = 'Return', pch=16,xlim = c(20,100))
text(stocks_risk, stocks_ret, labels = universe, pos = c(2,1))
grid()

points(100*sqrt(250)*apply(portfolio, 2, sd), 100*250*apply(portfolio, 2, mean), col='red', pch=17)

install.packages("PortfolioAnalytics")
install.packages("DEoptim")

library(PortfolioAnalytics)
library(DEoptim)

pf <- portfolio.spec(universe)

pf <- add.constraint(portfolio = pf, type = "full_investment")
pf <- add.constraint(portfolio = pf, type = "long_only")
pf <- add.constraint(portfolio = pf, type = "box", min=0.05, max=0.25)
portf.minStdDev <- add.objective(pf, type="risk", name="StdDev")

opt_portf <- optimize.portfolio(universe_ret, portf.minStdDev)

w_opt <- opt_portf$weights
portfolio_opt <- xts(rowSums(universe_ret * w), order.by = index(universe_ret))
points(100*sqrt(250)*apply(portfolio_opt, 2, sd), 100*250*apply(portfolio_opt, 2, mean), col='blue', pch=17)

library(PerformanceAnalytics)
charts.PerformanceSummary(portfolio_opt)

getSymbols('SPY')
compare <- merge(portfolio_opt, dailyReturn(SPY[,4]))
charts.PerformanceSummary(compare)


SharpeRatio.annualized(compare)



