# PORTFOLIO RISK PROJECT

# Packages
# install.packages("quantmod", "PerformanceAnalytics","ggplot2","zoo")

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)

# ---- 1. Load data ----
tickers <- c("AAPL", "MSFT", "GOOG")

getSymbols(tickers, from = "2020-01-01")
getSymbols("SPY", from = "2020-01-01")

# ---- 2. Adjusted prices ----
stock_prices <- merge(Ad(AAPL), Ad(MSFT), Ad(GOOG))
colnames(stock_prices) <- tickers

# ---- 3. Daily returns calculation ----
stock_ret <- na.omit(Return.calculate(stock_prices))

# ---- 4. Equal-weight portfolio ----
w <- c(1/3, 1/3, 1/3)
portf_ret <- Return.portfolio(stock_ret, weights = w)

# ---- 5. Market returns for beta / alpha ----
market_ret <- na.omit(Return.calculate(Ad(SPY)))

analysis_data <- na.omit(merge(portf_ret, market_ret))
colnames(analysis_data) <- c("portfolio", "market")

# ---- 6. Risk metrics ----
sharpe_ratio <- SharpeRatio.annualized(analysis_data$portfolio, scale = 252)
max_drawdown <- maxDrawdown(analysis_data$portfolio)
cvar_95 <- ETL(analysis_data$portfolio, p = 0.95)

# ---- 7. Alpha and beta ----
regression_model <- lm(portfolio ~ market, data = as.data.frame(analysis_data))
alpha <- coef(regression_model)[1] * 252
beta <- coef(regression_model)[2]

sharpe_ratio
max_drawdown
cvar_95
alpha
beta

# ---- 8. Rolling volatility (20-day) ----
# rolling volatility
rolling_volatility <- runSD(portf_ret, n = 20) * sqrt(252)

# plot
vol_data <- na.omit(data.frame(
  date = index(rolling_volatility),
  vol = as.numeric(rolling_volatility)
))

ggplot(vol_data, aes(x = date, y = vol)) +
  geom_line() +
  labs(title = "Rolling Volatility")

# ---- 9. Cumulative return chart ----
Sys.setlocale("LC_ALL", "C")
chart.CumReturns(portf_ret, main = "Cumulative Return")

# ---- 10. Return distribution ----
return_plot_data <- data.frame(
  Return = as.numeric(portf_ret)
)

ggplot(return_plot_data, aes(x = Return)) +
  geom_histogram(bins = 50) +
  labs(title = "Portfolio Return Distribution", x = "Daily Return", y = "Frequency")
