# PORTFOLIO RISK PROJECT

# Packages
# install.packages("quantmod", "PerformanceAnalytics","ggplot2","zoo")

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)

# ---- 1. Parameters ----
tickers <- c("AAPL", "MSFT", "GOOG")
benchmark <- "SPY"
start_date <- "2020-01-01"
weights <- c(1/3, 1/3, 1/3)

# ---- 2. Load data ----
getSymbols(tickers, from = start_date)
getSymbols(benchmark, from = start_date)

# Adjusted prices 
stock_prices <- merge(Ad(AAPL), Ad(MSFT), Ad(GOOG))
colnames(stock_prices) <- tickers
market_prices <- Ad(SPY)

# ---- 3. Daily returns calculation ----
stock_ret <- na.omit(Return.calculate(stock_prices))
market_ret <- na.omit(Return.calculate(market_prices))

# ---- 4. Individual stock metrics ----
ann_return <- apply(stock_ret, 2, Return.annualized, scale = 252)
ann_vol <- apply(stock_ret, 2, sd) * sqrt(252)
VaR_95 <- apply(stock_ret, 2, function(x) abs(VaR(x, p = 0.95, method = "historical")))
CVaR_95 <- apply(stock_ret, 2, function(x) abs(ETL(x, p = 0.95, method = "historical")))

stock_metrics <- data.frame(
  ticker = tickers,
  annualized_return = ann_return,
  annualized_volatility = ann_vol,
  VaR_95 = VaR_95,
  CVaR_95 = CVaR_95
)

print(stock_metrics)

# ---- 5. Portfolio ----
portf_ret <- Return.portfolio(stock_ret, weights = weights)

# Portfolio metrics
port_annual_return <- Return.annualized(portf_ret, scale = 252)
port_annual_vol <- sd(portf_ret) * sqrt(252)
port_var_95 <- abs(VaR(portf_ret, p = 0.95, method = "historical"))
port_cvar_95 <- abs(ETL(portf_ret, p = 0.95, method = "historical"))
port_sharpe <- SharpeRatio.annualized(portf_ret, scale = 252)
port_max_dd <- maxDrawdown(portf_ret)

portfolio_metrics <- data.frame(
  metric = c("annualized_return", "annualized_volatility", "VaR_95", "CVaR_95", "Sharpe_ratio", "max_drawdown"),
  value = c(port_annual_return, port_annual_vol, port_var_95, port_cvar_95, port_sharpe, port_max_dd)
)

print(portfolio_metrics)

# ---- 6. Beta and alpha ----
analysis_data <- na.omit(merge(portf_ret, market_ret))
colnames(analysis_data) <- c("portfolio", "market")

reg_model <- lm(portfolio ~ market, data = analysis_data)
alpha_daily <- coef(reg_model)[1]
beta <- coef(reg_model)[2]
alpha_annual <- alpha_daily * 252

print(paste("Daily alpha:", alpha_daily))
print(paste("Annualized alpha:", alpha_annual))
print(paste("Beta:", beta))

# ---- 7. Rolling volatility (20-day) ----
# Rolling volatility
rolling_volatility <- runSD(portf_ret, n = 20) * sqrt(252)

# Plot
vol_data <- na.omit(data.frame(
  date = index(rolling_volatility),
  vol = as.numeric(rolling_volatility)
))

ggplot(vol_data, aes(x = date, y = vol)) +
  geom_line() +
  labs(title = "Rolling Volatility")

# ---- 8. Plots ----
#Risk vs Return
risk_return_df <- data.frame(
  ticker = tickers,
  annualized_return = ann_return,
  annualized_volatility = ann_vol
)

ggplot(risk_return_df, aes(x = annualized_volatility, y = annualized_return, label = ticker)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -0.5) +
  labs(title = "Risk vs Return", x = "Annualized Volatility", y = "Annualized Return") +
  theme_minimal()

# Cumulative portfolio return
cum_ret <- cumprod(1 + portf_ret) - 1
cum_data <- data.frame(date = index(cum_ret), cum_return = as.numeric(cum_ret))

ggplot(cum_data, aes(x = date, y = cum_return)) +
  geom_line(color = "darkgreen") +
  labs(title = "Cumulative Portfolio Return", x = NULL, y = "Cumulative Return") +
  theme_minimal()

# Portfolio return distribution
return_data <- data.frame(Return = as.numeric(portf_ret))
ggplot(return_data, aes(x = Return)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Portfolio Return Distribution", x = "Daily Return", y = "Frequency") +
  theme_minimal()

