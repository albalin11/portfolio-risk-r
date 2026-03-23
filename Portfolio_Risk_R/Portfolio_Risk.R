library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)                

tickers <- c("AAPL", "MSFT", "GOOG")
start_date <- "2020-01-01"
end_date <- Sys.Date()

getSymbols(tickers, from = start_date, to = end_date, auto.assign = TRUE)

prices <- do.call(merge, lapply(tickers, function(x) Ad(get(x))))
colnames(prices) <- tickers

returns <- na.omit(Return.calculate(prices, method = "log"))

weights <- c(1/3, 1/3, 1/3)
port_returns <- Return.portfolio(returns, weights = weights)

VaR_95 <- VaR(port_returns, p = 0.95, method = "historical")


chart.CumReturns(port_returns, main = "Portfolio Cumulative Returns", 
                 wealth.index = TRUE, legend.loc = "topleft")

ann_ret <- Return.annualized(returns, scale = 252)
ann_sd  <- StdDev.annualized(returns, scale = 252)
port_ann_ret <- Return.annualized(port_returns, scale = 252)
port_ann_sd  <- StdDev.annualized(port_returns, scale = 252)

df <- data.frame(
  Asset = c(tickers, "Portfolio"),
  Return = c(ann_ret, port_ann_ret),
  Risk   = c(ann_sd, port_ann_sd))

ggplot(df, aes(x = Risk, y = Return, label = Asset)) +
  geom_point(size = 3, color = "darkblue") +
  geom_text(vjust = -0.8) +
  labs(title = "Risk-Return Profile",
       x = "Annualized Volatility",
       y = "Annualized Return") +
  theme_minimal()

port_returns_vec <- as.numeric(port_returns)
ggplot(data.frame(Return = port_returns_vec), aes(x = Return)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black") +
  geom_vline(xintercept = VaR_95, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = VaR_95 - 0.005, y = 10,
           label = paste0("95% VaR = ", round(VaR_95, 4))) +
  labs(title = "Portfolio Return Distribution with VaR",
       x = "Daily Return",
       y = "Frequency") +
  theme_minimal()

