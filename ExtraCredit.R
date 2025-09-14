# =======================================================
# Full Analysis Script: Correlation, Sharpe, Beta, Alpha
# =======================================================

# Install packages if needed
# install.packages(c("tidyquant", "dplyr", "GGally"))

library(tidyquant)
library(dplyr)
library(GGally)

# ----------------------------
# Step 1: Define tickers
# ----------------------------
tickers <- c("GEV", "NBIS", "ALAB", "NVDA", "PLTR")

# ----------------------------
# Step 2: Get monthly returns
# ----------------------------
stock_returns <- tq_get(tickers,
                        get  = "stock.prices",
                        from = "2015-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "monthly_return"
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = symbol, values_from = monthly_return)

# Clean missing data
stock_returns_clean <- na.omit(stock_returns)

# ----------------------------
# Step 3: Correlation Matrix
# ----------------------------
cor_matrix <- cor(stock_returns_clean[ , -1])   # exclude date column
print("Correlation Matrix:")
print(cor_matrix)

# Optional visualization
ggpairs(stock_returns_clean[, -1],
        title = "Monthly Return Correlation Matrix")

# ----------------------------
# Step 4: Get S&P500 (market proxy)
# ----------------------------
sp500 <- tq_get("^GSPC",
                get = "stock.prices",
                from = "2015-01-01") %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "market_return")

# ----------------------------
# Step 5: Merge with stock data (long format)
# ----------------------------
stocks_long <- tq_get(tickers,
                      get  = "stock.prices",
                      from = "2015-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "stock_return")

returns <- left_join(stocks_long, sp500, by = "date")

# ----------------------------
# Step 6: Risk-free rate (example)
# ----------------------------
rf_annual  <- 0.0432
rf_monthly <- rf_annual / 12

# ----------------------------
# Step 7: Calculate metrics
# ----------------------------
results <- returns %>%
  group_by(symbol) %>%
  summarise(
    # Excess returns
    excess_stock  = mean(stock_return - rf_monthly, na.rm = TRUE),
    excess_market = mean(market_return - rf_monthly, na.rm = TRUE),
    
    # Sharpe Ratio
    sharpe_monthly = mean(stock_return - rf_monthly, na.rm = TRUE) /
      sd(stock_return, na.rm = TRUE),
    sharpe_annual  = sharpe_monthly * sqrt(12),
    
    # Regression for Beta & Alpha
    beta  = coef(lm(I(stock_return - rf_monthly) ~ I(market_return - rf_monthly)))[2],
    alpha_monthly = coef(lm(I(stock_return - rf_monthly) ~ I(market_return - rf_monthly)))[1],
    alpha_annual  = (1 + alpha_monthly)^12 - 1
  )

print("Sharpe Ratios, Beta, Jensen's Alpha:")
print(results)
