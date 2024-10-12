library(quantmod)
library(ggplot2)
library(zoo)

# ETFs and Tagesgeld Zinsen (cash interest)
tickers <- c("XDEM.L", "VHYL.AS", "HYG", "REET", 
             "WDSC.L", "EMIM.L")

weights <- c(0.178, 0.182, 0.096, 0.081,
             0.076, 0.07)
tagesgeld_return <- 0.03  # 3% p.a.
tagesgeld_weight <- 0.392

# Get historical data from Yahoo Finance
getSymbols(tickers, src = "yahoo", from = "2015-01-01", to = Sys.Date(), auto.assign = TRUE)

# Combine list: Adjusted Close Prices
prices <- do.call(merge, lapply(tickers, function(ticker) Ad(get(ticker))))
for(ticker in tickers){
  rm(list = ticker)
}
rm(ticker, tickers)

# Step 1: Calculate log returns for each ETF (log(P_t / P_{t-1}))
log_prices <- na.omit(diff(log(prices)))  # Log returns

# Step 2: Calculate the log return for the Tagesgeld (cash interest rate)
log_tagesgeld_return <- log(1 + tagesgeld_return) / 252  # Daily log return

# Step 3: Combine log returns of ETFs with the log return of Tagesgeld
# Merge Tagesgeld with log prices
log_combined <- merge(log_tagesgeld_return, log_prices)

# Step 4: Calculate the portfolio log returns by applying the weights
portfolio_log_returns <- as.matrix(log_combined) %*% c(tagesgeld_weight, weights)
rm(prices, log_prices, log_combined)

# Step 5: Implement the simulation function
initial_wealth <- 100
withdrawal_start <- 255  # Withdrawals start after 1 year
withdrawal_duration <- 255 * 1.5  # 1.5 years
withdrawl_intervalls <- round(90 /365*255,0) # Every 90 days
withdrawl_amount <- 100

simulation <- function(returns, initial_wealth, withdrawal_start, withdrawal_duration, withdrawl_intervalls, withdrawl_amount) {
  # Initialize wealth vector
  wealth <- numeric(length(returns))
  wealth[1] <- initial_wealth
  
  # Create a sequence of withdrawal days (e.g., every 90 days starting at withdrawal_start), exclude last one as one too many
  withdrawal_days <- seq(from = withdrawal_start, 
                         to = (withdrawal_start + withdrawal_duration), 
                         by = withdrawl_intervalls)
  withdrawal_days <- withdrawal_days[-(length(withdrawal_days))]
  
  # Calculate the withdrawal amount for each interval
  interval_withdrawal_amount <- withdrawl_amount / length(withdrawal_days)
  
  # Loop over each return period
  for (i in 2:length(returns)) {
    # Apply log return to grow wealth
    wealth[i] <- wealth[i - 1] * (1 + returns[i])
    
    # Check if today is a withdrawal day and if we are in the withdrawal period
    if (i %in% withdrawal_days) {
      # Apply the interval withdrawal
      wealth[i] <- wealth[i] - interval_withdrawal_amount
    }
    
    # Ensure wealth doesn't go below zero
    if (!is.na(wealth[i]) && wealth[i] < 0) {
      wealth[i] <- 0
      break
    }
  }
  
  return(wealth)
}

roll_simulation <- rollapply(
  data = portfolio_log_returns,
  width = withdrawal_start + withdrawal_duration,
  FUN = function(returns) {
    simulation(
      returns,
      initial_wealth,
      withdrawal_start,
      withdrawal_duration,
      withdrawl_intervalls,
      withdrawl_amount
    )
  },
  by.column = FALSE, # Treat each rolling window as a whole vector, not column-wise
  partial = FALSE,    # Doens't allow for incomplete windows at the beginning or end
  align = "right"    # Align rolling windows to the right, so wealth simulation aligns with the rightmost date in each window
)

# idk why but the resulting matrix is transposed. we fix this simply but not elegantly as I have better things to do
roll_simulation <- t(roll_simulation)

# Generate the Cash Portfolio with explicit withdrawal logic
cash_portfolio <- numeric(withdrawal_start + withdrawal_duration)
cash_portfolio[1] <- initial_wealth

# Create a sequence of withdrawal days for the cash portfolio
cash_withdrawal_days <- seq(from = withdrawal_start, 
                            to = (withdrawal_start + withdrawal_duration), 
                            by = withdrawl_intervalls)
cash_withdrawal_days <- cash_withdrawal_days[-(length(cash_withdrawal_days))]

# Calculate the total number of withdrawals and the interval withdrawal amount
total_cash_withdrawals <- length(cash_withdrawal_days)
interval_withdrawal_amount <- withdrawl_amount / total_cash_withdrawals

# Loop through each day to simulate the cash portfolio's growth and withdrawals
for (i in 2:(withdrawal_start + withdrawal_duration)) {
  # Apply the daily interest rate to grow the wealth
  cash_portfolio[i] <- cash_portfolio[i - 1] * (1 + log_tagesgeld_return)
  
  # Check if today is a withdrawal day
  if (i %in% cash_withdrawal_days) {
    # Apply the interval withdrawal
    cash_portfolio[i] <- cash_portfolio[i] - interval_withdrawal_amount
  }
  
  # Ensure wealth doesn't go below zero
  if (!is.na(cash_portfolio[i]) && cash_portfolio[i] < 0) {
    cash_portfolio[i] <- 0
    break
  }
}

# Define threshold days before the end to define bankruptcy
threshold <- 62

# Plot some results from the roll simulation
plot(roll_simulation[, 1], type = "l", main = "Portfolio Simulation 1")
plot(roll_simulation[, 40], type = "l", main = "Portfolio Simulation 40")
plot(roll_simulation[, 255], type = "l", main = "Portfolio Simulation 255")
plot(roll_simulation[, 366], type = "l", main = "Portfolio Simulation 366")
plot(roll_simulation[, 998], type = "l", main = "Portfolio Simulation 998")
# Plot a random simulation
plot(roll_simulation[, floor(runif(1, min = 1, max = 998))], type = "l", main = "Random Portfolio Simulation")

# Check how often we go bankrupt by defining bankruptcy as wealth going to 0 before 'threshold' days
bankrupt_at_threshold <- roll_simulation[nrow(roll_simulation) - threshold, ] == 0
num_bankrupt <- sum(bankrupt_at_threshold)
ratio_bankrupt <- num_bankrupt / ncol(roll_simulation)

# Plot cash portfolio over time
plot(cash_portfolio, type = "l", main = "Cash Portfolio Performance", xlab = "Time", ylab = "Wealth")

# Extract final wealth at 'threshold' days before the end of the portfolio lifecycle from the simulation
final_portfolio_wealth <- roll_simulation[nrow(roll_simulation) - threshold, ]

# Compute key statistics for final portfolio wealth
mean_wealth <- mean(final_portfolio_wealth)
median_wealth <- median(final_portfolio_wealth)
min_wealth <- min(final_portfolio_wealth)
max_wealth <- max(final_portfolio_wealth)

# Compare with final value of the cash portfolio
final_cash_wealth <- cash_portfolio[length(cash_portfolio)]

# Calculate how many portfolios end up with more wealth than the cash portfolio
better_than_cash <- sum(final_portfolio_wealth > final_cash_wealth)
better_than_cash_ratio <- better_than_cash / length(final_portfolio_wealth)

# Plot the distribution of final wealth values
hist(final_portfolio_wealth, breaks = 30, 
     main = "Distribution of Final Portfolio Wealth (at threshold)", 
     xlab = "Final Wealth", 
     col = "skyblue", border = "white")

# Add vertical lines for the mean, median, and cash portfolio
abline(v = mean_wealth, col = "red", lwd = 2, lty = 2)  # Mean
abline(v = median_wealth, col = "blue", lwd = 2, lty = 2)  # Median
abline(v = final_cash_wealth, col = "green", lwd = 2, lty = 2)  # Cash Portfolio
legend("topright", legend = c("Mean Wealth", "Median Wealth", "Cash Portfolio"), 
       col = c("red", "blue", "green"), lty = 2, lwd = 2)

# Print summary insights
cat("### Portfolio Simulation Summary ###\n")
cat("Total number of simulations:", length(final_portfolio_wealth), "\n")
cat("Mean final wealth (at threshold): $", round(mean_wealth, 2), "\n")
cat("Median final wealth (at threshold): $", round(median_wealth, 2), "\n")
cat("Minimum final wealth (at threshold): $", round(min_wealth, 2), "\n")
cat("Maximum final wealth (at threshold): $", round(max_wealth, 2), "\n")
cat("Number of bankrupt portfolios (wealth = 0 at threshold days before end):", num_bankrupt, "\n")
cat("Ratio of bankrupt portfolios: ", round(ratio_bankrupt * 100, 2), "%\n")
cat("Final cash portfolio value: $", round(final_cash_wealth, 2), "\n")
cat("Portfolios with better wealth than cash portfolio:", better_than_cash, "\n")
cat("Ratio of portfolios better than cash portfolio: ", round(better_than_cash_ratio * 100, 2), "%\n")

# Clean up!
rm(tagesgeld_return, tagesgeld_weight, log_tagesgeld_return, 
   weights, portfolio_log_returns, initial_wealth, withdrawal_start,
   withdrawal_duration, withdrawl_intervalls, withdrawl_amount, 
   cash_portfolio, cash_withdrawal_days, 
   total_cash_withdrawals, interval_withdrawal_amount, threshold, 
   bankrupt_at_threshold, num_bankrupt, 
   mean_wealth, median_wealth, min_wealth, max_wealth)
