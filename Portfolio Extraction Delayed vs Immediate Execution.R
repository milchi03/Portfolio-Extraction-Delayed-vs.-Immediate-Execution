library(quantmod)
library(ggplot2)

# ETFs und Cash interests
tickers <- c("XDEM.L", "VHYL.AS", "HYG", "REET", 
             "WDSC.L", "EMIM.L", "AGEB.MI")

weights <- c(0.207, 0.212, 0.112, 0.095, 0.088, 0.082, 0.106)
tagesgeld_return <- 0.02  # 2% interest p.a.
tagesgeld_weight <- 0.188

# 1. Hole historische Daten von Yahoo Finance
getSymbols(tickers, src = "yahoo", from = "2015-01-01", to = Sys.Date(), auto.assign = TRUE)

# 2. Berechne die monatlichen Renditen für jedes ETF
returns_list <- lapply(tickers, function(ticker) {
  monthly_returns <- monthlyReturn(Cl(get(ticker)))  # Berechne monatliche Renditen
  return(monthly_returns)
})

# 3. Wandle die Liste der Renditen in eine gemeinsame Zeitreihe um
returns_matrix <- do.call(merge, returns_list)

# Bereinige fehlende Werte (NA) aus der Matrix
returns_matrix <- na.omit(returns_matrix)

# 4. Füge die monatliche Rendite des Tagesgeldes hinzu
# Berechne die monatliche Tagesgeldrendite
tagesgeld_monthly_return <- rep(tagesgeld_return / 12, nrow(returns_matrix))
returns_matrix <- cbind(returns_matrix, tagesgeld_monthly_return)
colnames(returns_matrix)[ncol(returns_matrix)] <- "Tagesgeld"

# 5. Berechne die mittleren monatlichen Renditen und die Kovarianzmatrix
mean_returns <- colMeans(returns_matrix)
cov_matrix <- cov(returns_matrix)

# 6. Erstelle ein Gewichtungsvektor für das Portfolio (einschließlich Tagesgeld)
portfolio_weights <- c(tagesgeld_weight, weights)

# 7. Berechne die erwartete Portfoliorendite und Volatilität
portfolio_return <- sum(portfolio_weights * mean_returns)  # Monatsrendite
portfolio_volatility <- sqrt(t(portfolio_weights) %*% cov_matrix %*% portfolio_weights)  # Monatliche Volatilität

## Prognose
# Initiales Vermögen
initial_wealth <- 100

# Anzahl der Monate und Entnahmeplan
months <- 24
withdraw_interval <- 3  # alle 3 Monate
withdraw_per_period <- initial_wealth / (months / withdraw_interval)

# Simulation des Vermögens über 24 Monate
#set.seed(123)  # Für Reproduzierbarkeit
portfolio_value <- numeric(months)
portfolio_value[1] <- initial_wealth

for (i in 2:months) {
  # Simuliere die monatliche Rendite als Normalverteilung
  monthly_return_simulated <- rnorm(1, mean = portfolio_return, sd = portfolio_volatility)
  
  # Aktualisiere den Portfolio-Wert
  portfolio_value[i] <- portfolio_value[i-1] * (1 + monthly_return_simulated)
  
  # Entnahme nach jedem Intervall
  if (i %% withdraw_interval == 0) {
    portfolio_value[i] <- portfolio_value[i] - withdraw_per_period
  }
}

# Portfolio mit 100% Tagesgeld
tagesgeld_value <- numeric(months)
tagesgeld_value[1] <- initial_wealth

for (i in 2:months) {
  # 2% Tagesgeld Zinsen auf Monatsbasis
  tagesgeld_return_monthly <- tagesgeld_return / 12
  tagesgeld_value[i] <- tagesgeld_value[i-1] * (1 + tagesgeld_return_monthly)
  
  # Entnahme nach jedem Intervall
  if (i %% withdraw_interval == 0) {
    tagesgeld_value[i] <- tagesgeld_value[i] - withdraw_per_period
  }
}

# 1. Berechne den erwarteten Portfoliowert (ohne Simulation)
expected_portfolio_value <- numeric(months)
expected_portfolio_value[1] <- initial_wealth

for (i in 2:months) {
  # Erwarteter Portfoliowert (durchschnittliche monatliche Rendite)
  expected_portfolio_value[i] <- expected_portfolio_value[i-1] * (1 + portfolio_return)
  
  # Entnahme nach jedem Intervall
  if (i %% withdraw_interval == 0) {
    expected_portfolio_value[i] <- expected_portfolio_value[i] - withdraw_per_period
  }
}

# 2. Berechne die oberen und unteren Grenzen basierend auf der Volatilität (Standardabweichung)
# Annahme: Die Standardabweichung skaliert über die Zeit mit der Wurzel der Zeit
portfolio_sd_upper <- expected_portfolio_value * exp(portfolio_volatility * sqrt(1:months))
portfolio_sd_lower <- expected_portfolio_value * exp(-portfolio_volatility * sqrt(1:months))

# 3. Plot: Vermögensverlauf des Portfolios und Standardabweichungen im Vergleich zu Tagesgeld
plot(1:months, portfolio_value, type = "l", col = "blue", lwd = 2,
     xlab = "Monate", ylab = "Vermögen", main = "Vermögensverlauf Portfolio vs. Tagesgeld")
lines(1:months, tagesgeld_value, col = "red", lwd = 2)

# Füge die erwarteten Werte und die Standardabweichungen hinzu
lines(1:months, expected_portfolio_value, col = "purple", lwd = 2, lty = 2)  # Erwarteter Wert
lines(1:months, portfolio_sd_upper, col = "green", lty = 2)  # +1 SD
lines(1:months, portfolio_sd_lower, col = "green", lty = 2)  # -1 SD

# Legende für den kombinierten Plot
legend("topright", legend = c("Aktuelles Portfolio", "100% Tagesgeld", "Erwarteter Wert", "+1 SD", "-1 SD"), 
       col = c("blue", "red", "purple", "green", "green"), lwd = 2, lty = c(1, 1, 2, 2, 2))
