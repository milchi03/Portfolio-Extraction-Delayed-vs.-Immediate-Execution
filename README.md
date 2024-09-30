# Portfolio-Extraction-Delayed-vs.-Immediate-Execution
Overview
This R script simulates the value development of a portfolio that includes a combination of exchange-traded funds (ETFs) and a fixed-interest savings account (Tagesgeld) over a period of 24 months. It retrieves historical data from Yahoo Finance for a set of ETFs, calculates monthly returns, and simulates portfolio growth, withdrawals, and volatility.

The script also compares the portfolio’s performance against a 100% savings account investment. Additionally, it estimates expected portfolio value and standard deviation (volatility) bands to visualize potential fluctuations.

Features
ETF and Savings Account Portfolio Simulation:

A portfolio is constructed with seven different ETFs and a fixed portion in a savings account (Tagesgeld).
Historical data for ETFs is downloaded using quantmod from Yahoo Finance, starting from January 1, 2015.
Monthly returns for the ETFs are calculated, and the returns for the savings account are added at a fixed 3% annual rate.
Portfolio Return and Volatility Calculation:

The weighted average monthly return of the portfolio is calculated using the portfolio's ETF and savings account allocations.
The script computes the covariance matrix of ETF returns to determine the portfolio’s volatility.
Withdrawal Simulation:

The portfolio simulation considers periodic withdrawals every three months.
The effect of withdrawals on portfolio value is illustrated alongside portfolio growth.
Savings Account (Tagesgeld) Comparison:

The portfolio's performance is compared to a scenario where the entire investment is placed in a savings account with a fixed 3% annual interest rate.
Expected Value and Volatility Visualization:

The expected portfolio value is calculated based on the average monthly return without simulation.
The script computes the upper and lower bounds (±1 standard deviation) of the portfolio value over time, accounting for volatility.
A plot is generated to visualize the portfolio’s actual performance, expected value, and ±1 standard deviation bands, along with the performance of a 100% savings account.

Required Libraries
quantmod: For downloading historical financial data from Yahoo Finance.
ggplot2: For visualizing the portfolio value and comparing it with other investment options.

Usage Instructions
Install required packages: To run this script, you need to have quantmod and ggplot2 installed. You can install them using:
R
install.packages("quantmod")
install.packages("ggplot2")

Running the Script:
Execute the script in R or RStudio. The code will automatically download historical ETF data from Yahoo Finance and simulate the portfolio's growth.
The script runs for a 24-month period with periodic withdrawals.
Customization:

You can adjust the initial portfolio by changing the ETFs in the tickers variable and modifying the weights.
Modify the savings account interest rate by changing the tagesgeld_return value.
The investment period (default 24 months) can be extended or reduced by changing the months variable.
The withdrawal frequency can be adjusted by changing the withdraw_interval.

Output
Portfolio Value Plot: A plot comparing the following:
Actual portfolio value over 24 months (in blue).
100% savings account value (in red).
Expected portfolio value (in purple).
Upper and lower bounds of portfolio value based on volatility (green dashed lines representing ±1 standard deviation).
The plot provides a visual representation of how the portfolio performs over time, factoring in periodic withdrawals and market volatility.

This script is a simulation tool and does not account for real-life complexities such as transaction costs, taxes, or changes in market conditions. It is intended for educational purposes only.

Variance is used as the risk measure in this simulation, which is a simplified representation of reality. Financial processes do not follow a normal distribution. As noted by Bo Li in "An Explanation for the Distribution Characteristics of Stock Returns" (PKU-WUHAN Institute for Artificial Intelligence, December 5, 2023), stock returns exhibit characteristics that deviate from normality. More advanced risk measures, such as Value at Risk (VaR) or Expected Shortfall, could provide better insights and may be incorporated in future versions of the model.
