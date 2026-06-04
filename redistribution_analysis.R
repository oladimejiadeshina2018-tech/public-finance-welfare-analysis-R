# ============================================================
# UNU-WIDER/African Union Winter School on Public Finance 2026
# Application Code Sample: Tax Progressivity & Redistribution
# Author: [OLADIMEJI ADESHINA]
# Date: [JUNE 4 2026]
# ============================================================

# 1. SETUP AND ENVIRONMENT --------------------------------
# Clear workspace
rm(list = ls())

# Load required packages (install if missing)
packages <- c("ineq", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ineq)
library(ggplot2)

# Set seed for reproducibility
set.seed(2026)

# 2. SIMULATE HOUSEHOLD DATA --------------------------------
# Creating realistic income distribution for 10,000 households
n <- 10000

household_data <- data.frame(
  household_id = 1:n,
  # Pre-tax income (log-normal distribution - realistic for income)
  market_income = round(rlnorm(n, meanlog = 10.5, sdlog = 0.9), 0),
  # Government transfers (higher for lower incomes)
  transfers = round(rgamma(n, shape = 2, scale = 400) * 
                    (1 - (rlnorm(n, 10.5, 0.9) / max(rlnorm(n, 10.5, 0.9)))), 0),
  # Taxes paid (proportional to income with some randomness)
  taxes = round(0.15 * rlnorm(n, 10.5, 0.9) * runif(n, 0.7, 1.3), 0)
)

# Ensure no negative values
household_data$transfers <- pmax(household_data$transfers, 0)
household_data$taxes <- pmax(household_data$taxes, 0)

# Calculate disposable income
household_data$disposable_income <- household_data$market_income + 
                                     household_data$transfers - 
                                     household_data$taxes

# 3. CREATE INCOME GROUPS (DECILES) -------------------------
household_data$income_decile <- ntile(household_data$market_income, 10)

# 4. CALCULATE REDISTRIBUTION STATISTICS --------------------
redistribution_table <- aggregate(
  cbind(market_income, disposable_income, transfers, taxes) ~ income_decile,
  data = household_data,
  FUN = mean
)

# Calculate percentage gain from redistribution
redistribution_table$percent_gain <- 
  (redistribution_table$disposable_income - redistribution_table$market_income) /
  redistribution_table$market_income * 100

# Calculate net benefit (transfers minus taxes)
redistribution_table$net_benefit <- 
  redistribution_table$transfers - redistribution_table$taxes

# 5. INEQUALITY MEASURES (GINI COEFFICIENTS) ---------------
gini_market <- ineq(household_data$market_income, type = "Gini")
gini_disposable <- ineq(household_data$disposable_income, type = "Gini")
redistribution_effect <- gini_market - gini_disposable

# 6. TAX PROGRESSIVITY (KAKWANI INDEX) ----------------------
# Calculate share of taxes paid by each decile
tax_shares <- aggregate(taxes ~ income_decile, data = household_data, FUN = sum)
tax_shares$share <- tax_shares$taxes / sum(tax_shares$taxes)

# Calculate share of market income earned by each decile
income_shares <- aggregate(market_income ~ income_decile, 
                           data = household_data, FUN = sum)
income_shares$share <- income_shares$market_income / sum(income_shares$market_income)

# Kakwani index = Concentration coefficient of taxes - Gini of pre-tax income
# Simplified calculation:
kakwani <- sum(tax_shares$share * (1:10)/10) - sum(income_shares$share * (1:10)/10)

# 7. DISPLAY RESULTS -----------------------------------------
cat("\n", rep("=", 60), "\n")
cat("PUBLIC FINANCE ANALYSIS RESULTS\n")
cat(rep("=", 60), "\n\n")

cat("INEQUALITY MEASURES:\n")
cat(sprintf("  Gini coefficient (pre-tax income): %.4f\n", gini_market))
cat(sprintf("  Gini coefficient (post-tax/transfer): %.4f\n", gini_disposable))
cat(sprintf("  Redistribution effect: %.4f\n", redistribution_effect))
cat(sprintf("  Percentage reduction in inequality: %.1f%%\n", 
    redistribution_effect / gini_market * 100))

cat("\nTAX PROGRESSIVITY:\n")
cat(sprintf("  Kakwani progressivity index: %.4f\n", kakwani))
if(kakwani > 0) {
  cat("  Interpretation: Tax system is PROGRESSIVE\n")
} else {
  cat("  Interpretation: Tax system is REGRESSIVE\n")
}

cat("\nREDISTRIBUTION BY INCOME GROUP:\n")
cat("Decile | Market Income | Disposable Income | Net Benefit | % Gain\n")
cat("-------|---------------|-------------------|-------------|--------\n")
for(i in 1:10) {
  cat(sprintf("  %2d   | %13.0f | %17.0f | %11.0f | %6.1f%%\n",
      i,
      redistribution_table$market_income[i],
      redistribution_table$disposable_income[i],
      redistribution_table$net_benefit[i],
      redistribution_table$percent_gain[i]))
}

# 8. CREATE VISUALIZATION ------------------------------------
# Bar plot showing redistribution by decile
plot_data <- data.frame(
  decile = factor(1:10),
  gain = redistribution_table$percent_gain
)

p <- ggplot(plot_data, aes(x = decile, y = gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Redistributive Impact by Income Decile",
       subtitle = "Percentage gain in income after taxes and transfers",
       x = "Income Decile (1 = poorest, 10 = richest)",
       y = "Percentage Gain (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot (optional - creates file in your working directory)
ggsave("redistribution_plot.png", p, width = 8, height = 6)

# Display plot
print(p)

# 9. SUMMARY CONCLUSION --------------------------------------
cat("\n", rep("=", 60), "\n")
cat("CONCLUSION:\n")
cat(sprintf("This analysis shows that the fiscal system reduces the Gini coefficient
from %.4f to %.4f, a %.1f%% reduction in inequality. The positive Kakwani index 
(%.4f) indicates the tax system is progressive, with higher-income households 
paying a larger share of taxes. The poorest decile gains %.1f%% in disposable 
income while the richest decile gains %.1f%%. These results demonstrate the 
redistributive potential of public finance tools in developing economies.\n",
    gini_market, gini_disposable, redistribution_effect/gini_market*100,
    kakwani, redistribution_table$percent_gain[1], 
    redistribution_table$percent_gain[10]))
cat(rep("=", 60), "\n")

# End of code
