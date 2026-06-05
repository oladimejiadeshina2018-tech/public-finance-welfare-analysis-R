# UNU-WIDER/AU Winter School 2026: Tax Progressivity & Redistribution
# Author: [OLADIMEJI ADESHINA] | Date: [JUNE 5 2026]

rm(list = ls())
packages <- c("ineq", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
library(ineq); library(ggplot2)
set.seed(2026)

# Simulate household data (n=10,000)
n <- 10000
household_data <- data.frame(
  household_id = 1:n,
  market_income = round(rlnorm(n, meanlog = 10.5, sdlog = 0.9), 0),
  transfers = round(rgamma(n, shape = 2, scale = 400) * 
                    (1 - (rlnorm(n, 10.5, 0.9) / max(rlnorm(n, 10.5, 0.9)))), 0),
  taxes = round(0.15 * rlnorm(n, 10.5, 0.9) * runif(n, 0.7, 1.3), 0)
)
household_data$transfers <- pmax(household_data$transfers, 0)
household_data$taxes <- pmax(household_data$taxes, 0)
household_data$disposable_income <- household_data$market_income + 
                                     household_data$transfers - household_data$taxes

# Income deciles
household_data$income_decile <- ntile(household_data$market_income, 10)

# Redistribution statistics by decile
redistribution_table <- aggregate(
  cbind(market_income, disposable_income, transfers, taxes) ~ income_decile,
  data = household_data, FUN = mean
)
redistribution_table$percent_gain <- 
  (redistribution_table$disposable_income - redistribution_table$market_income) /
  redistribution_table$market_income * 100
redistribution_table$net_benefit <- 
  redistribution_table$transfers - redistribution_table$taxes

# Gini coefficients
gini_market <- ineq(household_data$market_income, type = "Gini")
gini_disposable <- ineq(household_data$disposable_income, type = "Gini")
redistribution_effect <- gini_market - gini_disposable

# Kakwani progressivity index
tax_shares <- aggregate(taxes ~ income_decile, data = household_data, FUN = sum)
tax_shares$share <- tax_shares$taxes / sum(tax_shares$taxes)
income_shares <- aggregate(market_income ~ income_decile, data = household_data, FUN = sum)
income_shares$share <- income_shares$market_income / sum(income_shares$market_income)
kakwani <- sum(tax_shares$share * (1:10)/10) - sum(income_shares$share * (1:10)/10)

# Results
cat("\n", rep("=", 60), "\nPUBLIC FINANCE ANALYSIS RESULTS\n", rep("=", 60), "\n\n")
cat(sprintf("Gini (pre-tax): %.4f\n", gini_market))
cat(sprintf("Gini (post-tax): %.4f\n", gini_disposable))
cat(sprintf("Redistribution effect: %.4f (%.1f%% reduction)\n", 
    redistribution_effect, redistribution_effect/gini_market*100))
cat(sprintf("Kakwani index: %.4f (%s)\n", kakwani, ifelse(kakwani>0,"Progressive","Regressive")))

cat("\nRedistribution by decile:\n")
cat("Decile | Market Inc | Disposable Inc | Net Benefit | % Gain\n")
for(i in 1:10) cat(sprintf("  %2d   | %10.0f | %13.0f | %11.0f | %5.1f%%\n",
    i, redistribution_table$market_income[i],
    redistribution_table$disposable_income[i],
    redistribution_table$net_benefit[i],
    redistribution_table$percent_gain[i]))

# Visualization
p <- ggplot(data.frame(decile=factor(1:10), gain=redistribution_table$percent_gain), 
       aes(x=decile, y=gain)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Redistributive Impact by Income Decile",
       x="Income Decile (1=poorest, 10=richest)", y="Percentage Gain (%)") +
  theme_minimal()
ggsave("redistribution_plot.png", p, width=8, height=6); print(p)
