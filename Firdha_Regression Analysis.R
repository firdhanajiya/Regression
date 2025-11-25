########################################################  
### Regression Analysis                              ###
### PPHA 31002: Stats I                              ###
### Name: Firdha Anisa Najiya Merdeka Sari           ###
### Objective: investigate the relationship between  ### 
### state capacity and the proportion of a country’s ### 
### land area that is under environmental protection ### 
########################################################  



library(tidyverse)
library(broom)
library(ggplot2)

setwd("~/Documents/01 My Coding/")
getwd()     
hw3data <- read.csv("hw3_data.csv", stringsAsFactors = FALSE)

# Quick look
cat("\nFirst 6 rows of data:\n")
print(head(hw3data, 6))


# ----------------------------
# Data exercises using hw3_data.csv
# ----------------------------
# percent_of_protected_land_area, state_capacity_score assumed present
if(!all(c("percent_of_protected_land_area","state_capacity_score") %in% names(hw3data))){
  stop("Expected columns 'percent_of_protected_land_area' and 'state_capacity_score' not found in hw3_data.csv")
}

# Scatterplot
p_scatter <- ggplot(hw3data, aes(x = state_capacity_score, y = percent_of_protected_land_area)) +
  geom_point() +
  labs(title = "Percent protected land vs State capacity (with OLS line)",
       x = "state_capacity_score",
       y = "percent_of_protected_land_area") +
  theme_minimal()
print(p_scatter)
ggsave("q7_scatter_with_ols.png", p_scatter, width = 7, height = 5, dpi = 300)

# OLS regression percent_of_protected_land_area ~ state_capacity_score
model_lin <- lm(percent_of_protected_land_area ~ state_capacity_score, data = hw3data)
cat("\nLinear model summary:\n")
print(summary(model_lin))

p_scatter <- ggplot(hw3data, aes(x = state_capacity_score, y = percent_of_protected_land_area)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Percent protected land vs State capacity (with OLS line)",
       x = "state_capacity_score",
       y = "percent_of_protected_land_area") +
  theme_minimal()
print(p_scatter)
ggsave("q7_scatter_with_ols.png", p_scatter, width = 7, height = 5, dpi = 300)

# Tidy coefficients
coef_lin <- tidy(model_lin)
print(coef_lin)

# prediction at state_capacity_score = 3
newdata <- tibble(state_capacity_score = 3)
pred_3 <- predict(model_lin, newdata = newdata, interval = "prediction")
cat("\nPrediction at state_capacity_score = 3 (prediction interval):\n")
print(pred_3)

# Decile regression
# Create decile factor (1..10), use cut or ntile
hw3data1 <- hw3data %>%
  mutate(capacity_decile = ntile(state_capacity_score, 10)) %>%
  mutate(capacity_decile = factor(capacity_decile, levels = 1:10))

# Decile means table
decile_means <- hw3data1 %>%
  group_by(capacity_decile) %>%
  summarise(n = n(),
            mean_percent_protected = mean(percent_of_protected_land_area, na.rm = TRUE),
            sd_percent_protected = sd(percent_of_protected_land_area, na.rm = TRUE)) %>%
  arrange(as.integer(capacity_decile))

cat("\nDecile means (percent protected) by capacity decile:\n")
print(decile_means)

# Fit decile regression: use decile dummies (decile 1 as reference)
model_dec <- lm(percent_of_protected_land_area ~ capacity_decile, data = hw3data1)
cat("\nDecile regression summary:\n")
print(summary(model_dec))

coef_dec <- tidy(model_dec)
print(coef_dec)

# Compare R-squared
# Linear model: percent_of_protected_land_area ~ state_capacity_score
lm_lin <- lm(percent_of_protected_land_area ~ state_capacity_score, data = hw3data1)
sum_lin <- summary(lm_lin)

# Decile-factor model: create deciles and fit factor regression
hw3data1 <- hw3data1 %>% mutate(capacity_decile = ntile(state_capacity_score, 10))
hw3data1$capacity_decile <- factor(hw3data1$capacity_decile)

lm_dec <- lm(percent_of_protected_land_area ~ capacity_decile, data = hw3data1)
sum_dec <- summary(lm_dec)

# Extract diagnostics
# Linear model diagnostics
r2_lin    <- sum_lin$r.squared
adjr2_lin <- sum_lin$adj.r.squared
rse_lin   <- sum_lin$sigma          # residual standard error
n_lin     <- sum_lin$df[1] + sum_lin$df[2] + 1 # alternative: nrow(hw3)
k_lin     <- length(coef(lm_lin))   # number of parameters including intercept

# Decile model diagnostics
r2_dec    <- sum_dec$r.squared
adjr2_dec <- sum_dec$adj.r.squared
rse_dec   <- sum_dec$sigma
k_dec     <- length(coef(lm_dec))

# Print neatly
cat("Linear model (y ~ state_capacity_score):\n")
cat(sprintf("  n = %d, parameters k = %d\n", nrow(hw3data1), k_lin))
cat(sprintf("  R-squared = %.6f\n  Adj R-squared = %.6f\n  Residual Std. Error (RSE) = %.6f (on %d df)\n\n",
            r2_lin, adjr2_lin, rse_lin, sum_lin$fstatistic["dendf"]))

cat("Decile-factor model (y ~ factor(capacity_decile)):\n")
cat(sprintf("  n = %d, parameters k = %d\n", nrow(hw3data1), k_dec))
cat(sprintf("  R-squared = %.6f\n  Adj R-squared = %.6f\n  Residual Std. Error (RSE) = %.6f (on %d df)\n\n",
            r2_dec, adjr2_dec, rse_dec, sum_dec$fstatistic["dendf"]))

# the raw RSS / SST numbers:
rss_lin <- sum(lm_lin$residuals^2)
rss_dec <- sum(lm_dec$residuals^2)
sst     <- sum((hw3data1$percent_of_protected_land_area - mean(hw3data1$percent_of_protected_land_area))^2)
cat(sprintf("RSS (linear) = %.6f, RSS (decile) = %.6f, SST = %.6f\n", rss_lin, rss_dec, sst))

# Optional: show coefficients for decile model (interpretable)
print(coef(lm_dec))



