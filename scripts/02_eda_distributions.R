# =============================================================================
# 02_eda_distributions.R – DeliveryPulse: Exploratory Analysis & Fitting
# =============================================================================

# Set user library path
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

library(ggplot2)
library(fitdistrplus)

# Ensure plots directory exists
if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 1. Load cleaned data
# -----------------------------------------------------------------------------
# Check both potential locations for robustness
csv_path <- if (file.exists("data/delivery_clean.csv")) {
  "data/delivery_clean.csv"
} else {
  "data/cleaned/delivery_clean.csv"
}
clean_data <- read.csv(csv_path, stringsAsFactors = TRUE)

cat("============================================================\n")
cat("DELIVERYPULSE: EXPLORATORY DATA ANALYSIS & DISTRIBUTIONS\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Probability of Late Delivery & Confidence Intervals
#    - binom.test(): Clopper-Pearson exact confidence interval for proportion
#    - prop.test(correct = FALSE): Wilson score interval without continuity correction
# -----------------------------------------------------------------------------
n_total <- nrow(clean_data)
n_late  <- sum(clean_data$Late == 1)
p_hat   <- n_late / n_total

cat(sprintf("Total deliveries: %d\n", n_total))
cat(sprintf("Late deliveries: %d\n", n_late))
cat(sprintf("Sample late rate (p_hat): %.4f (%.2f%%)\n\n", p_hat, p_hat * 100))

# Clopper-Pearson exact confidence interval
# Used because it guarantees coverage probability of at least 1 - alpha
exact_ci <- binom.test(n_late, n_total, conf.level = 0.95)
cat("Exact 95% Confidence Interval (Clopper-Pearson via binom.test):\n")
cat(sprintf("  Estimate: %.4f\n", exact_ci$estimate))
cat(sprintf("  95%% CI:   [%.4f, %.4f]\n\n", exact_ci$conf.int[1], exact_ci$conf.int[2]))

# Wilson score confidence interval
# Asymptotically optimal and avoids over-conservatism of exact intervals
wilson_ci <- prop.test(n_late, n_total, conf.level = 0.95, correct = FALSE)
cat("Wilson 95% Confidence Interval (via prop.test, correct = FALSE):\n")
cat(sprintf("  Estimate: %.4f\n", wilson_ci$estimate))
cat(sprintf("  95%% CI:   [%.4f, %.4f]\n\n", wilson_ci$conf.int[1], wilson_ci$conf.int[2]))

# -----------------------------------------------------------------------------
# 3. Fit Continuous Probability Distributions to Delivery Time
#    - Gamma and Lognormal distributions are standard right-skewed positive models
#    - Maximum Likelihood Estimation via fitdistrplus::fitdist()
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("DISTRIBUTION FITTING FOR Time_taken_min\n")
cat("------------------------------------------------------------\n")

# Fit Gamma distribution (shape-rate parameterization)
fit_gamma <- fitdist(clean_data$Time_taken_min, "gamma", method = "mle")
# Fit Lognormal distribution (meanlog-sdlog parameterization)
fit_lnorm <- fitdist(clean_data$Time_taken_min, "lnorm", method = "mle")

cat("Gamma Distribution Fit:\n")
print(summary(fit_gamma))
cat("\nLognormal Distribution Fit:\n")
print(summary(fit_lnorm))

cat(sprintf("\nAIC Comparison:\n  Gamma AIC:     %.2f\n  Lognormal AIC: %.2f\n",
            fit_gamma$aic, fit_lnorm$aic))
if (fit_gamma$aic < fit_lnorm$aic) {
  cat("  -> Gamma distribution provides the superior parametric fit (lower AIC).\n\n")
} else {
  cat("  -> Lognormal distribution provides the superior parametric fit (lower AIC).\n\n")
}

# Save comparison plots (Density, Q-Q, CDF plots)
png("plots/01_time_taken_fit.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2))
denscomp(list(fit_gamma, fit_lnorm), legendtext = c("Gamma", "Lognormal"),
         main = "Empirical vs Fitted Density")
qqcomp(list(fit_gamma, fit_lnorm), legendtext = c("Gamma", "Lognormal"),
       main = "Q-Q Plot")
cdfcomp(list(fit_gamma, fit_lnorm), legendtext = c("Gamma", "Lognormal"),
        main = "Empirical vs Fitted CDF")
ppcomp(list(fit_gamma, fit_lnorm), legendtext = c("Gamma", "Lognormal"),
       main = "P-P Plot")
par(mfrow = c(1, 1))
dev.off()
cat("Saved: plots/01_time_taken_fit.png\n\n")

# -----------------------------------------------------------------------------
# 4. Binomial Batch Risk Probability
#    - Random batch size n = 50 orders
#    - At least 40% late means k >= 20 late orders (out of 50)
#    - P(X >= 20) = 1 - P(X <= 19) = pbinom(19, size=50, prob=p_hat, lower.tail=FALSE)
# -----------------------------------------------------------------------------
batch_n <- 50
threshold_pct <- 0.40
k_threshold <- ceiling(batch_n * threshold_pct) # 20 deliveries

# pbinom with lower.tail = FALSE calculates P(X > k - 1) = P(X >= k)
prob_batch_risk <- pbinom(k_threshold - 1, size = batch_n, prob = p_hat, lower.tail = FALSE)

cat("------------------------------------------------------------\n")
cat("BATCH RISK SIMULATION (BINOMIAL PROBABILITY)\n")
cat("------------------------------------------------------------\n")
cat(sprintf("Batch size: %d deliveries\n", batch_n))
cat(sprintf("Probability of each delivery being late: p = %.4f\n", p_hat))
cat(sprintf("Probability that >= 40%% (>= %d orders) are late: %.6f (%.4f%%)\n\n",
            k_threshold, prob_batch_risk, prob_batch_risk * 100))

# -----------------------------------------------------------------------------
# 5. Visualizations with ggplot2
# -----------------------------------------------------------------------------
theme_custom <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.position = "bottom")

# 5.1 Plot 02: Late vs OnTime counts
p2 <- ggplot(clean_data, aes(x = LateF, fill = LateF)) +
  geom_bar(width = 0.6, color = "black", alpha = 0.85) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("OnTime" = "#2b8cbe", "Late" = "#e41a1c")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Delivery Punctuality Class Distribution",
       subtitle = sprintf("Total: %d | On-Time: %d (%.1f%%) | Late: %d (%.1f%%)",
                          n_total, n_total - n_late, (1 - p_hat)*100, n_late, p_hat*100),
       x = "Delivery Outcome", y = "Number of Orders", fill = "Outcome") +
  theme_custom

ggsave("plots/02_late_distribution.png", plot = p2, width = 7, height = 5, dpi = 300)
cat("Saved: plots/02_late_distribution.png\n")

# 5.2 Plot 03: Boxplot of distance_km by LateF
p3 <- ggplot(clean_data, aes(x = LateF, y = distance_km, fill = LateF)) +
  geom_boxplot(width = 0.5, outlier.size = 0.8, alpha = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow") +
  scale_fill_manual(values = c("OnTime" = "#2b8cbe", "Late" = "#e41a1c")) +
  labs(title = "Delivery Distance Distribution by Outcome",
       subtitle = "Yellow diamond indicates mean distance (km)",
       x = "Delivery Outcome", y = "Distance (km)", fill = "Outcome") +
  theme_custom

ggsave("plots/03_distance_by_late.png", plot = p3, width = 7, height = 5, dpi = 300)
cat("Saved: plots/03_distance_by_late.png\n")

# 5.3 Plot 04: Boxplot of Delivery_person_Ratings by LateF
p4 <- ggplot(clean_data, aes(x = LateF, y = Delivery_person_Ratings, fill = LateF)) +
  geom_boxplot(width = 0.5, outlier.size = 0.8, alpha = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow") +
  scale_fill_manual(values = c("OnTime" = "#2b8cbe", "Late" = "#e41a1c")) +
  labs(title = "Rider Ratings Distribution by Outcome",
       subtitle = "Yellow diamond indicates mean rating",
       x = "Delivery Outcome", y = "Delivery Person Rating (1-5)", fill = "Outcome") +
  theme_custom

ggsave("plots/04_ratings_by_late.png", plot = p4, width = 7, height = 5, dpi = 300)
cat("Saved: plots/04_ratings_by_late.png\n")

# 5.4 Plot 05: Late Rate by Road Traffic Density
traffic_summary <- aggregate(Late ~ Road_traffic_density, data = clean_data, FUN = function(x) c(rate = mean(x), count = length(x)))
traffic_df <- data.frame(
  Traffic = traffic_summary$Road_traffic_density,
  LateRate = traffic_summary$Late[, "rate"],
  Total = traffic_summary$Late[, "count"]
)

p5 <- ggplot(traffic_df, aes(x = reorder(Traffic, LateRate), y = LateRate, fill = LateRate)) +
  geom_col(width = 0.6, color = "black", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", LateRate * 100, Total)),
            vjust = -0.2, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
  labs(title = "Late Delivery Rate across Road Traffic Conditions",
       subtitle = "Proportion of orders delayed (>30 min) by traffic density",
       x = "Road Traffic Density", y = "Late Delivery Rate (%)", fill = "Late Rate") +
  theme_custom

ggsave("plots/05_late_by_traffic.png", plot = p5, width = 7, height = 5, dpi = 300)
cat("Saved: plots/05_late_by_traffic.png\n")

# 5.5 Plot 06: Late Rate by Weather Conditions
weather_summary <- aggregate(Late ~ Weatherconditions, data = clean_data, FUN = function(x) c(rate = mean(x), count = length(x)))
weather_df <- data.frame(
  Weather = weather_summary$Weatherconditions,
  LateRate = weather_summary$Late[, "rate"],
  Total = weather_summary$Late[, "count"]
)

p6 <- ggplot(weather_df, aes(x = reorder(Weather, LateRate), y = LateRate, fill = LateRate)) +
  geom_col(width = 0.6, color = "black", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", LateRate * 100, Total)),
            vjust = -0.2, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_gradient(low = "#e0f3f8", high = "#08519c") +
  labs(title = "Late Delivery Rate across Weather Conditions",
       subtitle = "Proportion of orders delayed (>30 min) by weather category",
       x = "Weather Condition", y = "Late Delivery Rate (%)", fill = "Late Rate") +
  theme_custom

ggsave("plots/06_late_by_weather.png", plot = p6, width = 8, height = 5, dpi = 300)
cat("Saved: plots/06_late_by_weather.png\n")

cat("\nAll EDA and distribution fitting plots generated successfully in plots/\n")
