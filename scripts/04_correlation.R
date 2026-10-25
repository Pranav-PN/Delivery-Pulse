# =============================================================================
# 04_correlation.R – DeliveryPulse: Correlation Analysis
# =============================================================================

# Set user library path
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

library(ggplot2)

# Ensure plots directory exists
if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 1. Load cleaned dataset
# -----------------------------------------------------------------------------
csv_path <- if (file.exists("data/delivery_clean.csv")) {
  "data/delivery_clean.csv"
} else {
  "data/cleaned/delivery_clean.csv"
}
clean_data <- read.csv(csv_path, stringsAsFactors = TRUE)

cat("============================================================\n")
cat("DELIVERYPULSE: CORRELATION ANALYSIS\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Pearson Correlation and Hypothesis Tests
#    - cor() and cor.test() test H0: rho = 0 vs H1: rho != 0
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("1. BIVARIATE CORRELATIONS WITH Time_taken_min\n")
cat("------------------------------------------------------------\n")

# 2.1 distance_km vs Time_taken_min
# Tests whether geographic transit distance linearly scales with duration
test_dist <- cor.test(clean_data$distance_km, clean_data$Time_taken_min, method = "pearson")
cat("Pearson Correlation: distance_km vs Time_taken_min:\n")
cat(sprintf("  Correlation coefficient (r): %.4f\n", test_dist$estimate))
cat(sprintf("  t-statistic:                %.4f (df = %d)\n", test_dist$statistic, test_dist$parameter))
cat(sprintf("  p-value:                    %s\n", format.pval(test_dist$p.value, digits = 4, eps = 0.0001)))
cat(sprintf("  95%% Confidence Interval:    [%.4f, %.4f]\n\n", test_dist$conf.int[1], test_dist$conf.int[2]))

# 2.2 Delivery_person_Ratings vs Time_taken_min
# Tests whether higher-rated riders execute deliveries faster
test_rate <- cor.test(clean_data$Delivery_person_Ratings, clean_data$Time_taken_min, method = "pearson")
cat("Pearson Correlation: Delivery_person_Ratings vs Time_taken_min:\n")
cat(sprintf("  Correlation coefficient (r): %.4f\n", test_rate$estimate))
cat(sprintf("  t-statistic:                %.4f (df = %d)\n", test_rate$statistic, test_rate$parameter))
cat(sprintf("  p-value:                    %s\n", format.pval(test_rate$p.value, digits = 4, eps = 0.0001)))
cat(sprintf("  95%% Confidence Interval:    [%.4f, %.4f]\n\n", test_rate$conf.int[1], test_rate$conf.int[2]))

# -----------------------------------------------------------------------------
# 3. Point-Biserial Correlation: distance_km vs Late (Binary 0/1 Indicator)
#    - Equivalent to Pearson's r between continuous variable and 0/1 dichotomy
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("2. POINT-BISERIAL CORRELATION: distance_km vs Late (0/1)\n")
cat("------------------------------------------------------------\n")

# Point-biserial correlation test
test_pb <- cor.test(clean_data$distance_km, clean_data$Late, method = "pearson")
cat(sprintf("Point-biserial r_pb:       %.4f\n", test_pb$estimate))
cat(sprintf("t-statistic:               %.4f (df = %d)\n", test_pb$statistic, test_pb$parameter))
cat(sprintf("p-value:                   %s\n", format.pval(test_pb$p.value, digits = 4, eps = 0.0001)))
cat(sprintf("95%% Confidence Interval:   [%.4f, %.4f]\n\n", test_pb$conf.int[1], test_pb$conf.int[2]))

# -----------------------------------------------------------------------------
# 4. Correlation Matrix of Numerical Features
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("3. NUMERICAL FEATURES CORRELATION MATRIX\n")
cat("------------------------------------------------------------\n")

num_vars <- c("distance_km", "Delivery_person_Age", "Delivery_person_Ratings", "Time_taken_min")
cor_matrix <- cor(clean_data[, num_vars], use = "complete.obs", method = "pearson")
cor_matrix_rounded <- round(cor_matrix, 4)
print(cor_matrix_rounded)
cat("\n")

# -----------------------------------------------------------------------------
# 5. Correlation Heatmap with ggplot2
# -----------------------------------------------------------------------------
# Convert correlation matrix into long format for ggplot2
cor_df <- as.data.frame(as.table(cor_matrix_rounded))
names(cor_df) <- c("Var1", "Var2", "Correlation")

# Clean labels for presentation
var_labels <- c(
  "distance_km"             = "Distance (km)",
  "Delivery_person_Age"     = "Rider Age",
  "Delivery_person_Ratings" = "Rider Rating",
  "Time_taken_min"          = "Delivery Time (min)"
)
cor_df$Var1_label <- var_labels[as.character(cor_df$Var1)]
cor_df$Var2_label <- var_labels[as.character(cor_df$Var2)]

p_heat <- ggplot(cor_df, aes(x = Var1_label, y = Var2_label, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", fontface = "bold", size = 4.5) +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  ) +
  labs(title = "Pearson Correlation Matrix Heatmap",
       subtitle = "Linear associations among operational continuous variables")

ggsave("plots/07_correlation_heatmap.png", plot = p_heat, width = 7.5, height = 6, dpi = 300)
cat("Saved: plots/07_correlation_heatmap.png\n\n")

cat("============================================================\n")
