# =============================================================================
# 03_hypothesis_tests.R – DeliveryPulse: Hypothesis Testing Suite
# =============================================================================

# Set user library path
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

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
cat("DELIVERYPULSE: HYPOTHESIS TESTING SUITE\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Welch Two-Sample t-tests for Continuous Features
#    - Welch's t-test is used because variances between OnTime and Late groups
#      cannot be assumed equal (heteroscedasticity-robust).
#    - Null Hypothesis (H0): mu_Late - mu_OnTime = 0
#    - Alternative Hypothesis (H1): mu_Late - mu_OnTime != 0
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("1. WELCH TWO-SAMPLE T-TESTS (LATE vs ON-TIME)\n")
cat("------------------------------------------------------------\n")

continuous_vars <- c("distance_km", "Delivery_person_Age", "Delivery_person_Ratings")
t_results_list <- list()

for (var in continuous_vars) {
  # Perform Welch t-test
  test_res <- t.test(clean_data[[var]] ~ clean_data$LateF)
  
  # Ensure accurate group mean attribution
  est_names <- names(test_res$estimate)
  ontime_idx <- grep("OnTime", est_names)
  late_idx   <- grep("Late", est_names)
  
  mean_ontime <- test_res$estimate[ontime_idx]
  mean_late   <- test_res$estimate[late_idx]
  diff_mean   <- mean_late - mean_ontime
  
  t_results_list[[var]] <- data.frame(
    Feature      = var,
    Mean_OnTime  = round(as.numeric(mean_ontime), 4),
    Mean_Late    = round(as.numeric(mean_late), 4),
    Difference_Late_minus_OnTime = round(as.numeric(diff_mean), 4),
    t_Statistic  = round(as.numeric(test_res$statistic), 4),
    df           = round(as.numeric(test_res$parameter), 1),
    p_value      = format.pval(test_res$p.value, digits = 4, eps = 0.0001),
    CI_95_Lower  = round(test_res$conf.int[1], 4),
    CI_95_Upper  = round(test_res$conf.int[2], 4),
    stringsAsFactors = FALSE
  )
}

t_results_df <- do.call(rbind, t_results_list)
rownames(t_results_df) <- NULL
print(t_results_df)
cat("\n")

# -----------------------------------------------------------------------------
# 3. Chi-Square Tests of Independence & Cramer's V for Categorical Features
#    - Chi-Square test tests independence between feature and LateF.
#    - Cramer's V measures the practical strength of association:
#      V = sqrt(chi2 / (n * (min(r, c) - 1)))
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("2. CHI-SQUARE TESTS OF INDEPENDENCE & CRAMER'S V\n")
cat("------------------------------------------------------------\n")

categorical_vars <- c(
  "Weatherconditions",
  "Road_traffic_density",
  "Type_of_order",
  "Type_of_vehicle",
  "Festival",
  "City",
  "multiple_deliveries",
  "Vehicle_condition"
)

n_total <- nrow(clean_data)
chi_list <- list()

for (var in categorical_vars) {
  # Build contingency table against LateF
  tbl <- table(clean_data[[var]], clean_data$LateF)
  
  # Run Pearson Chi-square test without continuity correction
  test_res <- chisq.test(tbl, correct = FALSE)
  
  # Compute Cramer's V effect size
  k_min <- min(dim(tbl))
  cramers_v <- sqrt(as.numeric(test_res$statistic) / (n_total * (k_min - 1)))
  
  chi_list[[var]] <- data.frame(
    Feature      = var,
    Chi_Square   = as.numeric(test_res$statistic),
    df           = as.numeric(test_res$parameter),
    p_value_raw  = as.numeric(test_res$p.value),
    Cramers_V    = cramers_v,
    stringsAsFactors = FALSE
  )
}

chi_df <- do.call(rbind, chi_list)
rownames(chi_df) <- NULL

# -----------------------------------------------------------------------------
# 4. Multiple Testing Correction via Benjamini-Hochberg (BH)
#    - Controls the False Discovery Rate (FDR) across 8 concurrent tests
# -----------------------------------------------------------------------------
chi_df$p_value_adj <- p.adjust(chi_df$p_value_raw, method = "BH")

# Sort descending by Cramer's V
chi_df_sorted <- chi_df[order(-chi_df$Cramers_V), ]

# Format nicely for reporting
chi_display <- chi_df_sorted
chi_display$Chi_Square   <- round(chi_display$Chi_Square, 2)
chi_display$Cramers_V    <- round(chi_display$Cramers_V, 4)
chi_display$p_value_raw  <- format.pval(chi_display$p_value_raw, digits = 4, eps = 0.0001)
chi_display$p_value_adj  <- format.pval(chi_display$p_value_adj, digits = 4, eps = 0.0001)

print(chi_display)
cat("\n")

# -----------------------------------------------------------------------------
# 5. Interpretive Summary Sentences
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("3. STATISTICAL SIGNIFICANCE & INTERPRETATION (alpha = 0.05)\n")
cat("------------------------------------------------------------\n")

for (i in 1:nrow(chi_df_sorted)) {
  feature_name <- chi_df_sorted$Feature[i]
  p_adj        <- chi_df_sorted$p_value_adj[i]
  v_val        <- chi_df_sorted$Cramers_V[i]
  
  if (p_adj < 0.05) {
    cat(sprintf("[SIGNIFICANT]   %-22s is statistically significantly associated with delivery delay (adj p = %s, Cramer's V = %.4f).\n",
                feature_name, format.pval(p_adj, digits = 4, eps = 0.0001), v_val))
  } else {
    cat(sprintf("[NOT SIGNIFICANT] %-20s is NOT statistically significantly associated with delivery delay (adj p = %s, Cramer's V = %.4f).\n",
                feature_name, format.pval(p_adj, digits = 4, eps = 0.0001), v_val))
  }
}

cat("\nWelch t-test interpretations:\n")
for (var in continuous_vars) {
  row <- t_results_df[t_results_df$Feature == var, ]
  p_val <- as.numeric(gsub("< ", "", row$p_value))
  cat(sprintf("[SIGNIFICANT]   %-22s differs significantly between OnTime and Late orders (t = %.2f, p %s, 95%% CI: [%.3f, %.3f]).\n",
              row$Feature, row$t_Statistic, row$p_value, row$CI_95_Lower, row$CI_95_Upper))
}
cat("\n============================================================\n")
