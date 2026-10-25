# =============================================================================
# 05_models.R – DeliveryPulse: Train/Test Split, Logistic Regression & Naive Bayes
# =============================================================================

# Set user library path
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

library(caTools)
library(e1071)

# Ensure models directory exists
if (!dir.exists("models")) {
  dir.create("models", recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 1. Load cleaned dataset & enforce explicit factor levels
# -----------------------------------------------------------------------------
csv_path <- if (file.exists("data/delivery_clean.csv")) {
  "data/delivery_clean.csv"
} else {
  "data/cleaned/delivery_clean.csv"
}
clean_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# Explicitly define target factor: "OnTime" = 0 (reference), "Late" = 1 (event)
clean_data$LateF <- factor(clean_data$LateF, levels = c("OnTime", "Late"))

# Categorical columns as factors
clean_data$Weatherconditions    <- factor(clean_data$Weatherconditions)
clean_data$Road_traffic_density <- factor(clean_data$Road_traffic_density)
clean_data$Type_of_vehicle      <- factor(clean_data$Type_of_vehicle)
clean_data$Festival             <- factor(clean_data$Festival)
clean_data$City                 <- factor(clean_data$City)
clean_data$multiple_deliveries  <- factor(clean_data$multiple_deliveries)

cat("============================================================\n")
cat("DELIVERYPULSE: STATISTICAL LEARNING & CLASSIFICATION MODELS\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Stratified Train / Test Split (70% Train, 30% Test)
#    - caTools::sample.split preserves the ~30.6% late class proportion in both splits
#    - set.seed(2026) ensures strict reproducibility across runs
# -----------------------------------------------------------------------------
set.seed(2026)
split_mask <- sample.split(clean_data$LateF, SplitRatio = 0.70)
train <- clean_data[split_mask, ]
test  <- clean_data[!split_mask, ]

cat("------------------------------------------------------------\n")
cat("1. STRATIFIED TRAIN / TEST SPLIT (70 / 30)\n")
cat("------------------------------------------------------------\n")
cat(sprintf("Training set count: %d rows (%.1f%% of total)\n", nrow(train), nrow(train) / nrow(clean_data) * 100))
cat(sprintf("Test set count:     %d rows (%.1f%% of total)\n\n", nrow(test), nrow(test) / nrow(clean_data) * 100))

train_tab <- table(train$LateF)
test_tab  <- table(test$LateF)

cat("Class breakdown in Training set:\n")
print(train_tab)
cat(sprintf("Training Late Proportion: %.4f (%.2f%%)\n\n", prop.table(train_tab)["Late"], prop.table(train_tab)["Late"] * 100))

cat("Class breakdown in Test set:\n")
print(test_tab)
cat(sprintf("Test Late Proportion:     %.4f (%.2f%%)\n\n", prop.table(test_tab)["Late"], prop.table(test_tab)["Late"] * 100))

# -----------------------------------------------------------------------------
# 3. Fit Logistic Regression (GLM Binomial with Logit Link)
#    - log(p / (1 - p)) = beta_0 + beta_1*X_1 + ... + beta_k*X_k
#    - Solved via Iteratively Reweighted Least Squares (IRLS)
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("2. LOGISTIC REGRESSION MODEL (glm, family = binomial)\n")
cat("------------------------------------------------------------\n")

model_formula <- LateF ~ distance_km + Delivery_person_Age + Delivery_person_Ratings +
  Road_traffic_density + Weatherconditions + Type_of_vehicle + Festival +
  multiple_deliveries + City

logit_model <- glm(model_formula, data = train, family = binomial)

# Print standard GLM summary
cat("GLM Summary (Coefficients & Significance):\n")
print(summary(logit_model))
cat("\n")

# Compute Exponentiated Odds Ratios and 95% Wald Confidence Intervals
# Wald CI: beta_j +/- 1.96 * SE(beta_j) -> exp(Wald CI)
coef_summary <- summary(logit_model)$coefficients
wald_ci <- confint.default(logit_model, level = 0.95)

or_table <- data.frame(
  Feature         = rownames(coef_summary),
  Estimate_Beta   = round(coef_summary[, "Estimate"], 4),
  Std_Error       = round(coef_summary[, "Std. Error"], 4),
  z_value         = round(coef_summary[, "z value"], 3),
  p_value         = format.pval(coef_summary[, "Pr(>|z|)"], digits = 4, eps = 0.0001),
  Odds_Ratio      = round(exp(coef_summary[, "Estimate"]), 4),
  CI_2.5_Pct      = round(exp(wald_ci[, 1]), 4),
  CI_97.5_Pct     = round(exp(wald_ci[, 2]), 4),
  stringsAsFactors = FALSE
)
rownames(or_table) <- NULL

cat("============================================================\n")
cat("EXPONENTIATED ODDS RATIOS WITH 95% WALD CONFIDENCE INTERVALS\n")
cat("============================================================\n")
print(or_table)
cat("\n")

# -----------------------------------------------------------------------------
# 4. Fit Naive Bayes Classifier
#    - e1071::naiveBayes applies Bayes' Theorem with conditional independence assumption:
#      P(Late | X) proportional to P(Late) * prod(P(X_i | Late))
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("3. NAIVE BAYES CLASSIFIER (e1071::naiveBayes)\n")
cat("------------------------------------------------------------\n")

nb_model <- naiveBayes(model_formula, data = train)
cat("Naive Bayes Model Fit Summary:\n")
cat(sprintf("A-priori class probabilities: OnTime = %.4f, Late = %.4f\n",
            nb_model$apriori["OnTime"] / sum(nb_model$apriori),
            nb_model$apriori["Late"] / sum(nb_model$apriori)))
cat(sprintf("Number of conditioned features: %d\n\n", length(nb_model$tables)))

# -----------------------------------------------------------------------------
# 5. Predictions on Held-Out Test Set (Cutoff = 0.5)
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("4. GENERATING TEST SET PREDICTIONS (CUTOFF = 0.50)\n")
cat("------------------------------------------------------------\n")

# Logistic regression predicted probabilities: P(Late | X)
pred_prob_logit <- predict(logit_model, newdata = test, type = "response")
pred_class_logit <- factor(ifelse(pred_prob_logit >= 0.5, "Late", "OnTime"), levels = c("OnTime", "Late"))

# Naive Bayes predicted probabilities and classes
pred_prob_nb_raw <- predict(nb_model, newdata = test, type = "raw")
pred_prob_nb     <- pred_prob_nb_raw[, "Late"]
pred_class_nb    <- factor(ifelse(pred_prob_nb >= 0.5, "Late", "OnTime"), levels = c("OnTime", "Late"))

cat("Predicted class distribution on Test Set (Cutoff 0.5):\n")
cat("Logistic Regression Confusion Counts:\n")
print(table(Predicted = pred_class_logit, Actual = test$LateF))
cat("\nNaive Bayes Confusion Counts:\n")
print(table(Predicted = pred_class_nb, Actual = test$LateF))
cat("\n")

# -----------------------------------------------------------------------------
# 6. Save Fitted Model Objects and Predictions to models/
# -----------------------------------------------------------------------------
saveRDS(logit_model, "models/logit_model.rds")
saveRDS(nb_model,    "models/nb_model.rds")

# Also bundle test predictions and test dataframe for script 06 & Shiny app
test_pred_bundle <- list(
  test_data         = test,
  pred_prob_logit   = pred_prob_logit,
  pred_class_logit  = pred_class_logit,
  pred_prob_nb      = pred_prob_nb,
  pred_class_nb     = pred_class_nb
)
saveRDS(test_pred_bundle, "models/test_predictions.rds")

cat("------------------------------------------------------------\n")
cat("5. MODEL ARTIFACTS PERSISTENCE\n")
cat("------------------------------------------------------------\n")
cat("Saved: models/logit_model.rds\n")
cat("Saved: models/nb_model.rds\n")
cat("Saved: models/test_predictions.rds\n\n")
cat("============================================================\n")
