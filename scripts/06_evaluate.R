# =============================================================================
# 06_evaluate.R – DeliveryPulse: Model Evaluation, ROC/AUC & Triage Analysis
# =============================================================================

# Set user library path
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

library(pROC)
library(ggplot2)

# Ensure plots directory exists
if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 1. Load Test Predictions and Data
# -----------------------------------------------------------------------------
bundle <- readRDS("models/test_predictions.rds")
test             <- bundle$test_data
pred_prob_logit  <- bundle$pred_prob_logit
pred_class_logit <- bundle$pred_class_logit
pred_prob_nb     <- bundle$pred_prob_nb
pred_class_nb    <- bundle$pred_class_nb

cat("============================================================\n")
cat("DELIVERYPULSE: MODEL EVALUATION & PERFORMANCE COMPARISON\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Confusion Matrices (Positive Class = "Late")
#    - Uses caret::confusionMatrix if available, otherwise exact base R equivalent
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("1. CONFUSION MATRICES (Positive = 'Late', Cutoff = 0.50)\n")
cat("------------------------------------------------------------\n")

print_cm <- function(pred, actual, model_name, positive = "Late") {
  cat(sprintf("\n=== Confusion Matrix & Statistics: %s ===\n", model_name))
  
  if (requireNamespace("caret", quietly = TRUE)) {
    cm <- caret::confusionMatrix(data = pred, reference = actual, positive = positive)
    print(cm)
    return(cm)
  } else {
    # Full exact statistical implementation matching caret::confusionMatrix output
    tab <- table(Prediction = pred, Reference = actual)
    print(tab)
    
    # Positive class is "Late", Negative class is "OnTime"
    TP <- tab[positive, positive]
    FP <- tab[positive, setdiff(levels(actual), positive)]
    FN <- tab[setdiff(levels(actual), positive), positive]
    TN <- tab[setdiff(levels(actual), positive), setdiff(levels(actual), positive)]
    total <- sum(tab)
    
    acc <- (TP + TN) / total
    binom_acc <- binom.test(TP + TN, total)
    sens <- TP / (TP + FN)
    spec <- TN / (TN + FP)
    ppv  <- TP / (TP + FP)
    npv  <- TN / (TN + FN)
    prev <- (TP + FN) / total
    bal_acc <- (sens + spec) / 2
    
    cat("\n      Accuracy :", round(acc, 4), "\n")
    cat("        95% CI : (", round(binom_acc$conf.int[1], 4), ",", round(binom_acc$conf.int[2], 4), ")\n")
    cat("   Sensitivity :", round(sens, 4), "\n")
    cat("   Specificity :", round(spec, 4), "\n")
    cat("Pos Pred Value :", round(ppv, 4), "\n")
    cat("Neg Pred Value :", round(npv, 4), "\n")
    cat("    Prevalence :", round(prev, 4), "\n")
    cat("Balanced Acc   :", round(bal_acc, 4), "\n")
    cat("Positive Class :", positive, "\n\n")
    
    return(list(
      overall = c(Accuracy = acc, AccuracyLower = binom_acc$conf.int[1], AccuracyUpper = binom_acc$conf.int[2]),
      byClass = c(Sensitivity = sens, Specificity = spec, `Pos Pred Value` = ppv, `Neg Pred Value` = npv, `Balanced Accuracy` = bal_acc)
    ))
  }
}

cm_logit <- print_cm(pred_class_logit, test$LateF, "Logistic Regression", positive = "Late")
cm_nb    <- print_cm(pred_class_nb,    test$LateF, "Naive Bayes Classifier", positive = "Late")

# -----------------------------------------------------------------------------
# 3. Receiver Operating Characteristic (ROC) & AUC with pROC
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("2. ROC & AUC ANALYSIS (pROC::roc / auc / ci.auc)\n")
cat("------------------------------------------------------------\n")

# Compute ROC curves (direction: OnTime < Late)
roc_logit <- roc(test$LateF, pred_prob_logit, levels = c("OnTime", "Late"), direction = "<")
roc_nb    <- roc(test$LateF, pred_prob_nb,    levels = c("OnTime", "Late"), direction = "<")

auc_logit    <- auc(roc_logit)
auc_nb       <- auc(roc_nb)
ci_auc_logit <- ci.auc(roc_logit)
ci_auc_nb    <- ci.auc(roc_nb)

cat(sprintf("Logistic Regression AUC: %.4f (95%% CI: [%.4f, %.4f])\n",
            auc_logit, ci_auc_logit[1], ci_auc_logit[3]))
cat(sprintf("Naive Bayes AUC:         %.4f (95%% CI: [%.4f, %.4f])\n\n",
            auc_nb, ci_auc_nb[1], ci_auc_nb[3]))

# Save overlaid ROC curve plot to plots/08_roc_curves.png
png("plots/08_roc_curves.png", width = 800, height = 700, res = 120)
plot(roc_logit, col = "#2166ac", lwd = 3, legacy.axes = TRUE,
     main = "Overlaid ROC Curves on Held-Out Test Set",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")
lines(roc_nb, col = "#b2182b", lwd = 3, lty = 2)
abline(a = 0, b = 1, lty = 3, col = "grey50")
legend("bottomright",
       legend = c(
         sprintf("Logistic Regression (AUC = %.4f)", auc_logit),
         sprintf("Naive Bayes (AUC = %.4f)", auc_nb),
         "Random Chance (AUC = 0.5000)"
       ),
       col = c("#2166ac", "#b2182b", "grey50"),
       lwd = c(3, 3, 1), lty = c(1, 2, 3), bty = "n")
dev.off()
cat("Saved: plots/08_roc_curves.png\n\n")

# -----------------------------------------------------------------------------
# 4. Triage Metric (Top 20% Riskiest Orders Intervention)
#    - Rank test-set orders by predicted probability of late delivery descending
#    - Select top 20%
#    - Compute proportion of all actual late orders captured in this top 20%
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("3. OPERATIONAL DISPATCH TRIAGE METRIC (Top 20% Capacity)\n")
cat("------------------------------------------------------------\n")

triage_rank_df <- data.frame(
  Actual_Late  = as.integer(test$LateF == "Late"),
  Pred_Prob    = pred_prob_logit
)

# Sort descending by predicted risk probability
triage_rank_df <- triage_rank_df[order(-triage_rank_df$Pred_Prob), ]

n_test       <- nrow(triage_rank_df)
top_20_count <- ceiling(0.20 * n_test)
total_actual_late <- sum(triage_rank_df$Actual_Late)

# Late orders in top 20%
top_20_slice <- triage_rank_df[1:top_20_count, ]
captured_late <- sum(top_20_slice$Actual_Late)
triage_share  <- captured_late / total_actual_late

cat(sprintf("Total test-set deliveries:          %d\n", n_test))
cat(sprintf("Top 20%% intervention capacity:     %d orders\n", top_20_count))
cat(sprintf("Total actually late deliveries:     %d\n", total_actual_late))
cat(sprintf("Actually late captured in top 20%%:  %d\n", captured_late))
cat(sprintf("Triage capture rate:                %.4f (%.2f%% of all late deliveries)\n\n",
            triage_share, triage_share * 100))

# -----------------------------------------------------------------------------
# 5. Model Comparison Summary Table
# -----------------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("4. MODEL COMPARISON SUMMARY TABLE (Test Set)\n")
cat("------------------------------------------------------------\n")

# Extract metrics
acc_logit  <- ifelse(is.null(cm_logit$overall), cm_logit$overall["Accuracy"], cm_logit$overall["Accuracy"])
sens_logit <- cm_logit$byClass["Sensitivity"]
spec_logit <- cm_logit$byClass["Specificity"]

acc_nb  <- ifelse(is.null(cm_nb$overall), cm_nb$overall["Accuracy"], cm_nb$overall["Accuracy"])
sens_nb <- cm_nb$byClass["Sensitivity"]
spec_nb <- cm_nb$byClass["Specificity"]

comp_table <- data.frame(
  Model       = c("Logistic Regression", "Naive Bayes Classifier"),
  Accuracy    = round(c(acc_logit, acc_nb), 4),
  Sensitivity = round(c(sens_logit, sens_nb), 4),
  Specificity = round(c(spec_logit, spec_nb), 4),
  AUC         = round(c(as.numeric(auc_logit), as.numeric(auc_nb)), 4),
  stringsAsFactors = FALSE
)
rownames(comp_table) <- NULL
print(comp_table)
cat("\n============================================================\n")
