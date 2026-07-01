# ==============================================================================
# Step 8: Persist Model Performance Metrics
# Reads the ALREADY-SAVED out-of-fold predictions (cv_predictions_step5.csv) and
# computes AUC, Brier, and a confusion-matrix summary for each model.
# Nothing is retrained — this only reads saved predictions.
#
# Outputs:
#   output/tables/model_metrics.csv   tidy metrics table (one row per model)
#   output/figures/roc_curves.png     combined ROC curve for all three models
# ==============================================================================

# --- Paths ---
project_root  <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data"
processed_dir <- file.path(project_root, "data/processed")
tables_dir    <- file.path(project_root, "output/tables")
figures_dir   <- file.path(project_root, "output/figures")
dir.create(tables_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load saved out-of-fold predictions (do NOT retrain) ---
preds <- read.csv(file.path(processed_dir, "cv_predictions_step5.csv"))
y <- preds$y_true

models <- c(`Elastic Net` = "pred_enet",
            `Random Forest` = "pred_rf",
            `XGBoost` = "pred_xgb")

cat("Loaded", nrow(preds), "out-of-fold predictions.\n")
cat("Outcome: 0 =", sum(y == 0), ", 1 =", sum(y == 1),
    sprintf(" (%.1f%% positive)\n\n", 100 * mean(y)))

# ======================================================================
# Helper functions (base R — no external packages)
# ======================================================================

# Exact AUC via the Mann-Whitney U statistic (ties handled by average ranks)
auc_fun <- function(score, label) {
  n_pos <- sum(label == 1)
  n_neg <- sum(label == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[label == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

# Brier score = mean squared error of predicted probabilities
brier_fun <- function(score, label) mean((score - label)^2)

# ROC points (TPR / FPR) over all distinct score thresholds, descending score
roc_points <- function(score, label) {
  ord <- order(score, decreasing = TRUE)
  s <- score[ord]; l <- label[ord]
  P <- sum(label == 1); N <- sum(label == 0)
  tp <- cumsum(l == 1)
  fp <- cumsum(l == 0)
  # keep one point per distinct threshold value
  keep <- c(s[-length(s)] != s[-1], TRUE)
  tpr <- c(0, tp[keep] / P)
  fpr <- c(0, fp[keep] / N)
  thr <- c(Inf, s[keep])
  list(thr = thr, tpr = tpr, fpr = fpr)
}

# Youden-optimal threshold (maximizes sensitivity + specificity - 1 = TPR - FPR)
youden_threshold <- function(score, label) {
  rp <- roc_points(score, label)
  j <- rp$tpr - rp$fpr
  rp$thr[which.max(j)]
}

# Confusion-matrix metrics at a given threshold (predict positive if score >= thr)
confusion_metrics <- function(score, label, thr) {
  pred_pos <- score >= thr
  TP <- sum(pred_pos & label == 1)
  FP <- sum(pred_pos & label == 0)
  TN <- sum(!pred_pos & label == 0)
  FN <- sum(!pred_pos & label == 1)
  list(
    sensitivity = TP / (TP + FN),  # recall / TPR
    specificity = TN / (TN + FP),  # TNR
    ppv         = if ((TP + FP) > 0) TP / (TP + FP) else NA_real_,  # precision
    npv         = if ((TN + FN) > 0) TN / (TN + FN) else NA_real_,
    TP = TP, FP = FP, TN = TN, FN = FN
  )
}

# ======================================================================
# Compute metrics for each model
# ======================================================================
rows <- list()
roc_list <- list()

for (m in names(models)) {
  score <- preds[[models[[m]]]]

  auc   <- auc_fun(score, y)
  brier <- brier_fun(score, y)
  thr   <- youden_threshold(score, y)   # per-model Youden-optimal cutoff
  cm    <- confusion_metrics(score, y, thr)

  roc_list[[m]] <- roc_points(score, y)

  rows[[m]] <- data.frame(
    model              = m,
    n                  = length(y),
    n_positive         = sum(y == 1),
    auc_oof            = round(auc, 4),
    brier              = round(brier, 4),
    threshold          = round(thr, 4),
    threshold_rule     = "Youden's J (max sensitivity+specificity-1)",
    sensitivity        = round(cm$sensitivity, 4),
    specificity        = round(cm$specificity, 4),
    ppv                = round(cm$ppv, 4),
    npv                = round(cm$npv, 4),
    TP = cm$TP, FP = cm$FP, TN = cm$TN, FN = cm$FN,
    stringsAsFactors = FALSE
  )
}

metrics <- do.call(rbind, rows)
rownames(metrics) <- NULL

# ======================================================================
# Save metrics table
# ======================================================================
out_csv <- file.path(tables_dir, "model_metrics.csv")
write.csv(metrics, out_csv, row.names = FALSE)
cat("Saved metrics table ->", out_csv, "\n\n")

# ======================================================================
# Combined ROC curve plot
# ======================================================================
out_png <- file.path(figures_dir, "roc_curves.png")
png(out_png, width = 1800, height = 1500, res = 200)
cols <- c(`Elastic Net` = "#377EB8", `Random Forest` = "#4DAF4A", `XGBoost` = "#E41A1C")
plot(NA, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     main = "Out-of-Fold ROC Curves — 30-Day Readmission")
abline(0, 1, lty = 2, col = "grey60")
for (m in names(roc_list)) {
  rp <- roc_list[[m]]
  lines(rp$fpr, rp$tpr, col = cols[[m]], lwd = 2)
}
legend_labels <- sprintf("%s (AUC = %.3f)", metrics$model, metrics$auc_oof)
legend("bottomright", legend = legend_labels, col = cols[metrics$model],
       lwd = 2, bty = "n")
dev.off()
cat("Saved ROC plot ->", out_png, "\n\n")

# ======================================================================
# Print the table
# ======================================================================
cat("========== MODEL PERFORMANCE (out-of-fold) ==========\n")
print(metrics[, c("model", "auc_oof", "brier", "threshold",
                  "sensitivity", "specificity", "ppv", "npv")],
      row.names = FALSE)
cat("\nThreshold rule: Youden's J (per model). Full table incl. confusion",
    "counts saved to output/tables/model_metrics.csv\n")
