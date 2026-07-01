# ==============================================================================
# Step 9: Post-hoc Calibration of XGBoost OOF Probabilities
# The XGBoost out-of-fold probabilities in cv_predictions_step5.csv are
# miscalibrated (05_modeling.R trains with scale_pos_weight). This script fits
# post-hoc calibration on the SAVED OOF predictions and reports the effect.
# Nothing is retrained — only cv_predictions_step5.csv is read.
#
# Calibration is done OUT-OF-FOLD: 5 stratified folds; for each fold the
# calibrator (Platt / isotonic) is fit on the other 4 folds and applied to the
# held-out fold. So the calibration map is never scored on data it was fit on.
#
# Outputs:
#   output/tables/xgb_calibration_metrics.csv   raw vs Platt vs isotonic
#   output/figures/xgb_calibration.png          reliability plot (10 bins)
# ==============================================================================

# --- Paths ---
project_root  <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data"
processed_dir <- file.path(project_root, "data/processed")
tables_dir    <- file.path(project_root, "output/tables")
figures_dir   <- file.path(project_root, "output/figures")
dir.create(tables_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load saved OOF predictions (do NOT retrain) ---
preds <- read.csv(file.path(processed_dir, "cv_predictions_step5.csv"))
y      <- preds$y_true
p_raw  <- preds$pred_xgb
n      <- length(y)

cat("Loaded", n, "OOF predictions.  Positive rate:",
    sprintf("%.1f%%\n\n", 100 * mean(y)))

# ======================================================================
# Metric helpers (base R)
# ======================================================================
clamp <- function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)

auc_fun <- function(score, label) {           # exact Mann-Whitney AUC
  np <- sum(label == 1); nn <- sum(label == 0)
  if (np == 0 || nn == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[label == 1]) - np * (np + 1) / 2) / (np * nn)
}
brier_fun   <- function(score, label) mean((score - label)^2)
logloss_fun <- function(score, label) {
  s <- clamp(score)
  -mean(label * log(s) + (1 - label) * log(1 - s))
}

# ======================================================================
# Calibrator fit/apply functions
# ======================================================================
# Platt scaling: logistic regression of the label on the LOGIT of the raw score
# (i.e. an affine rescale in logit space — temperature + intercept correction).
platt_fit <- function(p, label) {
  z <- qlogis(clamp(p, 1e-6))
  glm(label ~ z, family = binomial())
}
platt_apply <- function(model, p) {
  z <- qlogis(clamp(p, 1e-6))
  as.numeric(predict(model, newdata = data.frame(z = z), type = "response"))
}

# Isotonic regression: monotone non-decreasing map from raw score -> prob.
# Fit with isoreg, apply to new scores by (monotone) linear interpolation.
iso_fit <- function(p, label) {
  o  <- order(p)
  ir <- isoreg(p[o], label[o])
  approxfun(ir$x, ir$yf, method = "linear", rule = 2, ties = mean)
}
iso_apply <- function(fn, p) pmin(pmax(fn(p), 0), 1)

# ======================================================================
# Out-of-fold calibration (5 stratified folds)
# ======================================================================
set.seed(42)
K <- 5
fold <- integer(n)
for (cls in c(0, 1)) {                       # stratify so fold positive-rate is stable
  idx <- which(y == cls)
  fold[idx] <- sample(rep(seq_len(K), length.out = length(idx)))
}

cal_platt <- numeric(n)
cal_iso   <- numeric(n)
for (k in seq_len(K)) {
  tr <- which(fold != k)
  te <- which(fold == k)
  cal_platt[te] <- platt_apply(platt_fit(p_raw[tr], y[tr]), p_raw[te])
  cal_iso[te]   <- iso_apply(iso_fit(p_raw[tr], y[tr]),   p_raw[te])
}

# ======================================================================
# Metrics table
# ======================================================================
variants <- list(raw = p_raw, platt = cal_platt, isotonic = cal_iso)
metrics <- do.call(rbind, lapply(names(variants), function(nm) {
  s <- variants[[nm]]
  data.frame(
    method  = nm,
    n       = n,
    auc     = round(auc_fun(s, y),     4),
    brier   = round(brier_fun(s, y),   4),
    logloss = round(logloss_fun(s, y), 4),
    stringsAsFactors = FALSE
  )
}))
rownames(metrics) <- NULL

out_csv <- file.path(tables_dir, "xgb_calibration_metrics.csv")
write.csv(metrics, out_csv, row.names = FALSE)
cat("Saved calibration metrics ->", out_csv, "\n")

# ======================================================================
# Reliability plot (10 equal-width bins)
# ======================================================================
reliability <- function(score, label, bins = 10) {
  br  <- seq(0, 1, length.out = bins + 1)
  bin <- cut(score, breaks = br, include.lowest = TRUE)
  data.frame(
    mean_pred = tapply(score, bin, mean),
    obs_freq  = tapply(label, bin, mean)
  )
}

out_png <- file.path(figures_dir, "xgb_calibration.png")
png(out_png, width = 1800, height = 1500, res = 200)
cols <- c(raw = "#E41A1C", platt = "#377EB8", isotonic = "#4DAF4A")
plot(NA, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Mean predicted probability (bin)",
     ylab = "Observed readmission frequency (bin)",
     main = "XGBoost Reliability — Raw vs Calibrated (10 bins)")
abline(0, 1, lty = 2, col = "grey60")
for (nm in names(variants)) {
  rel <- reliability(variants[[nm]], y)
  points(rel$mean_pred, rel$obs_freq, col = cols[[nm]], pch = 19)
  lines(rel$mean_pred,  rel$obs_freq, col = cols[[nm]], lwd = 2)
}
legend("topleft",
       legend = sprintf("%s (Brier %.3f)", metrics$method, metrics$brier),
       col = cols[metrics$method], lwd = 2, pch = 19, bty = "n")
dev.off()
cat("Saved reliability plot ->", out_png, "\n\n")

# ======================================================================
# Print result
# ======================================================================
cat("========== XGBOOST CALIBRATION (out-of-fold) ==========\n")
print(metrics, row.names = FALSE)
cat("\nCalibration: 5-fold stratified out-of-fold (calibrator fit on 4 folds,",
    "applied to the held-out fold).\n")
