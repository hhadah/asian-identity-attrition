#!/usr/bin/env Rscript
#
# Pooled Multinomial Logit: Does Anti-Asian Bias Affect AW and WA Children
# Differently?
# Date: October 25th, 2026
#
# Reviewer 2 (JPAM-2025-13688.R2, comment 4) objected that comparing
# coefficients or marginal effects across separately estimated multinomial
# logits conflates (i) the scale normalisation of each model's errors and
# (ii) differences in the covariate distributions of the two groups. The
# reviewer's remedy: pool AW and WA children, constrain every coefficient
# to be common, and interact bias with an AW indicator. This script does
# that, with the same outcome, sample construction, controls, weights, and
# region x year dummies as the bootstrap models in script 44.
#
# Inference. nnet::multinom has no cluster-robust covariance and the
# sandwich package has no multinom methods, so the state-clustered
# covariance is built by hand from the weighted scores
#   s_i = w_i * (1{y_i = j} - p_ij) x_i,  j = 2, ..., J,
# and nnet's observed information matrix (model$Hessian, which vcov.multinom
# inverts). It is verified at run time against sandwich::vcovCL() on a
# two-category fit, where a multinomial logit is a binary logit and the two
# must agree; a mismatch is fatal. The reviewer's likelihood-ratio test is
# also reported, labelled as assuming independent observations.
#
# Must not: touch the bootstrap draws or the main-text marginal effects.
# Runtime: a few minutes (two full multinom fits plus the verification fit).
# Runner: programs/95-make-all.R (listed, commented like its neighbours).
#
# Results -> output/tables/tab51-pooled-mnl-bias-x-aw.csv
#         -> output/tables/tab51-pooled-mnl-bias-x-aw-ame.csv
#         -> output/tables/tab51-pooled-mnl-bias-x-aw.tex

suppressPackageStartupMessages({
  library(data.table)
  library(nnet)
  library(sandwich)
  library(marginaleffects)
})

# ---- Paths ----------------------------------------------------------------
resolve_root <- function() {
  env_root <- Sys.getenv("ASIAN_IDENTITY_ROOT")
  if (nzchar(env_root)) return(normalizePath(env_root, mustWork = TRUE))
  if (exists("git_mdir", inherits = TRUE)) {
    return(normalizePath(get("git_mdir", inherits = TRUE), mustWork = TRUE))
  }
  args <- commandArgs(trailingOnly = TRUE)
  normalizePath(if (length(args) >= 1) args[[1]] else getwd(),
                mustWork = TRUE)
}
root <- resolve_root()
datasets_dir <- file.path(root, "data", "datasets")
tables_dir <- file.path(root, "output", "tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
script_name <- "51-run-pooled-mnl-bias-x-aw.R"
assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

# ---- Data: same construction as script 44 ---------------------------------
input_path <- file.path(datasets_dir, "CPS_IAT_asian.csv")
assert_true(file.exists(input_path), sprintf("Missing input: %s", input_path))
dat <- fread(input_path, showProgress = FALSE)
dat[, value := lw_index]
dat[, identity_choice := fifelse(race == 651L, "Asian_only",
                           fifelse(race == 100L, "White_only",
                             fifelse(race %in% c(803L, 804L, 809L, 813L),
                                     "Asian_and_White", "Other")))]
dat <- dat[SecondGen_Asian == 1 & (AW_0bj == 1 | WA_0bj == 1) &
             identity_choice != "Other" & !is.na(value)]
dat[, identity_choice := factor(identity_choice,
                                levels = c("Asian_only", "White_only",
                                           "Asian_and_White"))]
dat[, region_year := factor(paste(region, year, sep = "_"))]
model_vars <- c("identity_choice", "value", "AW_0bj", "Female",
                "MomGradCollege", "DadGradCollege", "frac_asian", "Age",
                "Age_sq", "Age_cube", "Age_quad", "region_year", "weight",
                "statefip")
dat <- dat[complete.cases(dat[, model_vars, with = FALSE])]
dat <- as.data.frame(dat[, model_vars, with = FALSE])
# Author decision Oct 2026: scale weights to mean one. Point estimates and
# the clustered covariance are invariant to the scale; the pseudo
# likelihood-ratio statistic is not (raw CPS weights average about 2,600
# and would inflate it by that factor).
dat$weight <- dat$weight / mean(dat$weight)
# Author decision Oct 2026: a region x year cell in which an outcome category
# never occurs sends that cell's dummy to infinity and makes the observed
# information matrix singular (division 32 in 2012 and 2013 have no "Asian
# only" children). Such cells carry no information about the interaction;
# drop them and report the count.
n_before <- nrow(dat)
cell_counts <- table(dat$region_year, dat$identity_choice)
separated_cells <- rownames(cell_counts)[apply(cell_counts == 0, 1, any)]
dat <- dat[!dat$region_year %in% separated_cells, ]
dat$region_year <- droplevels(dat$region_year)
n_dropped_separated <- n_before - nrow(dat)
cat(sprintf("Dropped %d observations in %d region x year cells with an empty outcome category: %s\n",
            n_dropped_separated, length(separated_cells),
            paste(separated_cells, collapse = ", ")))
n_obs <- nrow(dat)
assert_true(n_obs > 0, "Empty estimation sample.")

rhs_common <- paste("Female + MomGradCollege + DadGradCollege + frac_asian +",
                    "Age + Age_sq + Age_cube + Age_quad + region_year")
fml_u <- as.formula(sprintf("identity_choice ~ value * AW_0bj + %s", rhs_common))
fml_r <- as.formula(sprintf("identity_choice ~ value + AW_0bj + %s", rhs_common))

fit_mnl <- function(fml, data, label) {
  # weights is resolved by model.frame inside the data frame; passing
  # data$weight here breaks because multinom evaluates it by name.
  model <- tryCatch(
    multinom(fml, data = data, weights = weight, Hess = TRUE,
             maxit = 2000, reltol = 1e-12, trace = FALSE),
    error = function(e) NULL
  )
  assert_true(!is.null(model), sprintf("multinom failed: %s", label))
  assert_true(model$convergence == 0,
              sprintf("multinom did not converge: %s", label))
  # marginaleffects recovers the estimation data by evaluating call$data;
  # the bare symbol `data` would resolve to utils::data, so embed the frame.
  model$call$data <- data
  model
}

# ---- Cluster-robust covariance for nnet::multinom -------------------------
# Returns the covariance in the same parameter order as vcov(model):
# outcome-major (all regressors for outcome 2, then outcome 3, ...).
vcov_cluster_multinom <- function(model, fml, data, cluster, weights) {
  X <- model.matrix(delete.response(terms(fml)), data = data)
  P <- fitted(model)
  # Two-category fits return an n x 1 matrix holding P(level 2).
  if (NCOL(P) == 1) P <- cbind(1 - as.vector(P), as.vector(P))
  lev <- model$lev
  colnames(P) <- lev
  y <- as.character(data$identity_choice)
  assert_true(nrow(X) == nrow(P) && nrow(X) == length(y),
              "Design matrix, fitted values, and outcome are not aligned.")
  non_ref <- lev[-1]
  scores <- do.call(cbind, lapply(non_ref, function(j) {
    weights * ((y == j) - P[, j]) * X
  }))
  colnames(scores) <- as.vector(vapply(non_ref, function(j) {
    paste(j, colnames(X), sep = ":")
  }, character(ncol(X))))
  H <- model$Hessian
  param_names <- rownames(vcov(model))
  if (length(non_ref) == 1) {
    # Two-category multinom names parameters without the outcome prefix.
    colnames(scores) <- colnames(X)
  }
  assert_true(identical(colnames(scores), param_names),
              "Score columns do not match the multinom parameter order.")
  cluster_scores <- rowsum(scores, group = cluster)
  G <- nrow(cluster_scores)
  meat <- crossprod(cluster_scores)
  bread <- solve(H)
  V <- bread %*% meat %*% bread * (G / (G - 1))
  dimnames(V) <- list(param_names, param_names)
  list(vcov = V, n_clusters = G)
}

# ---- Verification: two-category multinom must equal binary logit ----------
# Author decision Oct 2026: run the check on the White-only vs Asian-only
# subsample with the full model so the test exercises the real design matrix.
check_dat <- dat[dat$identity_choice != "Asian_and_White", ]
check_dat$identity_choice <- droplevels(check_dat$identity_choice)
check_dat$region_year <- droplevels(check_dat$region_year)
check_dat$white_only <- as.numeric(check_dat$identity_choice == "White_only")
check_mnl <- fit_mnl(fml_u, check_dat, "verification (two categories)")
# IRLS diverges from the default start with the age quartic; start the
# reference fit at the multinom solution, which is the same likelihood.
check_glm <- suppressWarnings(glm(
  update(fml_u, white_only ~ .), data = check_dat, weights = check_dat$weight,
  family = binomial(), start = as.vector(coef(check_mnl)),
  control = glm.control(maxit = 100, epsilon = 1e-12)))
assert_true(check_glm$converged, "Verification: reference glm did not converge.")
check_V <- vcov_cluster_multinom(check_mnl, fml_u, check_dat,
                                 check_dat$statefip, check_dat$weight)$vcov
check_V_ref <- vcovCL(check_glm, cluster = check_dat$statefip, type = "HC0",
                      cadjust = TRUE)
coef_gap <- max(abs(coef(check_mnl) - coef(check_glm)))
assert_true(coef_gap < 1e-3,
            sprintf("Verification: coefficients differ by %.2e", coef_gap))
se_gap <- max(abs(sqrt(diag(check_V)) / sqrt(diag(check_V_ref)) - 1))
assert_true(se_gap < 1e-3,
            sprintf("Verification: clustered SEs differ by %.2e (relative)",
                    se_gap))
# nnet's vcov.multinom() inverts the Hessian with an SVD pseudo-inverse whose
# tolerance truncates the badly conditioned age quartic and returns standard
# errors near zero. solve() gives the correct inverse information; check it
# against glm's conventional covariance on the same fit.
conv_gap <- max(abs(sqrt(diag(solve(check_mnl$Hessian))) /
                      sqrt(diag(vcov(check_glm))) - 1))
assert_true(conv_gap < 1e-3,
            sprintf("Verification: conventional SEs differ by %.2e (relative)",
                    conv_gap))
cat(sprintf("PASS: clustered multinom covariance matches sandwich::vcovCL on %d obs, %d parameters (max coef gap %.1e, max relative SE gap %.1e); solve(Hessian) matches glm conventional SEs (max relative gap %.1e)\n",
            nrow(check_dat), length(coef(check_glm)), coef_gap, se_gap, conv_gap))

# ---- Estimation -----------------------------------------------------------
mnl_u <- fit_mnl(fml_u, dat, "unrestricted (bias x AW)")
mnl_r <- fit_mnl(fml_r, dat, "restricted (common bias effect)")
cl <- vcov_cluster_multinom(mnl_u, fml_u, dat, dat$statefip, dat$weight)
V <- cl$vcov
# Conventional (observed-information) covariance, for comparison with the
# clustered one: the individual-level bootstrap behind the main figures
# treats children as independent, as this covariance does.
V_conv <- solve(mnl_u$Hessian)
dimnames(V_conv) <- dimnames(V)
b <- as.vector(t(coef(mnl_u)))
names(b) <- rownames(V)

inter_names <- paste(mnl_u$lev[-1], "value:AW_0bj", sep = ":")
assert_true(all(inter_names %in% names(b)),
            "Interaction parameters not found in the unrestricted model.")
wald_stat <- as.numeric(t(b[inter_names]) %*% solve(V[inter_names, inter_names]) %*% b[inter_names])
wald_df <- length(inter_names)
wald_p <- pchisq(wald_stat, df = wald_df, lower.tail = FALSE)
lr_stat <- mnl_r$deviance - mnl_u$deviance
lr_p <- pchisq(lr_stat, df = wald_df, lower.tail = FALSE)

report_terms <- c("value", "AW_0bj", "value:AW_0bj")
coef_rows <- rbindlist(lapply(mnl_u$lev[-1], function(j) {
  keys <- paste(j, report_terms, sep = ":")
  data.table(
    run_id = run_id, script = script_name,
    outcome = j, term = report_terms,
    estimate = b[keys], std_error = sqrt(diag(V)[keys]),
    std_error_conventional = sqrt(diag(V_conv)[keys]),
    p_value = 2 * pnorm(-abs(b[keys] / sqrt(diag(V)[keys]))),
    n_estimated = n_obs, n_clusters = cl$n_clusters,
    n_dropped_separated_cells = n_dropped_separated,
    wald_stat = wald_stat, wald_df = wald_df, wald_p = wald_p,
    lr_stat = lr_stat, lr_df = wald_df, lr_p = lr_p,
    estimator = "multinomial logit, Asian only reference",
    fixed_effects = "region x year", weighting = "CPS person weight",
    inference = "cluster-robust SE, state (hand-built sandwich)"
  )
}))
write.csv(coef_rows, file.path(tables_dir, "tab51-pooled-mnl-bias-x-aw.csv"),
          row.names = FALSE)

# ---- Average marginal effects of bias at AW = 1 and AW = 0 ----------------
# Evaluated on the whole pooled sample with AW switched for everyone, so the
# AW-WA gap is not driven by differences in the two groups' covariates.
cf <- datagrid(model = mnl_u, AW_0bj = c(0, 1), grid_type = "counterfactual")
ame <- avg_slopes(mnl_u, variables = "value", newdata = cf, by = "AW_0bj",
                  vcov = V)
ame <- as.data.frame(ame)
gap <- avg_slopes(mnl_u, variables = "value", newdata = cf, by = "AW_0bj",
                  vcov = V, hypothesis = ~ pairwise | group)
gap <- as.data.frame(gap)
needed <- c("group", "AW_0bj", "estimate", "std.error", "p.value")
assert_true(all(needed %in% names(ame)),
            "avg_slopes output lacks expected columns.")
assert_true(all(c("group", "estimate", "std.error", "p.value") %in% names(gap)),
            "Pairwise hypothesis output lacks expected columns.")
# Common-effect (restricted) model: the pooled average marginal effect of
# bias with state-clustered and conventional standard errors. This is the
# state-clustered counterpart of the pooled estimate in the main figure.
V_r <- vcov_cluster_multinom(mnl_r, fml_r, dat, dat$statefip, dat$weight)$vcov
ame_common <- as.data.frame(avg_slopes(mnl_r, variables = "value", vcov = V_r))
ame_common_conv <- as.data.frame(avg_slopes(mnl_r, variables = "value",
                                            vcov = solve(mnl_r$Hessian)))
assert_true(all(c("group", "estimate", "std.error", "p.value") %in% names(ame_common)),
            "Common-effect avg_slopes output lacks expected columns.")
assert_true(identical(as.character(ame_common$group),
                      as.character(ame_common_conv$group)),
            "Common-effect AME outputs are not aligned.")
ame_rows <- data.table(
  run_id = run_id, script = script_name,
  outcome = c(ame_common$group, ame$group, gap$group),
  contrast = c(rep("AME of bias, common effect", nrow(ame_common)),
               ifelse(ame$AW_0bj == 1, "AME of bias, AW = 1",
                      "AME of bias, AW = 0"),
               rep("AW minus WA", nrow(gap))),
  estimate = c(ame_common$estimate, ame$estimate, gap$estimate),
  std_error = c(ame_common$std.error, ame$std.error, gap$std.error),
  std_error_conventional = c(ame_common_conv$std.error,
                             rep(NA_real_, nrow(ame) + nrow(gap))),
  p_value = c(ame_common$p.value, ame$p.value, gap$p.value),
  n_estimated = n_obs, n_clusters = cl$n_clusters
)
# marginaleffects reports pairwise contrasts as "(b) - (a)" by sort order;
# make the sign explicit by recomputing AW (1) minus WA (0).
for (g in unique(gap$group)) {
  a1 <- ame$estimate[ame$group == g & ame$AW_0bj == 1]
  a0 <- ame$estimate[ame$group == g & ame$AW_0bj == 0]
  reported <- gap$estimate[gap$group == g]
  assert_true(abs(abs(reported) - abs(a1 - a0)) < 1e-8,
              sprintf("Pairwise contrast for %s does not match AME gap", g))
  ame_rows[outcome == g & contrast == "AW minus WA",
           estimate := sign(a1 - a0) * abs(reported)]
}
write.csv(ame_rows, file.path(tables_dir, "tab51-pooled-mnl-bias-x-aw-ame.csv"),
          row.names = FALSE)

# ---- LaTeX table ----------------------------------------------------------
stars <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}
fmt_est <- function(est, p) {
  est <- round(est, 3)
  est[est == 0] <- 0
  sprintf("$%s$%s", formatC(est, format = "f", digits = 3), stars(p))
}
fmt_se <- function(se) sprintf("($%s$)", formatC(se, format = "f", digits = 3))
fmt_p <- function(p) {
  if (p < 0.001) "< 0.001" else sprintf("= %s", formatC(p, format = "f", digits = 3))
}
outcome_label <- c(White_only = "White only", Asian_and_White = "Asian and White")
term_label <- c(value = "Bias", AW_0bj = "Asian father--White mother (AW)",
                `value:AW_0bj` = "Bias $\\times$ AW")
outcomes <- mnl_u$lev[-1]

coef_block <- vapply(report_terms, function(tm) {
  r <- coef_rows[term == tm][match(outcomes, outcome)]
  paste0(sprintf("%s & %s \\\\\n", term_label[[tm]],
                 paste(fmt_est(r$estimate, r$p_value), collapse = " & ")),
         sprintf(" & %s \\\\", paste(fmt_se(r$std_error), collapse = " & ")))
}, character(1))
ame_block <- vapply(c("AME of bias, common effect", "AME of bias, AW = 1",
                      "AME of bias, AW = 0", "AW minus WA"),
                    function(ct) {
  r <- ame_rows[contrast == ct][match(outcomes, outcome)]
  label <- c(`AME of bias, common effect` = "Average marginal effect of bias, common-effect model",
             `AME of bias, AW = 1` = "Average marginal effect of bias, AW families",
             `AME of bias, AW = 0` = "Average marginal effect of bias, WA families",
             `AW minus WA` = "Difference, AW minus WA")[[ct]]
  paste0(sprintf("%s & %s \\\\\n", label,
                 paste(fmt_est(r$estimate, r$p_value), collapse = " & ")),
         sprintf(" & %s \\\\", paste(fmt_se(r$std_error), collapse = " & ")))
}, character(1))

notes <- paste(
  "The table reports a single multinomial logit, equation",
  "(\\ref{eq:multinomial_logit}), estimated on pooled second-generation",
  "children with one parent born in an Asian country and one born outside Asia, with",
  "``Asian only'' as the reference category. All coefficients are",
  "constrained to be equal across Asian father--White mother (AW) and White",
  "father--Asian mother (WA) families except that the bias index is",
  "interacted with an AW indicator. Controls are sex, mother and father",
  "college graduation, a quartic in age, the state Asian population share,",
  "and region $\\times$ year fixed effects; estimation is weighted by CPS",
  sprintf("person weights scaled to mean one. %s observations in %d region $\\times$ year cells with no ``Asian only'' responses are excluded because the cell dummies are not identified.",
          formatC(n_dropped_separated, format = "d", big.mark = "{,}"),
          length(separated_cells)),
  "Standard errors in parentheses are clustered at the",
  "state level using a sandwich covariance built from the weighted",
  "multinomial logit scores. The Wald test uses that clustered covariance;",
  "the likelihood-ratio test compares the model with and without the two",
  "interaction terms and assumes independent observations. Average marginal",
  "effects of a one standard deviation increase in bias are evaluated on",
  "the full pooled sample with the AW indicator set to one for every",
  "observation and then to zero, so the two rows share the same covariate",
  "distribution; the difference is the AW--WA gap with its delta-method",
  "standard error. This is a test of whether the bias effect differs by",
  "parental configuration, not of the bias effect itself. The first row of",
  "Panel B comes from the restricted model with a common bias effect.",
  sprintf("Conventional standard errors that treat children as independent, the assumption the individual-level bootstrap in the main figures also makes, are smaller: for the common-effect average marginal effects they are %s (White only) and %s (Asian and White) against the clustered %s and %s, and for the bias coefficients in Panel A they are %s and %s against %s and %s.",
          formatC(ame_rows[contrast == "AME of bias, common effect" & outcome == "White_only", std_error_conventional], format = "f", digits = 3),
          formatC(ame_rows[contrast == "AME of bias, common effect" & outcome == "Asian_and_White", std_error_conventional], format = "f", digits = 3),
          formatC(ame_rows[contrast == "AME of bias, common effect" & outcome == "White_only", std_error], format = "f", digits = 3),
          formatC(ame_rows[contrast == "AME of bias, common effect" & outcome == "Asian_and_White", std_error], format = "f", digits = 3),
          formatC(coef_rows[term == "value" & outcome == "White_only", std_error_conventional], format = "f", digits = 3),
          formatC(coef_rows[term == "value" & outcome == "Asian_and_White", std_error_conventional], format = "f", digits = 3),
          formatC(coef_rows[term == "value" & outcome == "White_only", std_error], format = "f", digits = 3),
          formatC(coef_rows[term == "value" & outcome == "Asian_and_White", std_error], format = "f", digits = 3)),
  "$^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$.",
  "Data source is the 2004--2021 Current Population Survey.")

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\small\\setstretch{1.0}",
  "\\caption{Pooled Multinomial Logit: Bias Interacted with Parental Configuration, Mixed-Race Second-Generation Children \\label{tab:pooled-mnl-bias-x-aw}}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  sprintf(" & %s \\\\", paste(outcome_label[outcomes], collapse = " & ")),
  sprintf(" & %s \\\\", paste(sprintf("(%d)", seq_along(outcomes)), collapse = " & ")),
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Panel A: Coefficients (log odds relative to Asian only)}} \\\\",
  coef_block,
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Panel B: Average marginal effects of bias (probability scale)}} \\\\",
  ame_block,
  "\\midrule",
  sprintf("\\multicolumn{3}{l}{Wald test, Bias $\\times$ AW $= 0$ in both equations (clustered): $\\chi^2(%d) = %s$, $p %s$} \\\\",
          wald_df, formatC(wald_stat, format = "f", digits = 2), fmt_p(wald_p)),
  sprintf("\\multicolumn{3}{l}{Likelihood-ratio test (independent observations): $\\chi^2(%d) = %s$, $p %s$} \\\\",
          wald_df, formatC(lr_stat, format = "f", digits = 2), fmt_p(lr_p)),
  sprintf("Observations & \\multicolumn{2}{c}{$%s$} \\\\",
          formatC(n_obs, format = "d", big.mark = "{,}")),
  sprintf("State clusters & \\multicolumn{2}{c}{$%d$} \\\\", cl$n_clusters),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  sprintf("\\item[1] \\footnotesize{%s}", notes),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
writeLines(tex, file.path(tables_dir, "tab51-pooled-mnl-bias-x-aw.tex"))

cat(sprintf("Unrestricted model: N = %d, clusters = %d, Wald chi2(%d) = %.2f (p = %.3f), LR chi2(%d) = %.2f (p = %.3f)\n",
            n_obs, cl$n_clusters, wald_df, wald_stat, wald_p, wald_df, lr_stat, lr_p))
print(coef_rows[, .(outcome, term, estimate = round(estimate, 3),
                    se = round(std_error, 3), p = round(p_value, 3))])
print(ame_rows[, .(outcome, contrast, estimate = round(estimate, 3),
                   se = round(std_error, 3), p = round(p_value, 3))])
