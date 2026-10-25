#!/usr/bin/env Rscript
#
# Heterogeneity in the Effect of Anti-Asian Bias on Asian Identification
# Date: October 25th, 2026
#
# Builds the single three-panel table that replaces the interaction
# coefficient plots (old Figures 14-16) at the JPAM editor's request
# (conditional acceptance, JPAM-2025-13688.R2, comment 1). Every model is
# the linear probability model previously estimated in scripts 40, 41, and
# 48, with identical samples, controls, weights, and clustering, so the
# tabulated coefficients are the ones that were plotted:
#
#   Asian ~ bias_dm * Female + bias_dm * Z1 + bias_dm * Z2
#           + frac_asian + quartic age | region x year,
#   weights = CPS person weight, SEs clustered by state,
#
# where (Z1, Z2) = (mother college, father college) for children and
# (own college, above-median family income) for adults. The pooled adult
# model was estimated in script 48 but never plotted; it is tabulated here.
#
# Also estimates the pooled mixed-race model that Reviewer 2 (comment 4)
# asked for: all coefficients constrained equal across AW and WA families
# except a bias x AW interaction.
#
# Must not: re-estimate the multinomial logit or touch the bootstrap draws.
# Run after 03 (children panel) and 34 (adult panel) have built the CSVs.
# Runner: programs/95-make-all.R (listed, commented like its neighbours).
#
# Results -> output/tables/tab50-heterogeneity-interactions.csv
#         -> output/tables/tab50-heterogeneity-interactions.tex
#         -> output/tables/tab50-pooled-bias-x-aw-lpm.csv

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# ---- Paths ----------------------------------------------------------------
# Env var first, then the runner's git_mdir, then the script argument/cwd.
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
if (!dir.exists(datasets_dir)) {
  stop(sprintf("Missing data directory: %s", datasets_dir), call. = FALSE)
}
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
script_name <- "50-make-heterogeneity-interaction-table.R"

# ---- Data -----------------------------------------------------------------
read_panel <- function(file) {
  path <- file.path(datasets_dir, file)
  if (!file.exists(path)) {
    stop(sprintf("Missing input: %s", path), call. = FALSE)
  }
  dat <- fread(path, showProgress = FALSE)
  # Same demeaning as scripts 40/41/48: full-file mean, before any subsetting.
  dat[, value := lw_index]
  dat[, value_dm := value - mean(value, na.rm = TRUE)]
  dat
}

kids <- read_panel("CPS_IAT_asian.csv")
adults <- read_panel("CPS_IAT_asian_adults.csv")

# Script 48 defines college as 16+ years of schooling and the income cut as
# the median of family income over the whole adult file (all generations),
# computed before subsetting. Reproduced exactly so Panel B matches Figure 15.
adults[, CollegeGrad := as.numeric(EducYears >= 16)]
adults[, AboveMedianInc := as.numeric(ftotval > median(ftotval, na.rm = TRUE))]

one_gp <- c("AWWW", "WAWW", "WWAW", "WWWA")
two_gp <- c("AAWW", "AWAW", "AWWA", "WAAW", "WWAA", "WAWA")
three_gp <- c("AAAW", "AAWA", "AWAA", "WAAA")
kids[, OneAsian := as.numeric(Grandparent_Type %in% one_gp)]
kids[, TwoAsian := as.numeric(Grandparent_Type %in% two_gp)]
kids[, ThreeAsian := as.numeric(Grandparent_Type %in% three_gp)]

# ---- Specifications -------------------------------------------------------
controls <- "frac_asian + Age + Age_sq + Age_cube + Age_quad"
fml_kids <- as.formula(sprintf(
  "Asian ~ value_dm * Female + value_dm * MomGradCollege + value_dm * DadGradCollege + %s | region^year",
  controls))
fml_adults <- as.formula(sprintf(
  "Asian ~ value_dm * Female + value_dm * CollegeGrad + value_dm * AboveMedianInc + %s | region^year",
  controls))

samples <- list(
  list(panel = "A", panel_title = "Mixed-race second generation (children)",
       group = "Asian father--White mother", key = "A_AW",
       dat = kids[SecondGen_Asian == 1 & AW_0bj == 1], fml = fml_kids),
  list(panel = "A", panel_title = "Mixed-race second generation (children)",
       group = "White father--Asian mother", key = "A_WA",
       dat = kids[SecondGen_Asian == 1 & WA_0bj == 1], fml = fml_kids),
  list(panel = "A", panel_title = "Mixed-race second generation (children)",
       group = "Pooled", key = "A_pooled",
       dat = kids[SecondGen_Asian == 1 & (AW_0bj == 1 | WA_0bj == 1)],
       fml = fml_kids),
  list(panel = "B", panel_title = "Mixed-race second generation (adults)",
       group = "Asian father--White mother", key = "B_AW",
       dat = adults[SecondGen_Asian == 1 & AW_0bj == 1], fml = fml_adults),
  list(panel = "B", panel_title = "Mixed-race second generation (adults)",
       group = "White father--Asian mother", key = "B_WA",
       dat = adults[SecondGen_Asian == 1 & WA_0bj == 1], fml = fml_adults),
  list(panel = "B", panel_title = "Mixed-race second generation (adults)",
       group = "Pooled", key = "B_pooled",
       dat = adults[SecondGen_Asian == 1 & (AW_0bj == 1 | WA_0bj == 1)],
       fml = fml_adults),
  list(panel = "C", panel_title = "Third generation (children)",
       group = "One Asian grandparent", key = "C_one",
       dat = kids[ThirdGen_Asian == 1 & OneAsian == 1], fml = fml_kids),
  list(panel = "C", panel_title = "Third generation (children)",
       group = "Two Asian grandparents", key = "C_two",
       dat = kids[ThirdGen_Asian == 1 & TwoAsian == 1], fml = fml_kids),
  list(panel = "C", panel_title = "Third generation (children)",
       group = "Three Asian grandparents", key = "C_three",
       dat = kids[ThirdGen_Asian == 1 & ThreeAsian == 1], fml = fml_kids),
  list(panel = "C", panel_title = "Third generation (children)",
       group = "Pooled", key = "C_pooled",
       dat = kids[ThirdGen_Asian == 1 &
                    (OneAsian == 1 | TwoAsian == 1 | ThreeAsian == 1)],
       fml = fml_kids)
)

term_labels <- c(
  "value_dm:Female" = "Bias $\\times$ Female",
  "value_dm:MomGradCollege" = "Bias $\\times$ Mother college",
  "value_dm:DadGradCollege" = "Bias $\\times$ Father college",
  "value_dm:CollegeGrad" = "Bias $\\times$ College graduate",
  "value_dm:AboveMedianInc" = "Bias $\\times$ Above-median income"
)
panel_terms <- list(
  A = c("value_dm:Female", "value_dm:MomGradCollege", "value_dm:DadGradCollege"),
  B = c("value_dm:Female", "value_dm:CollegeGrad", "value_dm:AboveMedianInc"),
  C = c("value_dm:Female", "value_dm:MomGradCollege", "value_dm:DadGradCollege")
)

# ---- Estimation -----------------------------------------------------------
fit_one <- function(spec) {
  model <- tryCatch(
    feols(spec$fml, data = spec$dat, weights = ~weight, vcov = ~statefip),
    error = function(e) NULL
  )
  if (is.null(model)) {
    stop(sprintf("feols failed for panel %s, group %s", spec$panel, spec$group),
         call. = FALSE)
  }
  ct <- coeftable(model)
  terms <- panel_terms[[spec$panel]]
  missing_terms <- setdiff(terms, rownames(ct))
  if (length(missing_terms) > 0) {
    stop(sprintf("Panel %s, group %s: missing terms %s", spec$panel,
                 spec$group, paste(missing_terms, collapse = ", ")),
         call. = FALSE)
  }
  # Counts come from the estimation sample, not the input subset.
  data.table(
    run_id = run_id, script = script_name,
    panel = spec$panel, panel_title = spec$panel_title,
    group = spec$group, group_key = spec$key,
    term = terms,
    term_label = unname(term_labels[terms]),
    estimate = ct[terms, "Estimate"],
    std_error = ct[terms, "Std. Error"],
    p_value = ct[terms, "Pr(>|t|)"],
    n_estimated = nobs(model),
    n_clusters = length(unique(spec$dat$statefip[obs(model)])),
    estimator = "OLS linear probability model",
    outcome = "Self-reported Asian identity (0/1)",
    fixed_effects = "region x year",
    weighting = "CPS person weight",
    inference = "cluster-robust SE, state"
  )
}

results <- rbindlist(lapply(samples, fit_one))
write.csv(results, file.path(tables_dir, "tab50-heterogeneity-interactions.csv"),
          row.names = FALSE)

# ---- Reviewer 2, comment 4: pooled model with bias x AW -------------------
# Author decision Oct 2026: constrain every slope except the bias effect to
# be common across AW and WA families, as the reviewer proposed, so the
# AW-WA difference in the bias effect is one coefficient with one SE.
fml_kids_aw <- as.formula(sprintf(
  "Asian ~ value_dm * AW_0bj + Female + MomGradCollege + DadGradCollege + %s | region^year",
  controls))
fml_adults_aw <- as.formula(sprintf(
  "Asian ~ value_dm * AW_0bj + Female + CollegeGrad + AboveMedianInc + %s | region^year",
  controls))
pooled_specs <- list(
  list(sample = "Second-generation children, AW + WA",
       dat = kids[SecondGen_Asian == 1 & (AW_0bj == 1 | WA_0bj == 1)],
       fml = fml_kids_aw),
  list(sample = "Second-generation adults, AW + WA",
       dat = adults[SecondGen_Asian == 1 & (AW_0bj == 1 | WA_0bj == 1)],
       fml = fml_adults_aw)
)
fit_pooled <- function(spec) {
  model <- tryCatch(
    feols(spec$fml, data = spec$dat, weights = ~weight, vcov = ~statefip),
    error = function(e) NULL
  )
  if (is.null(model)) {
    stop(sprintf("feols failed for pooled bias x AW model: %s", spec$sample),
         call. = FALSE)
  }
  ct <- coeftable(model)
  terms <- c("value_dm", "AW_0bj", "value_dm:AW_0bj")
  if (!all(terms %in% rownames(ct))) {
    stop(sprintf("Pooled bias x AW model (%s): missing terms", spec$sample),
         call. = FALSE)
  }
  data.table(
    run_id = run_id, script = script_name, sample = spec$sample,
    term = terms,
    estimate = ct[terms, "Estimate"],
    std_error = ct[terms, "Std. Error"],
    p_value = ct[terms, "Pr(>|t|)"],
    n_estimated = nobs(model),
    estimator = "OLS linear probability model",
    outcome = "Self-reported Asian identity (0/1)",
    fixed_effects = "region x year",
    weighting = "CPS person weight",
    inference = "cluster-robust SE, state"
  )
}
pooled_results <- rbindlist(lapply(pooled_specs, fit_pooled))
write.csv(pooled_results,
          file.path(tables_dir, "tab50-pooled-bias-x-aw-lpm.csv"),
          row.names = FALSE)

# ---- LaTeX table ----------------------------------------------------------
stars <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}
fmt_est <- function(est, p) {
  est <- round(est, 3)
  est[est == 0] <- 0  # drop the sign of a rounded negative zero
  sprintf("$%s$%s", formatC(est, format = "f", digits = 3), stars(p))
}
fmt_se <- function(se) sprintf("($%s$)", formatC(se, format = "f", digits = 3))
fmt_n <- function(n) sprintf("$%s$", formatC(n, format = "d", big.mark = "{,}"))

panel_block <- function(panel_id) {
  rows <- results[panel == panel_id]
  terms <- panel_terms[[panel_id]]
  header <- sprintf(
    "\\multicolumn{5}{l}{\\textit{Panel %s: %s}} \\\\\n & %s & Observations \\\\\n\\midrule",
    panel_id, rows$panel_title[1],
    paste(unname(term_labels[terms]), collapse = " & "))
  body <- vapply(unique(rows$group_key), function(key) {
    r <- rows[group_key == key]
    r <- r[match(terms, r$term)]
    label <- r$group[1]
    if (label == "Pooled") label <- "\\textbf{Pooled}"
    paste0(
      sprintf("%s & %s & %s \\\\\n", label,
              paste(fmt_est(r$estimate, r$p_value), collapse = " & "),
              fmt_n(r$n_estimated[1])),
      sprintf(" & %s & \\\\", paste(fmt_se(r$std_error), collapse = " & "))
    )
  }, character(1))
  paste(c(header, body), collapse = "\n")
}

notes <- paste(
  "Each row is a separate weighted linear probability model,",
  "$A_{ist} = \\beta_1 Bias_{st} + \\sum_k \\delta_k (Bias_{st} \\times Z_{ist}^k)",
  "+ \\sum_k \\theta_k Z_{ist}^k + \\mathbf{X}_{ist}\\pi + \\gamma_{rt} + \\varepsilon_{ist}$,",
  "where $A_{ist}$ is self-reported Asian identity, $Bias_{st}$ is the",
  "demeaned state-level anti-Asian bias index (standard deviation units),",
  "and the table reports the interaction coefficients $\\delta_k$.",
  "In Panels A and C, $Z$ is female, mother college graduate, and father",
  "college graduate; in Panel B, $Z$ is female, own college degree",
  "(16 or more years of schooling), and family income above the median of",
  "the full adult file. All models include region $\\times$ year fixed effects and",
  "controls for a quartic in age and the state Asian population share, and",
  "are weighted by CPS person weights. Panel A uses children ages 17 and",
  "younger living with both parents, with one parent born in an Asian",
  "country and the other born outside Asia (labelled White, following the",
  "paper's convention; about 86 percent of these parents are US-born); Panel B uses adults ages 18",
  "and older with the same parental configuration, restricted to March (ASEC)",
  "respondents because family income is observed only for them; Panel C uses native-born",
  "children with native-born parents and one, two, or three grandparents",
  "born in an Asian country. The pooled rows do not include group indicators,",
  "matching the specifications previously plotted. Standard errors clustered",
  "at the state level in parentheses. $^{*}p<0.1$, $^{**}p<0.05$,",
  "$^{***}p<0.01$. Data source is the 2004--2021 Current Population Survey.")

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\small\\setstretch{1.0}",
  "\\caption{Heterogeneity in the Effect of Anti-Asian Bias on Asian Identification \\label{tab:heterogeneity-interactions}}",
  "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  panel_block("A"),
  "\\midrule",
  panel_block("B"),
  "\\midrule",
  panel_block("C"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  sprintf("\\item[1] \\footnotesize{%s}", notes),
  "\\end{tablenotes}",
  "\\end{threeparttable}}",
  "\\end{table}"
)
tex_path <- file.path(tables_dir, "tab50-heterogeneity-interactions.tex")
writeLines(tex, tex_path)

cat(sprintf("Wrote %d interaction estimates and %d pooled bias x AW estimates (run %s)\n",
            nrow(results), nrow(pooled_results), run_id))
print(results[, .(panel, group, term, estimate = round(estimate, 3),
                  se = round(std_error, 3), p = round(p_value, 3), n_estimated)])
print(pooled_results[, .(sample, term, estimate = round(estimate, 3),
                         se = round(std_error, 3), p = round(p_value, 3), n_estimated)])
