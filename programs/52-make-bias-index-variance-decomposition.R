#!/usr/bin/env Rscript
#
# Between-State and Within-State Variation in the Anti-Asian Bias Index
# Date: October 25th, 2026
#
# Reviewer 2 (JPAM-2025-13688.R2, comment 1) asked for the standard
# deviation of the bias index across states rather than a two-state time
# series as evidence of cross-sectional variation, and for a statement of
# how much variation is within state over time. This script decomposes the
# state-year bias index (the Lubotsky-Wittenberg composite, lw_index, as
# merged onto the children's CPS panel) into between-state and within-state
# components and reports the numbers that the text and the response quote.
#
# Units are those of the index itself; the manuscript describes a one-unit
# change as one standard deviation, and the individual-level standard
# deviation in the estimation sample is reported here so that claim can be
# checked against the data.
#
# Must not: modify the panel. Run after 03 has built CPS_IAT_asian.csv.
# Runner: programs/95-make-all.R (listed, commented like its neighbours).
#
# Results -> output/tables/bias-index-variance-decomposition.csv

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

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
input_path <- file.path(root, "data", "datasets", "CPS_IAT_asian.csv")
tables_dir <- file.path(root, "output", "tables")
if (!file.exists(input_path)) {
  stop(sprintf("Missing input: %s", input_path), call. = FALSE)
}
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")

dat <- fread(input_path, showProgress = FALSE,
             select = c("statefip", "year", "region", "lw_index", "weight"))
dat <- dat[!is.na(lw_index)]

# The index is a state-year quantity; make sure it does not vary within cell.
cells <- unique(dat[, .(statefip, year, region, lw_index)])
if (nrow(cells) != nrow(unique(cells[, .(statefip, year)]))) {
  stop("lw_index varies within state-year cells.", call. = FALSE)
}

state_means <- cells[, .(state_mean = mean(lw_index), n_years = .N),
                     by = statefip]
cells <- merge(cells, state_means, by = "statefip")
cells[, within_dev := lw_index - state_mean]
year_sd <- cells[, .(sd_across_states = sd(lw_index), n_states = .N),
                 by = year]

overall_var <- var(cells$lw_index)
between_var <- var(state_means$state_mean)
within_var <- var(cells$within_dev)

# Residual variation left for the state-fixed-effects specifications in
# Appendix Table A.1: what remains after each set of fixed effects, at the
# state-year level, so the text can say how much identifying variation the
# within-state estimates actually use.
resid_sd <- function(fml) {
  fit <- tryCatch(feols(fml, data = cells), error = function(e) NULL)
  if (is.null(fit)) stop("feols failed for residual SD", call. = FALSE)
  sd(resid(fit))
}
resid_sd_region_year <- resid_sd(lw_index ~ 1 | region^year)
resid_sd_state_region_year <- resid_sd(lw_index ~ 1 | statefip + region^year)
resid_sd_state_year <- resid_sd(lw_index ~ 1 | statefip + year)

results <- data.table(
  run_id = run_id,
  statistic = c(
    "n_state_year_cells", "n_states", "n_years",
    "sd_state_year_cells",
    "sd_between_states_of_state_means",
    "sd_within_state_over_time",
    "share_variance_between_states",
    "mean_within_year_sd_across_states",
    "min_within_year_sd_across_states",
    "max_within_year_sd_across_states",
    "sd_individual_level_children_unweighted",
    "sd_individual_level_children_weighted",
    "mean_individual_level_children_unweighted",
    "resid_sd_after_region_year_fe",
    "resid_sd_after_state_and_region_year_fe",
    "resid_sd_after_state_and_year_fe"
  ),
  value = c(
    nrow(cells), nrow(state_means), uniqueN(cells$year),
    sqrt(overall_var),
    sqrt(between_var),
    sqrt(within_var),
    between_var / overall_var,
    mean(year_sd$sd_across_states),
    min(year_sd$sd_across_states),
    max(year_sd$sd_across_states),
    sd(dat$lw_index),
    sqrt(sum(dat$weight * (dat$lw_index - weighted.mean(dat$lw_index, dat$weight))^2) / sum(dat$weight)),
    mean(dat$lw_index),
    resid_sd_region_year,
    resid_sd_state_region_year,
    resid_sd_state_year
  )
)
write.csv(results,
          file.path(tables_dir, "bias-index-variance-decomposition.csv"),
          row.names = FALSE)
cat(sprintf("Bias index variance decomposition (run %s)\n", run_id))
print(results[, .(statistic, value = signif(value, 4))])
