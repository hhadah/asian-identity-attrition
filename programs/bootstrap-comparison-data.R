# Validated child-bootstrap comparisons
# Date: November 5th, 2026
# Reads saved summaries and checks the comparison CSV; never fits a model,
# bootstraps, plots, or writes a file. Shared by scripts 49 and 54.
# Trap (Nov 2026): the datasets-folder CSV came from 50 replications,
# whereas the marginal-effect figures used the preserved 1,000-replication
# run. Read only the checkpoint directory and verify both B and every row.

read_bootstrap_comparisons <- function(results_dir, expected_B = 1000L) {
  require_true <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
  }
  require_true(requireNamespace("digest", quietly = TRUE),
               "Package digest is required for bootstrap provenance hashes.")
  results_dir <- normalizePath(results_dir, mustWork = TRUE)
  variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")
  outcomes <- c("Asian_only", "White_only", "Asian_and_White")
  labels <- c(second_AW = "Second gen: AW", second_WA = "Second gen: WA",
              third_oneAsian = "Third gen: 1 Asian GP",
              third_twoAsian = "Third gen: 2 Asian GP",
              third_threeAsian = "Third gen: 3 Asian GP")
  pairs <- list(c("second_AW", "second_WA"),
                c("third_oneAsian", "third_twoAsian"),
                c("third_oneAsian", "third_threeAsian"),
                c("third_twoAsian", "third_threeAsian"))
  cell_key <- function(x) paste(x$variable, x$outcome, sep = "|")
  row_key <- function(x) paste(x$model_a, x$model_b, cell_key(x), sep = "|")
  cells <- expand.grid(outcome = outcomes, variable = variables,
                       stringsAsFactors = FALSE)
  wanted_cells <- cell_key(cells)
  summaries <- list()
  checkpoints <- list()

  for (slug in names(labels)) {
    rds <- file.path(results_dir, sprintf("boot_results_%s.rds", slug))
    txt <- file.path(results_dir, sprintf("boot_results_%s.txt", slug))
    require_true(file.exists(rds) && file.exists(txt),
                 sprintf("Missing checkpoint or metadata for %s", slug))
    lines <- readLines(txt, warn = FALSE)
    colon <- regexpr(": ", lines, fixed = TRUE)
    require_true(all(colon > 0L), sprintf("Malformed metadata: %s", txt))
    fields <- substr(lines, 1L, colon - 1L)
    require_true(!anyDuplicated(fields), sprintf("Duplicate metadata: %s", txt))
    meta <- setNames(trimws(substring(lines, colon + 2L)), fields)
    required <- c("label_slug", "B_requested", "B_eff_me", "seed",
                  "n_cores", "timestamp_utc")
    require_true(all(required %in% names(meta)),
                 sprintf("Incomplete metadata: %s", txt))
    require_true(identical(unname(meta[["label_slug"]]), slug),
                 sprintf("Checkpoint label mismatch: %s", txt))
    obj <- readRDS(rds)
    counts <- suppressWarnings(as.integer(c(meta[["B_requested"]],
                                            meta[["B_eff_me"]], obj$B_eff$me)))
    require_true(length(counts) == 3L && all(counts == expected_B),
                 sprintf("%s: require %d requested/effective replications; got %s",
                         slug, expected_B, paste(counts, collapse = "/")))
    me <- obj$me
    require_true(is.data.frame(me) &&
                   all(c("variable", "outcome", "ame_point", "ame_lo_plot",
                         "ame_hi_plot") %in% names(me)),
                 sprintf("Invalid marginal-effect summary: %s", rds))
    require_true(nrow(me) == length(wanted_cells) &&
                   !anyDuplicated(cell_key(me)) &&
                   setequal(cell_key(me), wanted_cells),
                 sprintf("Unexpected variable/outcome cells: %s", rds))
    numeric_cols <- c("ame_point", "ame_lo_plot", "ame_hi_plot")
    require_true(all(vapply(me[numeric_cols], is.numeric, logical(1L))) &&
                   all(is.finite(as.matrix(me[numeric_cols]))) &&
                   all(me$ame_hi_plot > me$ame_lo_plot),
                 sprintf("Invalid estimates or percentile intervals: %s", rds))
    summaries[[slug]] <- me[match(wanted_cells, cell_key(me)), ]
    checkpoints[[slug]] <- list(
      label = labels[[slug]], B_requested = counts[[1L]],
      B_effective = counts[[2L]], seed = as.integer(meta[["seed"]]),
      n_cores = as.integer(meta[["n_cores"]]),
      timestamp_utc = meta[["timestamp_utc"]],
      rds = basename(rds), rds_sha256 = digest::digest(file = rds, algo = "sha256"),
      metadata = basename(txt),
      metadata_sha256 = digest::digest(file = txt, algo = "sha256"))
  }

  # Independent reconstruction checks that the CSV belongs to these summaries.
  # These are the historical normal-approximation tests, not new draw-level tests.
  expected <- lapply(pairs, function(pair) {
    a <- summaries[[pair[[1L]]]]
    b <- summaries[[pair[[2L]]]]
    diff_est <- a$ame_point - b$ame_point
    se_a <- (a$ame_hi_plot - a$ame_lo_plot) / (2 * 1.96)
    se_b <- (b$ame_hi_plot - b$ame_lo_plot) / (2 * 1.96)
    se_diff <- sqrt(se_a^2 + se_b^2)
    z_stat <- diff_est / se_diff
    p_value <- 2 * stats::pnorm(-abs(z_stat))
    data.frame(variable = a$variable, outcome = a$outcome,
               model_a = labels[[pair[[1L]]]], model_b = labels[[pair[[2L]]]],
               ame_a = a$ame_point, ame_b = b$ame_point,
               diff_est = diff_est, se_diff = se_diff, z_stat = z_stat,
               p_value = p_value,
               sig = ifelse(p_value < 0.01, "***",
                            ifelse(p_value < 0.05, "**",
                                   ifelse(p_value < 0.1, "*", ""))),
               stringsAsFactors = FALSE)
  })
  expected <- do.call(rbind, expected)
  csv <- file.path(results_dir, "boot_me_between_model_tests.csv")
  require_true(file.exists(csv), sprintf("Missing comparison CSV: %s", csv))
  actual <- read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)
  require_true(identical(names(actual), names(expected)) &&
                 nrow(actual) == nrow(expected) &&
                 !anyDuplicated(row_key(actual)) &&
                 setequal(row_key(actual), row_key(expected)),
               "Comparison CSV has missing, duplicate, or unexpected cells.")
  actual <- actual[match(row_key(expected), row_key(actual)), ]
  numeric_cols <- c("ame_a", "ame_b", "diff_est", "se_diff", "z_stat", "p_value")
  require_true(all(vapply(actual[numeric_cols], is.numeric, logical(1L))) &&
                 all(is.finite(as.matrix(actual[numeric_cols]))),
               "Comparison CSV contains non-finite or non-numeric statistics.")
  error <- max(abs(as.matrix(actual[numeric_cols]) -
                     as.matrix(expected[numeric_cols])))
  require_true(error < 1e-12 && identical(actual$sig, expected$sig),
               sprintf("Comparison CSV does not match %d-replication summaries (max error %.6g).",
                       expected_B, error))
  rownames(actual) <- NULL
  list(data = actual, provenance = list(
    replications = expected_B, comparison_rows = nrow(actual),
    checkpoints = checkpoints, csv = basename(csv),
    csv_sha256 = digest::digest(file = csv, algo = "sha256"),
    maximum_numeric_reconstruction_error = error,
    estimator = "Difference between model-specific median AMEs",
    inference = paste("Normal approximation: each model SE is its 95% percentile",
                      "interval width / 3.92; difference SE combines the two",
                      "model SEs in quadrature, assuming independent samples."),
    state_clustered = FALSE, raw_replication_draws_available = FALSE))
}
