# =========================
# BOOTSTRAP INFRASTRUCTURE
# =========================

# --- 1A. utilities to create grids and predict ---
make_var_grid <- function(data_subset, var_name, grid_step = 0.5) {
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    tibble(x_val = c(0, 1))
  } else {
    vrng <- range(data_subset[[var_name]], na.rm = TRUE)
    vmin <- vrng[1]; vmax <- vrng[2]
    x_breaks <- seq(floor(vmin / grid_step) * grid_step,
                    ceiling(vmax / grid_step) * grid_step,
                    by = grid_step)
    interior <- x_breaks[x_breaks > vmin & x_breaks < vmax]
    tibble(x_val = sort(unique(c(vmin, interior, vmax))))
  }
}

# Representative row for PP curves (kept only to hold others fixed);
# AMEs will average across the *full data* instead.
representative_row <- function(data_subset) {
  get_rep_value <- function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else {
    tbl <- table(x); names(tbl)[which.max(tbl)]
  }
  rep <- data_subset[1, , drop = FALSE]
  for (nm in names(data_subset)) {
    if (nm %in% c("identity_choice","weight")) next
    rep[[nm]] <- get_rep_value(data_subset[[nm]])
  }
  rep
}

# Build newdata for PP along var grid
newdata_for_grid <- function(data_subset, var_name, grid, rep_row = NULL) {
  if (is.null(rep_row)) rep_row <- representative_row(data_subset)
  nd <- rep_row[rep(1, nrow(grid)), , drop = FALSE]
  nd[[var_name]] <- grid$x_val
  nd
}

# --- 1B. AME calculator (average over sample) ---
# For binaries: discrete 0->1 change; for continuous: central difference with h = 0.1*SD
compute_AME <- function(model, data_subset, var_name, h_frac = 0.1) {
  d <- data_subset
  # ensure factors in newdata carry same levels as training
  # (nnet::multinom handles this if columns are factors in d)

  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    d0 <- d; d1 <- d
    d0[[var_name]] <- 0; d1[[var_name]] <- 1
    p0 <- predict(model, newdata = d0, type = "probs")
    p1 <- predict(model, newdata = d1, type = "probs")
    me_mat <- p1 - p0  # N x K
  } else {
    x <- d[[var_name]]
    sd_x <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sd_x) || sd_x == 0) sd_x <- 0.1  # fallback
    h <- h_frac * sd_x
    d_lo <- d; d_hi <- d
    d_lo[[var_name]] <- x - h
    d_hi[[var_name]] <- x + h
    p_lo <- predict(model, newdata = d_lo, type = "probs")
    p_hi <- predict(model, newdata = d_hi, type = "probs")
    me_mat <- (p_hi - p_lo) / (2 * h)  # N x K derivatives
  }
  # Average across observations (AME):
  ame <- colMeans(me_mat, na.rm = TRUE)
  tibble(variable = var_name,
         outcome  = colnames(me_mat),
         ame      = as.numeric(ame))
}

# --- 1C. Single bootstrap iteration ---
boot_once <- function(data_subset, generation, ancestry_filter = NULL,
                      var_for_pp = "value", pp_grid_step = 0.5,
                      me_vars = c("value","Female","MomGradCollege","DadGradCollege"),
                      stratify_by = "region_year",
                      seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Optional: stratified resampling to preserve region_year composition
  if (!is.null(stratify_by) && stratify_by %in% names(data_subset)) {
    d_split <- split(data_subset, data_subset[[stratify_by]])
    resampled <- lapply(d_split, function(df) df[sample.int(nrow(df), nrow(df), replace = TRUE), , drop = FALSE])
    d_boot <- bind_rows(resampled)
  } else {
    d_boot <- data_subset[sample.int(nrow(data_subset), nrow(data_subset), replace = TRUE), , drop = FALSE]
  }

  # Refit
  mod <- try(fit_multinomial_model(d_boot, generation, ancestry_filter), silent = TRUE)
  if (inherits(mod, "try-error")) return(NULL)

  # Predicted probabilities along grid for var_for_pp
  grid <- make_var_grid(d_boot, var_for_pp, grid_step = pp_grid_step)
  nd   <- newdata_for_grid(d_boot, var_for_pp, grid)
  probs <- predict(mod, newdata = nd, type = "probs")
  if (is.vector(probs)) {  # edge case: 1-row matrix
    probs <- matrix(probs, nrow = 1)
    colnames(probs) <- mod$lev
  }
  pp_df <- as_tibble(probs)
  pp_df$x_val <- grid$x_val
  pp_df <- tidyr::pivot_longer(pp_df, -x_val, names_to = "outcome", values_to = "p_hat")

  # AMEs for selected variables (averaged over sample)
  me_list <- lapply(me_vars, function(v) compute_AME(mod, d_boot, v))
  me_df <- bind_rows(me_list)

  list(pp = pp_df, me = me_df)
}

# --- 1D. Bootstrap driver (parallel) ---
bootstrap_multinom <- function(data_subset, generation, ancestry_filter = NULL,
                               var_for_pp = "value", pp_grid_step = 0.5,
                               me_vars = c("value","Female","MomGradCollege","DadGradCollege"),
                               B = N_BOOTSTRAP, n_cores = N_CORES,
                               stratify_by = "region_year", seed = 2024) {

  # First, compute point estimates on the *original* sample
  model_main <- fit_multinomial_model(data_subset, generation, ancestry_filter)
  grid_main  <- make_var_grid(data_subset, var_for_pp, grid_step = pp_grid_step)
  nd_main    <- newdata_for_grid(data_subset, var_for_pp, grid_main)
  probs_main <- predict(model_main, newdata = nd_main, type = "probs")
  if (is.vector(probs_main)) {
    probs_main <- matrix(probs_main, nrow = 1); colnames(probs_main) <- model_main$lev
  }
  pp_main <- as_tibble(probs_main)
  pp_main$x_val <- grid_main$x_val
  pp_main <- tidyr::pivot_longer(pp_main, -x_val, names_to = "outcome", values_to = "p_hat")

  me_main <- bind_rows(lapply(me_vars, function(v) compute_AME(model_main, data_subset, v)))

  # Parallel bootstrap
  set.seed(seed)
  seeds <- sample.int(1e7, B)

  # Use mclapply on unix; fallback to parLapply if needed
  use_mc <- .Platform$OS.type != "windows"
  if (use_mc) {
    res_list <- parallel::mclapply(seq_len(B), function(b)
      boot_once(data_subset, generation, ancestry_filter,
                var_for_pp, pp_grid_step, me_vars, stratify_by, seeds[b]),
      mc.cores = n_cores)
  } else {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, varlist = c("fit_multinomial_model","make_var_grid","newdata_for_grid",
                                            "representative_row","compute_AME","boot_once"),
                            envir = environment())
    res_list <- parallel::parLapply(cl, seq_len(B), function(b)
      boot_once(data_subset, generation, ancestry_filter,
                var_for_pp, pp_grid_step, me_vars, stratify_by, seeds[b]))
  }

  # Drop any failed iterations
  res_list <- Filter(Negate(is.null), res_list)
  if (length(res_list) == 0) stop("All bootstrap iterations failed to converge.")

  # Stack bootstrap draws
  pp_all <- bind_rows(lapply(res_list, `[[`, "pp"), .id = "draw")
  me_all <- bind_rows(lapply(res_list, `[[`, "me"), .id = "draw")

  # Summarize PP: mean and percentile CIs at each x, outcome
  pp_summ <- pp_all |>
    group_by(x_val, outcome) |>
    summarise(
      p_mean = mean(p_hat, na.rm = TRUE),
      p_lo   = quantile(p_hat, 0.025, na.rm = TRUE),
      p_hi   = quantile(p_hat, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # add main-sample point prediction for the plotted line
    left_join(pp_main |> rename(p_main = p_hat), by = c("x_val","outcome"))

  # Summarize AME across draws
  me_summ <- me_all |>
    group_by(variable, outcome) |>
    summarise(
      ame_mean = mean(ame, na.rm = TRUE),
      ame_lo   = quantile(ame, 0.025, na.rm = TRUE),
      ame_hi   = quantile(ame, 0.975, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    left_join(me_main |> rename(ame_point = ame), by = c("variable","outcome"))

  list(pp = pp_summ, me = me_summ, B_eff = length(unique(pp_all$draw)))
}
