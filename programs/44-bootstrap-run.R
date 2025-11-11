# ================================================================
# Bootstrap runner: fits multinomial models, performs bootstraps,
# and stores draws/metadata for downstream plotting.
# ================================================================

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(parallel)
  library(broom)
  library(tidyr)
  library(boot)
})

# ---------------------------
# Configuration
# ---------------------------
N_BOOTSTRAP <- getOption("bootstrap_n", 1000)  # raise (e.g., 500–1000) for production runs
detected_cores <- suppressWarnings(as.integer(detectCores()))
if (is.na(detected_cores) || detected_cores < 1) {
  detected_cores <- 1
}
N_CORES     <- getOption("bootstrap_cores", max(1, detected_cores - 1))
PP_VARS     <- c("value", "Female", "MomGradCollege", "DadGradCollege")
GRID_STEP   <- 0.5
ME_VARS     <- c("value","Female","MomGradCollege","DadGradCollege")

# plotting safeguards used for summaries (PP/ME CIs)
EPS_PP <- getOption("bootstrap_eps_pp", 0.002)   # enlarge collapsed PP CI by ±epsilon (probability scale)
EPS_ME <- getOption("bootstrap_eps_me", 1e-4)    # enlarge collapsed ME CI by ±epsilon

VERBOSE <- isTRUE(getOption("bootstrap_verbose", FALSE))
SHOW_PROGRESS <- isTRUE(getOption("bootstrap_show_progress", TRUE))
PROGRESS_EVERY <- max(1L, as.integer(getOption("bootstrap_progress_every", 1L)))

`%||%` <- function(x, y) if (!is.null(x)) x else y

.log_info <- function(...) {
  if (VERBOSE) message(...)
}

stopifnot(exists("git_mdir"), exists("git_mdir"))
if (!dir.exists(git_mdir)) dir.create(git_mdir, recursive = TRUE)

.safe_save_rds <- function(object, path) {
  tryCatch({
    saveRDS(object, path)
    TRUE
  }, error = function(e) {
    warning("Failed to save checkpoint: ", path, " — ", conditionMessage(e))
    FALSE
  })
}

.checkpoint_paths <- function(label_slug, output_dir = git_mdir) {
  list(
    rds = file.path(output_dir, sprintf("boot_results_%s.rds", label_slug)),
    metadata = file.path(output_dir, sprintf("boot_results_%s.txt", label_slug))
  )
}

.write_metadata <- function(path, info) {
  lines <- sprintf("%s: %s", names(info), unlist(info))
  tryCatch(writeLines(lines, con = path), error = function(e) {
    warning("Failed to write metadata: ", path, " — ", conditionMessage(e))
  })
}

# ---------------------------
# Progress helpers
# ---------------------------
create_boot_progress_tracker <- function(total, model_label, var_label, stage){
  if (!SHOW_PROGRESS) return(NULL)
  env <- new.env(parent = emptyenv())
  env$count <- 0L
  env$total <- total
  env$model_label <- model_label
  env$var_label <- var_label
  env$stage <- stage
  env$every <- PROGRESS_EVERY
  env$skip_first <- TRUE
  env
}

boot_progress_tick <- function(env){
  if (!is.environment(env)) return(invisible(NULL))
  if (isTRUE(env$skip_first)) {
    env$skip_first <- FALSE
    return(invisible(NULL))
  }
  env$count <- env$count + 1L
  if (env$count %% env$every == 0L || env$count == env$total) {
    remaining <- max(env$total - env$count, 0L)
    message(sprintf(
      "[progress][%s] model=%s | variable=%s | completed=%d/%d | remaining=%d",
      env$stage, env$model_label, env$var_label, env$count, env$total, remaining
    ))
  }
  invisible(NULL)
}

# ---------------------------
# Load & prepare data
# ---------------------------
CPS_IAT <- readr::read_csv(file.path(git_mdir, "CPS_IAT_asian.csv.zip")) |>
  rename(value = lw_index) |>
  mutate(
    OneAsian   = case_when(Grandparent_Type %in% c("AWWW","WAWW","WWAW","WWWA") ~ 1, TRUE ~ 0),
    TwoAsian   = case_when(Grandparent_Type %in% c("AAWW","AWAW","AWWA","WAAW","WWAA","WAWA") ~ 1, TRUE ~ 0),
    ThreeAsian = case_when(Grandparent_Type %in% c("AAAW","AAWA","AWAA","WAAA") ~ 1, TRUE ~ 0),
    FourAsian  = case_when(Grandparent_Type == "AAAA" ~ 1, TRUE ~ 0)
  )

get_pp_vars <- function(data){
  base <- c("value","Female")
  if (all(c("MomGradCollege","DadGradCollege") %in% names(data))) base <- c(base,"MomGradCollege","DadGradCollege")
  if ("education" %in% names(data)) base <- c(base,"education")
  if ("income"    %in% names(data)) base <- c(base,"income")
  base
}
get_me_vars <- function(data) get_pp_vars(data)

categorize_race_multinomial <- function(race_code){
  case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only",
    race_code %in% c(803,804,809,813) ~ "Asian_and_White",
    TRUE ~ "Other"
  )
}

CPS_IAT_multinomial <- CPS_IAT |>
  mutate(identity_choice = factor(categorize_race_multinomial(race),
                                  levels=c("Asian_only","White_only","Asian_and_White"))) |>
  filter(identity_choice %in% levels(identity_choice),
         !is.na(value), !is.na(identity_choice))

if (!"weight" %in% names(CPS_IAT_multinomial)) CPS_IAT_multinomial$weight <- 1

if ("Age" %in% names(CPS_IAT_multinomial)) {
  CPS_IAT_multinomial <- CPS_IAT_multinomial |>
    mutate(Age_sq = Age^2, Age_cube = Age^3, Age_quad = Age^4)
}

CPS_IAT_multinomial <- CPS_IAT_multinomial |>
  mutate(region_year = interaction(region, year, drop = TRUE),
         identity_choice = relevel(identity_choice, ref = "Asian_only"))

if (VERBOSE) {
  message("Distribution of identity choices:")
  print(table(CPS_IAT_multinomial$identity_choice))
  print(prop.table(table(CPS_IAT_multinomial$identity_choice)))
}

# ---------------------------
# Model
# ---------------------------
fit_multinomial_model <- function(data_subset, generation="all", ancestry_filter=NULL){
  if (generation == "first") {
    fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
      frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
  } else if (generation == "second") {
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("AA_0bj","AW_0bj","WA_0bj")) {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + region_year
    }
  } else if (generation == "third") {
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("OneAsian","TwoAsian","ThreeAsian","FourAsian")) {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type + region_year
    }
  } else {
    fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
      frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj +
      FirstGen_Asian + SecondGen_Asian + region_year
  }
  .log_info(sprintf("[multinom] gen=%s | anc=%s", generation, if (is.null(ancestry_filter)) "none" else ancestry_filter))
  nnet::multinom(fml, data=data_subset, weights=data_subset$weight, trace=FALSE, na.action=na.exclude)
}

# Pre-fit models (helps early failure visibility)
mnl_all_gen    <- fit_multinomial_model(CPS_IAT_multinomial, "all")
mnl_first_gen  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(FirstGen_Asian==1), "first")
mnl_second_gen <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian==1), "second")
mnl_third_gen  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian==1),  "third")

# ---------------------------
# Bootstrap helpers
# ---------------------------
make_var_grid <- function(data_subset, var_name, grid_step=0.5){
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    tibble(x_val=c(0,1))
  } else {
    rng <- range(data_subset[[var_name]], na.rm=TRUE)
    x_breaks <- seq(floor(rng[1]/grid_step)*grid_step, ceiling(rng[2]/grid_step)*grid_step, by=grid_step)
    interior <- x_breaks[x_breaks > rng[1] & x_breaks < rng[2]]
    tibble(x_val = sort(unique(c(rng[1], interior, rng[2]))))
  }
}

representative_row <- function(data_subset){
  get_mode <- function(x){
    if (is.numeric(x)) return(mean(x, na.rm=TRUE))
    tb <- table(x); md <- names(tb)[which.max(tb)]
    if (is.factor(x)) factor(md, levels=levels(x)) else md
  }
  rep <- data_subset[1,, drop=FALSE]
  for (nm in names(data_subset)) {
    if (nm %in% c("identity_choice","weight")) next
    rep[[nm]] <- get_mode(data_subset[[nm]])
  }
  rep
}

set_var_column_from_grid <- function(nd, rep_row, var_name, grid_vals){
  col <- rep_row[[var_name]]
  if (is.factor(col)) {
    lv <- levels(col)
    nd[[var_name]] <- if (length(lv) < 2) factor(lv[1], levels=lv)
                      else factor(ifelse(grid_vals==1, lv[2], lv[1]), levels=lv)
  } else if (is.logical(col)) {
    nd[[var_name]] <- grid_vals == 1
  } else {
    nd[[var_name]] <- grid_vals
  }
  nd
}

newdata_for_grid <- function(data_subset, var_name, grid, rep_row=NULL){
  if (is.null(rep_row)) rep_row <- representative_row(data_subset)
  nd <- rep_row[rep(1, nrow(grid)), , drop=FALSE]
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    nd <- set_var_column_from_grid(nd, rep_row, var_name, grid$x_val)
  } else {
    nd[[var_name]] <- grid$x_val
  }
  nd
}

compute_AME <- function(model, data_subset, var_name, h_frac=0.1){
  d <- data_subset
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    d0 <- d; d1 <- d
    if (is.factor(d[[var_name]])) {
      lv <- levels(d[[var_name]])
      d0[[var_name]] <- factor(lv[1], levels=lv); d1[[var_name]] <- factor(lv[2], levels=lv)
    } else if (is.logical(d[[var_name]])) { d0[[var_name]] <- FALSE; d1[[var_name]] <- TRUE
    } else { d0[[var_name]] <- 0; d1[[var_name]] <- 1 }
    p0 <- predict(model, newdata=d0, type="probs")
    p1 <- predict(model, newdata=d1, type="probs")
    if (is.vector(p0)) { outcome_names <- model$lev; p0 <- matrix(p0, nrow=1); p1 <- matrix(p1, nrow=1); colnames(p0) <- colnames(p1) <- outcome_names }
    ame <- colMeans(p1 - p0, na.rm=TRUE)
  } else {
    h <- h_frac * sd(d[[var_name]], na.rm=TRUE)
    vmin <- min(d[[var_name]], na.rm=TRUE); vmax <- max(d[[var_name]], na.rm=TRUE)
    d_minus <- d; d_plus <- d
    d_minus[[var_name]] <- pmax(pmin(d[[var_name]] - h, vmax), vmin)
    d_plus [[var_name]] <- pmax(pmin(d[[var_name]] + h, vmax), vmin)
    p_minus <- predict(model, newdata=d_minus, type="probs")
    p_plus  <- predict(model, newdata=d_plus,  type="probs")
    if (is.vector(p_minus)) { outcome_names <- model$lev; p_minus <- matrix(p_minus, nrow=1); p_plus <- matrix(p_plus, nrow=1); colnames(p_minus) <- colnames(p_plus) <- outcome_names }
    ame <- colMeans((p_plus - p_minus)/(2*h), na.rm=TRUE)
  }
  data.frame(variable=var_name, outcome=names(ame), ame=ame, row.names=NULL, stringsAsFactors=FALSE)
}

# --- BOOT: predictions for PP
boot_pp_stat <- function(data, indices, var_name, grid, generation, ancestry_filter=NULL, progress_env=NULL){
  boot_progress_tick(progress_env)
  d <- data[indices, ]
  model <- tryCatch(fit_multinomial_model(d, generation, ancestry_filter), error=function(e) NULL)
  if (is.null(model)) {
    n_outcomes <- length(levels(d$identity_choice))
    n_grid <- nrow(grid)
    return(rep(NA_real_, n_outcomes*n_grid))
  }
  rep_row <- representative_row(d); nd <- newdata_for_grid(d, var_name, grid, rep_row)
  pred <- predict(model, newdata=nd, type="probs")
  if (is.vector(pred)) { outcome_names <- model$lev; pred <- matrix(pred, nrow=1); colnames(pred) <- outcome_names }
  # IMPORTANT: keep column-major order (no transpose)
  as.vector(pred)
}

# --- BOOT: AME
boot_me_stat <- function(data, indices, var_name, generation, ancestry_filter=NULL, progress_env=NULL){
  boot_progress_tick(progress_env)
  d <- data[indices, ]
  model <- tryCatch(fit_multinomial_model(d, generation, ancestry_filter), error=function(e) NULL)
  if (is.null(model)) { n_outcomes <- length(levels(d$identity_choice)); return(rep(NA_real_, n_outcomes)) }
  compute_AME(model, d, var_name)$ame
}

# --- Non-centered percentile CIs (PP & ME) ---
bootstrap_multinom_boot <- function(data_subset, generation="all", ancestry_filter=NULL,
                                   pp_vars=c("value"), pp_grid_step=0.5,
                                   me_vars=c("value"), B=500, n_cores=1,
                                   stratify_by=NULL, seed=2024,
                                   progress_label=NULL){

  set.seed(seed); .log_info("Starting bootstrap with boot package (percentile CIs; NOT centered)...")
  if (!exists("EPS_PP")) EPS_PP <- 0.002
  if (!exists("EPS_ME")) EPS_ME <- 1e-4

  .clamp01 <- function(x) pmax(0, pmin(1, x))

  pp_results <- list(); me_results <- list()
  pp_valid_draws <- list(); me_valid_draws <- list()
  strata <- if (!is.null(stratify_by)) data_subset[[stratify_by]] else NULL
  boot_parallel <- if (.Platform$OS.type == "windows") "no" else "multicore"
  n_cpus <- if (boot_parallel=="no") 1 else n_cores
  if (SHOW_PROGRESS && boot_parallel != "no") {
    .log_info("Progress requested; falling back to sequential bootstrap execution.")
    boot_parallel <- "no"
    n_cpus <- 1
  }
  outcome_levels <- levels(data_subset$identity_choice)
  rep_row_full <- representative_row(data_subset)
  main_model <- tryCatch(
    fit_multinomial_model(data_subset, generation, ancestry_filter),
    error = function(e) {
      warning("Failed to fit main model for fallback predictions: ", conditionMessage(e))
      NULL
    }
  )
  model_label <- progress_label %||% sprintf("gen=%s|anc=%s", generation, if (is.null(ancestry_filter)) "none" else ancestry_filter)

  # ==============
  # Predicted probabilities (percentile CI, no centering)
  # ==============
  for (var_name in pp_vars){
    .log_info(sprintf("[BOOT][PP] var=%s | gen=%s | anc=%s | R=%d", var_name, generation,
                if (is.null(ancestry_filter)) "none" else ancestry_filter, B))

    grid <- make_var_grid(data_subset, var_name, pp_grid_step)
    nd <- newdata_for_grid(data_subset, var_name, grid, rep_row_full)

    progress_env <- create_boot_progress_tracker(B, model_label, var_name, "PP")
    boot_results <- boot(data=data_subset, statistic=boot_pp_stat, R=B, sim="ordinary", stype="i",
                         parallel=boot_parallel, ncpus=n_cpus, strata=strata,
                         var_name=var_name, grid=grid, generation=generation,
                         ancestry_filter=ancestry_filter, progress_env=progress_env)

    # Main model (for names/fallback)
    if (!is.null(main_model)) {
      main_pred <- predict(main_model, newdata=nd, type="probs")
      if (is.vector(main_pred)) {
        outcome_names <- main_model$lev
        main_pred <- matrix(main_pred, nrow=1)
        colnames(main_pred) <- outcome_names
      }
      main_df <- as.data.frame(main_pred) |>
        dplyr::mutate(x_val = grid$x_val) |>
        tidyr::pivot_longer(cols = -x_val, names_to="outcome", values_to="p_main_fallback")
    } else {
      outcome_names <- outcome_levels
      main_pred <- matrix(NA_real_, nrow = nrow(grid), ncol = length(outcome_names))
      colnames(main_pred) <- outcome_names
      main_df <- tidyr::expand_grid(
        x_val = grid$x_val,
        outcome = outcome_levels
      ) |>
        dplyr::mutate(p_main_fallback = NA_real_)
    }

    if (NROW(boot_results$t) > 0){
      valid_rows <- !apply(is.na(boot_results$t), 1, all)
      pp_valid_draws[[var_name]] <- sum(valid_rows)

      if (sum(valid_rows) > 0){
        boot_matrix_all <- boot_results$t[valid_rows,, drop=FALSE]
        n_grid <- nrow(grid); n_outcomes <- ncol(main_pred); outcome_names <- colnames(main_pred)

        # rebuild matrices in column-major order (NO transpose)
        boot_long <- vector("list", length=nrow(boot_matrix_all))
        for (i in seq_len(nrow(boot_matrix_all))){
          boot_vec <- boot_matrix_all[i, ]
          boot_mat <- matrix(boot_vec, nrow=n_grid, ncol=n_outcomes, byrow=FALSE)
          colnames(boot_mat) <- outcome_names
          boot_long[[i]] <- as.data.frame(boot_mat) |>
            dplyr::mutate(x_val=grid$x_val, iter=i) |>
            tidyr::pivot_longer(cols = -c(x_val, iter), names_to="outcome", values_to="prob")
        }
        boot_long <- dplyr::bind_rows(boot_long)

        # Percentile CI (always in [0,1]); dot = bootstrap median
        ci_df <- boot_long |>
          dplyr::group_by(x_val, outcome) |>
          dplyr::summarize(
            p_main = stats::median(prob, na.rm=TRUE),
            p_lo   = stats::quantile(prob, 0.025, na.rm=TRUE),
            p_hi   = stats::quantile(prob, 0.975, na.rm=TRUE),
            .groups="drop"
          ) |>
          dplyr::mutate(
            p_main   = .clamp01(p_main),
            p_lo_plot = .clamp01(p_lo),
            p_hi_plot = .clamp01(p_hi),
            # ensure visible if collapsed
            p_lo_plot = ifelse(abs(p_hi_plot - p_lo_plot) < 1e-12, .clamp01(p_main - EPS_PP), p_lo_plot),
            p_hi_plot = ifelse(abs(p_hi_plot - p_lo_plot) < 1e-12, .clamp01(p_main + EPS_PP), p_hi_plot)
          ) |>
          dplyr::select(x_val, outcome, p_main, p_lo_plot, p_hi_plot)

        pp_df <- main_df |>
          dplyr::select(x_val, outcome, p_main_fallback) |>
          dplyr::left_join(ci_df, by=c("x_val","outcome")) |>
          dplyr::mutate(
            p_main = ifelse(is.na(p_main), p_main_fallback, p_main),
            variable = var_name
          ) |>
          dplyr::select(x_val, outcome, variable, p_main, p_lo_plot, p_hi_plot)

        pp_results[[var_name]] <- pp_df
      } else {
        pp_results[[var_name]] <- main_df |>
          dplyr::transmute(x_val, outcome, variable = var_name,
                           p_main = .clamp01(p_main_fallback),
                           p_lo_plot = .clamp01(p_main_fallback - EPS_PP),
                           p_hi_plot = .clamp01(p_main_fallback + EPS_PP))
      }
    } else {
      pp_results[[var_name]] <- main_df |>
        dplyr::transmute(x_val, outcome, variable = var_name,
                         p_main = .clamp01(p_main_fallback),
                         p_lo_plot = .clamp01(p_main_fallback - EPS_PP),
                         p_hi_plot = .clamp01(p_main_fallback + EPS_PP))
    }
  }

  # ==========
  # Marginal Effects (percentile CI, no centering)
  # ==========
  for (var_name in me_vars){
    .log_info(sprintf("[BOOT][ME] var=%s | gen=%s | anc=%s | R=%d", var_name, generation,
                if (is.null(ancestry_filter)) "none" else ancestry_filter, B))

    progress_env <- create_boot_progress_tracker(B, model_label, var_name, "ME")
    boot_results <- boot(data=data_subset, statistic=boot_me_stat, R=B, sim="ordinary", stype="i",
                         parallel=boot_parallel, ncpus=n_cpus, strata=strata,
                         var_name=var_name, generation=generation,
                         ancestry_filter=ancestry_filter, progress_env=progress_env)

    main_ame <- if (!is.null(main_model)) {
      tryCatch(
        compute_AME(main_model, data_subset, var_name),
        error = function(e) {
          warning("Failed to compute fallback AME for ", var_name, ": ", conditionMessage(e))
          tibble::tibble(variable = var_name, outcome = outcome_levels, ame = NA_real_)
        }
      )
    } else {
      tibble::tibble(variable = var_name, outcome = outcome_levels, ame = NA_real_)
    }  # variable, outcome, ame

    if (NROW(boot_results$t) > 0){
      valid_rows <- !apply(is.na(boot_results$t), 1, all)
      me_valid_draws[[var_name]] <- sum(valid_rows)

      if (sum(valid_rows) > 0){
        bm <- boot_results$t[valid_rows,, drop=FALSE]
        me_df <- tibble::tibble(
          outcome = main_ame$outcome,
          ame_point = apply(bm, 2, stats::median, na.rm=TRUE),
          ame_lo    = apply(bm, 2, stats::quantile, probs=0.025, na.rm=TRUE),
          ame_hi    = apply(bm, 2, stats::quantile, probs=0.975, na.rm=TRUE)
        ) |>
        dplyr::mutate(
          ame_lo_plot = ifelse(abs(ame_hi - ame_lo) < 1e-12, ame_point - EPS_ME, ame_lo),
          ame_hi_plot = ifelse(abs(ame_hi - ame_lo) < 1e-12, ame_point + EPS_ME, ame_hi)
        )

        me_results[[var_name]] <- main_ame |>
          dplyr::select(variable, outcome) |>
          dplyr::left_join(me_df, by="outcome") |>
          dplyr::select(variable, outcome, ame_point, ame_lo_plot, ame_hi_plot)
      } else {
        me_results[[var_name]] <- main_ame |>
          dplyr::transmute(variable, outcome,
                           ame_point = ame,
                           ame_lo_plot = ame - EPS_ME,
                           ame_hi_plot = ame + EPS_ME)
      }
    } else {
      me_results[[var_name]] <- main_ame |>
        dplyr::transmute(variable, outcome,
                         ame_point = ame,
                         ame_lo_plot = ame - EPS_ME,
                         ame_hi_plot = ame + EPS_ME)
    }
  }

  list(
    pp = if (length(pp_results)) dplyr::bind_rows(pp_results) else tibble::tibble(),
    me = if (length(me_results)) dplyr::bind_rows(me_results) else tibble::tibble(),
    B_eff = list(
      pp = if (length(pp_valid_draws)) min(unlist(pp_valid_draws)) else 0L,
      me = if (length(me_valid_draws)) min(unlist(me_valid_draws)) else 0L
    )
  )
}

# ---------------------------
# Run + save results (no plotting)
# ---------------------------
run_boot_and_save <- function(data_subset, generation, ancestry_filter, label_slug, label_pretty,
                              pp_vars=NULL, grid_step=GRID_STEP, me_vars=NULL,
                              B=N_BOOTSTRAP, n_cores=N_CORES, seed=2024,
                              save_checkpoint=TRUE, resume=FALSE, overwrite=FALSE){
  if (is.null(pp_vars)) pp_vars <- get_pp_vars(data_subset)
  if (is.null(me_vars)) me_vars <- get_me_vars(data_subset)

  paths <- .checkpoint_paths(label_slug)
  ancestry_lbl <- if (is.null(ancestry_filter)) "none" else ancestry_filter

  if (resume && file.exists(paths$rds) && !overwrite) {
    .log_info(sprintf("Checkpoint found for %s; loading cached results.", label_pretty))
    boot_res <- readRDS(paths$rds)
  } else {
    .log_info(sprintf("=== Bootstrapping block: %s (gen=%s | anc=%s) ===",
                label_pretty, generation, ancestry_lbl))
    .log_info(sprintf("PP variables: %s", paste(pp_vars, collapse=", ")))
    .log_info(sprintf("ME variables: %s", paste(me_vars, collapse=", ")))

    boot_res <- bootstrap_multinom_boot(
      data_subset=data_subset, generation=generation, ancestry_filter=ancestry_filter,
      pp_vars=pp_vars, pp_grid_step=grid_step, me_vars=me_vars,
      B=B, n_cores=n_cores, stratify_by="region_year", seed=seed,
      progress_label=label_pretty
    )
    .log_info(sprintf("Effective draws — PP: %s, ME: %s",
                as.integer(boot_res$B_eff$pp), as.integer(boot_res$B_eff$me)))

    if (save_checkpoint) {
      saved <- .safe_save_rds(boot_res, paths$rds)
      if (saved) {
        metadata <- list(
          label = label_pretty,
          label_slug = label_slug,
          generation = generation,
          ancestry_filter = ancestry_lbl,
          seed = seed,
          B_requested = B,
          B_eff_pp = boot_res$B_eff$pp,
          B_eff_me = boot_res$B_eff$me,
          n_cores = n_cores,
          pp_vars = paste(pp_vars, collapse = ", "),
          me_vars = paste(me_vars, collapse = ", "),
          timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
        .write_metadata(paths$metadata, metadata)
      }
    }
  }

  invisible(boot_res)
}

# ---------- All generations ----------
boot_all <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial,
  generation  = "all",
  ancestry_filter = NULL,
  label_slug  = "all_generations",
  label_pretty = "All generations",
  seed = 2024
)

# ---------- Second generation (overall) ----------
boot_second <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian==1),
  generation  = "second",
  ancestry_filter = NULL,
  label_slug  = "second_generation",
  label_pretty = "Second generation",
  seed = 2026
)


# ---------- Third generation (overall) ----------
boot_third <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian==1),
  generation  = "third",
  ancestry_filter = NULL,
  label_slug  = "third_generation",
  label_pretty = "Third generation",
  seed = 2027
)

# ---------- Second generation subgroups ----------
boot_second_aa <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian==1, AA_0bj==1),
  generation  = "second", ancestry_filter = "AA_0bj",
  label_slug  = "second_AA", label_pretty = "Second gen: AA parents",
  seed = 2028
)
boot_second_aw <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian==1, AW_0bj==1),
  generation  = "second", ancestry_filter = "AW_0bj",
  label_slug  = "second_AW", label_pretty = "Second gen: AW parents",
  seed = 2029
)
boot_second_wa <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian==1, WA_0bj==1),
  generation  = "second", ancestry_filter = "WA_0bj",
  label_slug  = "second_WA", label_pretty = "Second gen: WA parents",
  seed = 2030
)

# ---------- Third generation subgroups ----------
boot_third_one <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, OneAsian==1),
  generation  = "third", ancestry_filter = "OneAsian",
  label_slug  = "third_oneAsian", label_pretty = "Third gen: One Asian grandparent",
  seed = 2031
  )
boot_third_two <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, TwoAsian==1),
  generation  = "third", ancestry_filter = "TwoAsian",
  label_slug  = "third_twoAsian", label_pretty = "Third gen: Two Asian grandparents",
  seed = 2032
  )

boot_third_three <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, ThreeAsian==1),
  generation  = "third", ancestry_filter = "ThreeAsian",
  label_slug  = "third_threeAsian", label_pretty = "Third gen: Three Asian grandparents",
  seed = 2033
  )
boot_third_four <- run_boot_and_save(
  data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, FourAsian==1),
  generation  = "third", ancestry_filter = "FourAsian",
  label_slug  = "third_fourAsian", label_pretty = "Third gen: Four Asian grandparents",
  seed = 2034
)

.log_info(sprintf("All bootstrap results saved in: %s", git_mdir))
.log_info("Set options bootstrap_show_progress = FALSE to silence iteration updates.")
