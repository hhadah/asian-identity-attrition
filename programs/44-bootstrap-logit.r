# ================================================================
# Multinomial (nnet::multinom) with BOOTSTRAP PP curves & AME plots
# End-to-end, self-contained
# ================================================================

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(parallel)
  library(broom)
  library(here)
  library(scales)
  library(tidyr)
  library(purrr)
})

# ---------------------------
# Configuration & fallbacks

# Bootstrap & compute parameters
N_BOOTSTRAP <- 1000                    # increased for stable CIs
N_CORES     <- max(1, detectCores() - 1)
PP_VARS     <- c("value", "Female", "MomGradCollege", "DadGradCollege")  # multiple PP variables
GRID_STEP   <- 0.5
ME_VARS     <- c("value","Female","MomGradCollege","DadGradCollege")

# ---------------------------
# Load & prepare data
# ---------------------------
CPS_IAT <- readr::read_csv(file.path(datasets, "CPS_IAT_asian.csv")) |>
  rename(value = lw_index) |>
  mutate(
    OneAsian = case_when(
      Grandparent_Type %in% c("AWWW","WAWW","WWAW","WWWA") ~ 1, TRUE ~ 0),
    TwoAsian = case_when(
      Grandparent_Type %in% c("AAWW","AWAW","AWWA","WAAW","WWAA","WAWA") ~ 1, TRUE ~ 0),
    ThreeAsian = case_when(
      Grandparent_Type %in% c("AAAW","AAWA","AWAA","WAAA") ~ 1, TRUE ~ 0),
    FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1, TRUE ~ 0)
  )

# Function to determine appropriate PP variables based on data
get_pp_vars <- function(data) {
  base_vars <- c("value", "Female")
  
  # Check if parental education variables exist (child sample)
  if ("MomGradCollege" %in% names(data) && "DadGradCollege" %in% names(data)) {
    base_vars <- c(base_vars, "MomGradCollege", "DadGradCollege")
  }
  
  # Check if adult-specific variables exist (adult sample)
  if ("education" %in% names(data)) {
    base_vars <- c(base_vars, "education")
  }
  if ("income" %in% names(data)) {
    base_vars <- c(base_vars, "income")
  }
  
  return(base_vars)
}

# Function to determine appropriate ME variables (same as PP vars for now)
get_me_vars <- function(data) {
  return(get_pp_vars(data))
}

categorize_race_multinomial <- function(race_code) {
  dplyr::case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only",
    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White",
    TRUE ~ "Other"
  )
}

CPS_IAT_multinomial <- CPS_IAT |>
  mutate(
    identity_choice = categorize_race_multinomial(race),
    identity_choice = factor(identity_choice,
                             levels = c("Asian_only","White_only","Asian_and_White"))
  ) |>
  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice))

# Fallbacks for variables occasionally missing in shared snippets
if (!"weight" %in% names(CPS_IAT_multinomial)) CPS_IAT_multinomial$weight <- 1
if (!"Age_sq" %in% names(CPS_IAT_multinomial))   CPS_IAT_multinomial <- CPS_IAT_multinomial |> mutate(Age_sq = Age^2)
if (!"Age_cube" %in% names(CPS_IAT_multinomial)) CPS_IAT_multinomial <- CPS_IAT_multinomial |> mutate(Age_cube = Age^3)
if (!"Age_quad" %in% names(CPS_IAT_multinomial)) CPS_IAT_multinomial <- CPS_IAT_multinomial |> mutate(Age_quad = Age^4)

CPS_IAT_multinomial <- CPS_IAT_multinomial |>
  mutate(region_year = interaction(region, year, drop = TRUE)) |>
  mutate(identity_choice = relevel(identity_choice, ref = "Asian_only"))

cat("Distribution of identity choices:\n")
print(table(CPS_IAT_multinomial$identity_choice))
print(prop.table(table(CPS_IAT_multinomial$identity_choice)))

# ---------------------------
# Fit multinomial models
# ---------------------------
fit_multinomial_model <- function(data_subset, generation = "all", ancestry_filter = NULL) {
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
  nnet::multinom(fml, data = data_subset, weights = data_subset$weight, trace = FALSE)
}

cat("Fitting multinomial models using nnet::multinom...\n")
mnl_all_gen    <- fit_multinomial_model(CPS_IAT_multinomial, "all")
mnl_first_gen  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(FirstGen_Asian == 1), "first")
mnl_second_gen <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1), "second")
mnl_third_gen  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1),  "third")

# Third-gen subgroups
cat("Fitting multinomial models for third generation ancestry subgroups...\n")
mnl_third_one   <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, OneAsian   == 1), "third", "OneAsian")
mnl_third_two   <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, TwoAsian   == 1), "third", "TwoAsian")
mnl_third_three <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, ThreeAsian == 1), "third", "ThreeAsian")
mnl_third_four  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, FourAsian  == 1), "third", "FourAsian")

# Second-gen subgroups
cat("Fitting multinomial models for second generation ancestry subgroups...\n")
mnl_second_aa <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AA_0bj == 1), "second", "AA_0bj")
mnl_second_aw <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AW_0bj == 1), "second", "AW_0bj")
mnl_second_wa <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, WA_0bj == 1), "second", "WA_0bj")

# ================================================================
# BOOTSTRAP INFRASTRUCTURE (self-contained; no external sourcing)
# ================================================================

# --- utilities to create grids and predict ---
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

newdata_for_grid <- function(data_subset, var_name, grid, rep_row = NULL) {
  if (is.null(rep_row)) rep_row <- representative_row(data_subset)
  nd <- rep_row[rep(1, nrow(grid)), , drop = FALSE]
  nd[[var_name]] <- grid$x_val
  nd
}

# --- AME calculator (average over sample) ---
# Binaries: discrete 0->1; Continuous: central difference, h = 0.1 SD
compute_AME <- function(model, data_subset, var_name, h_frac = 0.1) {
  d <- data_subset

  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    d0 <- d; d1 <- d
    d0[[var_name]] <- 0; d1[[var_name]] <- 1
    p0 <- predict(model, newdata = d0, type = "probs")
    p1 <- predict(model, newdata = d1, type = "probs")
    me_mat <- p1 - p0                    # N x K
  } else {
    x <- d[[var_name]]
    sd_x <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sd_x) || sd_x == 0) sd_x <- 0.1
    h <- h_frac * sd_x
    d_lo <- d; d_hi <- d
    d_lo[[var_name]] <- x - h
    d_hi[[var_name]] <- x + h
    p_lo <- predict(model, newdata = d_lo, type = "probs")
    p_hi <- predict(model, newdata = d_hi, type = "probs")
    me_mat <- (p_hi - p_lo) / (2 * h)    # N x K
  }
  ame <- colMeans(me_mat, na.rm = TRUE)
  tibble(variable = var_name,
         outcome  = colnames(me_mat),
         ame      = as.numeric(ame))
}

# --- Calculate bias-corrected bootstrap CIs ---
calculate_bc_ci <- function(boot_estimates, original_estimate, alpha = 0.05) {
  # Bias-correction factor
  z0 <- qnorm(mean(boot_estimates <= original_estimate, na.rm = TRUE))
  
  # Critical values
  z_alpha2 <- qnorm(alpha/2)
  z_1alpha2 <- qnorm(1 - alpha/2)
  
  # Bias-corrected percentiles
  p_lo <- pnorm(2*z0 + z_alpha2)
  p_hi <- pnorm(2*z0 + z_1alpha2)
  
  # Ensure percentiles are within valid range
  p_lo <- pmax(0.001, pmin(0.999, p_lo))
  p_hi <- pmax(0.001, pmin(0.999, p_hi))
  
  list(
    ci_lo = quantile(boot_estimates, p_lo, na.rm = TRUE),
    ci_hi = quantile(boot_estimates, p_hi, na.rm = TRUE),
    bc_p_lo = p_lo,
    bc_p_hi = p_hi
  )
}

# --- single bootstrap iteration ---
boot_once <- function(data_subset, generation, ancestry_filter = NULL,
                      pp_vars = c("value", "Female", "MomGradCollege", "DadGradCollege"), 
                      pp_grid_step = 0.5,
                      me_vars = c("value","Female","MomGradCollege","DadGradCollege"),
                      stratify_by = "region_year",
                      seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Optional stratified resampling to preserve region_year composition
  if (!is.null(stratify_by) && stratify_by %in% names(data_subset)) {
    d_split <- split(data_subset, data_subset[[stratify_by]])
    resampled <- lapply(d_split, function(df) df[sample.int(nrow(df), nrow(df), replace = TRUE), , drop = FALSE])
    d_boot <- dplyr::bind_rows(resampled)
  } else {
    d_boot <- data_subset[sample.int(nrow(data_subset), nrow(data_subset), replace = TRUE), , drop = FALSE]
  }

  # Refit
  mod <- try(fit_multinomial_model(d_boot, generation, ancestry_filter), silent = TRUE)
  if (inherits(mod, "try-error")) return(NULL)

  # Predicted probabilities along grid for each PP variable
  pp_list <- lapply(pp_vars, function(var_name) {
    grid <- make_var_grid(d_boot, var_name, grid_step = pp_grid_step)
    nd   <- newdata_for_grid(d_boot, var_name, grid)
    probs <- predict(mod, newdata = nd, type = "probs")
    if (is.vector(probs)) {  # edge case
      probs <- matrix(probs, nrow = 1)
      colnames(probs) <- mod$lev
    }
    pp_df <- as_tibble(probs)
    pp_df$x_val <- grid$x_val
    pp_df$variable <- var_name
    pp_df <- tidyr::pivot_longer(pp_df, -c(x_val, variable), names_to = "outcome", values_to = "p_hat")
    return(pp_df)
  })
  pp_df <- dplyr::bind_rows(pp_list)

  # AMEs
  me_list <- lapply(me_vars, function(v) compute_AME(mod, d_boot, v))
  me_df <- dplyr::bind_rows(me_list)

  list(pp = pp_df, me = me_df)
}

# --- bootstrap driver (parallel) ---
bootstrap_multinom <- function(data_subset, generation, ancestry_filter = NULL,
                               pp_vars = c("value", "Female", "MomGradCollege", "DadGradCollege"),
                               pp_grid_step = 0.5,
                               me_vars = c("value","Female","MomGradCollege","DadGradCollege"),
                               B = N_BOOTSTRAP, n_cores = N_CORES,
                               stratify_by = "region_year", seed = 2024) {

  # Point estimates on original sample for all PP variables
  model_main <- fit_multinomial_model(data_subset, generation, ancestry_filter)
  
  pp_main_list <- lapply(pp_vars, function(var_name) {
    grid_main  <- make_var_grid(data_subset, var_name, grid_step = pp_grid_step)
    nd_main    <- newdata_for_grid(data_subset, var_name, grid_main)
    probs_main <- predict(model_main, newdata = nd_main, type = "probs")
    if (is.vector(probs_main)) {
      probs_main <- matrix(probs_main, nrow = 1); colnames(probs_main) <- model_main$lev
    }
    pp_main <- as_tibble(probs_main)
    pp_main$x_val <- grid_main$x_val
    pp_main$variable <- var_name
    pp_main <- tidyr::pivot_longer(pp_main, -c(x_val, variable), names_to = "outcome", values_to = "p_hat")
    return(pp_main)
  })
  pp_main <- dplyr::bind_rows(pp_main_list)

  me_main <- dplyr::bind_rows(lapply(me_vars, function(v) compute_AME(model_main, data_subset, v)))

  # Parallel bootstrap
  set.seed(seed)
  seeds <- sample.int(1e7, B)
  use_mc <- .Platform$OS.type != "windows"

  if (use_mc) {
    res_list <- parallel::mclapply(seq_len(B), function(b)
      boot_once(data_subset, generation, ancestry_filter,
                pp_vars, pp_grid_step, me_vars, stratify_by, seeds[b]),
      mc.cores = n_cores)
  } else {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      varlist = c("fit_multinomial_model","make_var_grid","newdata_for_grid",
                  "representative_row","compute_AME","boot_once"),
      envir = environment()
    )
    res_list <- parallel::parLapply(cl, seq_len(B), function(b)
      boot_once(data_subset, generation, ancestry_filter,
                pp_vars, pp_grid_step, me_vars, stratify_by, seeds[b]))
  }

  # Drop failed iterations
  res_list <- Filter(Negate(is.null), res_list)
  if (length(res_list) == 0) stop("All bootstrap iterations failed to converge.")

  # Stack bootstrap draws
  pp_all <- dplyr::bind_rows(lapply(res_list, `[[`, "pp"), .id = "draw")
  me_all <- dplyr::bind_rows(lapply(res_list, `[[`, "me"), .id = "draw")

  # Summarize PP: mean and percentile CIs by variable
  pp_summ <- pp_all |>
    group_by(variable, x_val, outcome) |>
    summarise(
      p_mean = mean(p_hat, na.rm = TRUE),
      p_lo   = quantile(p_hat, 0.025, na.rm = TRUE),
      p_hi   = quantile(p_hat, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(pp_main |> rename(p_main = p_hat), by = c("variable", "x_val","outcome"))

  # Summarize AME across draws with bias-corrected CIs
  me_summ <- me_all |>
    group_by(variable, outcome) |>
    reframe({
      boot_ames <- ame
      orig_ame <- me_main$ame[me_main$variable == variable[1] & me_main$outcome == outcome[1]]
      bc_ci <- calculate_bc_ci(boot_ames, orig_ame)
      
      tibble(
        ame_mean = mean(boot_ames, na.rm = TRUE),
        ame_lo   = bc_ci$ci_lo,
        ame_hi   = bc_ci$ci_hi,
        ame_lo_simple = quantile(boot_ames, 0.025, na.rm = TRUE),
        ame_hi_simple = quantile(boot_ames, 0.975, na.rm = TRUE),
        ame_point_original = orig_ame
      )
    }) |>
    # Use original point estimate for display consistency
    mutate(ame_point_consistent = ame_point_original)

  list(pp = pp_summ, me = me_summ, B_eff = length(unique(pp_all$draw)))
}

# ================================================================
# PLOTTING WITH BOOTSTRAP CIs (from your snippet)
# ================================================================
plot_pp_boot <- function(pp_summ, var_name, gen_label) {
  outcome_labels <- c("Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White")
  pp_summ <- pp_summ |>
    mutate(group_labeled = dplyr::recode(outcome, !!!outcome_labels, .default = outcome),
           group_labeled = factor(group_labeled, levels = outcome_labels))

  var_labels <- c("value"="Anti-Asian Bias","Female"="Female",
                  "MomGradCollege"="College Graduate: Mother","DadGradCollege"="College Graduate: Father",
                  "education"="Years of Education", "income"="Household Income")
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name

  # Add percentage labels for text annotations
  pp_summ$label <- scales::percent(pp_summ$p_main, accuracy = 1)
  
  # For binary variables, only show CIs at the two endpoints
  # For continuous variables, show CIs only at min, max, and selected interior points
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    pp_summ$show_ci <- TRUE  # Show CIs for both 0 and 1
  } else {
    # For continuous variables, show CIs at min, max, and a few key points
    x_range <- range(pp_summ$x_val)
    key_points <- c(x_range[1], 0, x_range[2])  # min, middle (0), max
    pp_summ$show_ci <- pp_summ$x_val %in% key_points | 
                       abs(pp_summ$x_val - 0) < 0.1 |  # near zero
                       pp_summ$x_val == min(pp_summ$x_val) | 
                       pp_summ$x_val == max(pp_summ$x_val)
  }
  
  # Position dodge for points and error bars
  pd <- position_dodge(width = if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) 0.15 else 0.07)
  
  # Error bar width
  error_width <- if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) 0.05
                 else (max(pp_summ$x_val) - min(pp_summ$x_val)) * 0.02

  p <- ggplot(pp_summ, aes(x = x_val, y = p_main, color = group_labeled, group = group_labeled)) +
    geom_ribbon(aes(ymin = p_lo, ymax = p_hi, fill = group_labeled),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2, position = pd) +
    geom_point(size = 3, alpha = 0.95, position = pd) +
    # Only show error bars for selected points
    geom_errorbar(data = pp_summ |> filter(show_ci),
                  aes(ymin = p_lo, ymax = p_hi),
                  width = error_width, linewidth = 1, position = pd) +
    ggrepel::geom_text_repel(
      aes(label = label),
      position = pd, size = 6,
      max.overlaps = Inf,
      box.padding = grid::unit(0.35, "lines"),
      point.padding = grid::unit(0.30, "lines"),
      min.segment.length = 0, segment.alpha = 0.6, seed = 123, show.legend = FALSE
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"), name = "") +
    scale_fill_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"), name = "") +
    labs(x = x_label, y = "Probability") +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )

  # Add appropriate x-axis labels
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    if (var_name == "Female")
      p <- p + scale_x_continuous(breaks = c(0,1), labels = c("Male","Female"))
    else
      p <- p + scale_x_continuous(breaks = c(0,1), labels = c("No College","College Graduate"))
  } else if (var_name == "education") {
    # For education, might want specific breaks
    edu_range <- range(pp_summ$x_val)
    p <- p + scale_x_continuous(breaks = pretty(edu_range, n = 5))
  } else if (var_name == "income") {
    # For income, might want to format as currency or use specific breaks
    inc_range <- range(pp_summ$x_val)
    p <- p + scale_x_continuous(breaks = pretty(inc_range, n = 5), labels = scales::dollar_format())
  }
  
  return(p)
}

plot_me_boot <- function(me_summ, gen_label) {
  var_labels <- c("value"="Anti-Asian Bias","Female"="Female",
                  "MomGradCollege"="College Graduate: Mother","DadGradCollege"="College Graduate: Father",
                  "education"="Years of Education", "income"="Household Income")
  outcome_labels <- c("Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White")

  me_summ |>
    mutate(variable_label = factor(variable, levels = names(var_labels), labels = var_labels),
           outcome_label  = factor(outcome,  levels = names(outcome_labels), labels = outcome_labels)) |>
    # Add coefficient value labels for annotation
    mutate(coef_label = sprintf("%.2f", ame_point_consistent)) |>
    ggplot(aes(x = ame_point_consistent, y = variable_label, color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.8)) +
    geom_errorbarh(aes(xmin = ame_lo, xmax = ame_hi),
                   height = 0.2, linewidth = 1.1, position = position_dodge(width = 0.8)) +
    ggrepel::geom_text_repel(aes(label = coef_label), 
                             size = 4, max.overlaps = Inf,
                             position = position_dodge(width = 0.8),
                             point.padding = 0.3, 
                             box.padding = 0.5,
                             segment.curvature = 0,
                             fontface = "bold", show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"),
                       name = "Identity Choice") +
    labs(y = NULL, x = "Average marginal effect (per unit of variable)") +
    theme_customs() +
    theme(legend.position = "bottom")
}

# ================================================================
# RUN BOOTSTRAPS + SAVE FIGURES FOR ALL GROUPS
# ================================================================

# Helper to run, plot, and save in one go
run_boot_and_save <- function(data_subset, generation, ancestry_filter, label_slug, label_pretty,
                              pp_vars = NULL, grid_step = GRID_STEP, me_vars = NULL,
                              B = N_BOOTSTRAP, n_cores = N_CORES, seed = 2024) {

  # Determine PP and ME variables if not provided
  if (is.null(pp_vars)) {
    pp_vars <- get_pp_vars(data_subset)
  }
  if (is.null(me_vars)) {
    me_vars <- get_me_vars(data_subset)
  }
  
  cat(sprintf("\nBootstrapping: %s ...\n", label_pretty))
  cat(sprintf("PP variables: %s\n", paste(pp_vars, collapse = ", ")))
  cat(sprintf("ME variables: %s\n", paste(me_vars, collapse = ", ")))
  
  boot_res <- bootstrap_multinom(
    data_subset   = data_subset,
    generation    = generation,
    ancestry_filter = ancestry_filter,
    pp_vars       = pp_vars,
    pp_grid_step  = grid_step,
    me_vars       = me_vars,
    B             = B,
    n_cores       = n_cores,
    stratify_by   = "region_year",
    seed          = seed
  )
  cat(sprintf("Effective bootstrap draws (converged): %d\n", boot_res$B_eff))

  # Create PP plots for each variable
  for (var_name in pp_vars) {
    pp_data <- boot_res$pp |> filter(variable == var_name)
    plt_pp <- plot_pp_boot(pp_data, var_name, label_pretty)
    
    # Save with variable-specific filename
    filename <- sprintf("boot_pp_%s_%s.png", label_slug, var_name)
    ggsave(file.path(figures_wd, filename), plt_pp, width = 8, height = 6, dpi = 300)
  }
  
  # ME plot (unchanged)
  plt_me <- plot_me_boot(boot_res$me, label_pretty)
  ggsave(file.path(figures_wd, sprintf("boot_me_%s.png",  label_slug)), plt_me, width = 10, height = 6, dpi = 300)

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

# ---------- First generation ----------
if (any(CPS_IAT_multinomial$FirstGen_Asian == 1, na.rm = TRUE)) {
  boot_first <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(FirstGen_Asian == 1),
    generation  = "first",
    ancestry_filter = NULL,
    label_slug  = "first_generation",
    label_pretty = "First generation",
    seed = 2024 + 1
  )
}

# ---------- Second generation (overall) ----------
if (any(CPS_IAT_multinomial$SecondGen_Asian == 1, na.rm = TRUE)) {
  boot_second <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian == 1),
    generation  = "second",
    ancestry_filter = NULL,
    label_slug  = "second_generation",
    label_pretty = "Second generation",
    seed = 2024 + 2
  )
}

# ---------- Third generation (overall) ----------
if (any(CPS_IAT_multinomial$ThirdGen_Asian == 1, na.rm = TRUE)) {
  boot_third <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1),
    generation  = "third",
    ancestry_filter = NULL,
    label_slug  = "third_generation",
    label_pretty = "Third generation",
    seed = 2024 + 3
  )
}

# ---------- Second generation subgroups: AA/AW/WA ----------
if (any(CPS_IAT_multinomial$SecondGen_Asian == 1 & CPS_IAT_multinomial$AA_0bj == 1, na.rm = TRUE)) {
  boot_second_aa <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AA_0bj == 1),
    generation  = "second",
    ancestry_filter = "AA_0bj",
    label_slug  = "second_AA",
    label_pretty = "Second gen: AA parents",
    seed = 2025
  )
}
if (any(CPS_IAT_multinomial$SecondGen_Asian == 1 & CPS_IAT_multinomial$AW_0bj == 1, na.rm = TRUE)) {
  boot_second_aw <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AW_0bj == 1),
    generation  = "second",
    ancestry_filter = "AW_0bj",
    label_slug  = "second_AW",
    label_pretty = "Second gen: AW parents",
    seed = 2025 + 1
  )
}
if (any(CPS_IAT_multinomial$SecondGen_Asian == 1 & CPS_IAT_multinomial$WA_0bj == 1, na.rm = TRUE)) {
  boot_second_wa <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, WA_0bj == 1),
    generation  = "second",
    ancestry_filter = "WA_0bj",
    label_slug  = "second_WA",
    label_pretty = "Second gen: WA parents",
    seed = 2025 + 2
  )
}

# ---------- Third generation subgroups: 1–4 Asian grandparents ----------
if (any(CPS_IAT_multinomial$ThirdGen_Asian == 1 & CPS_IAT_multinomial$OneAsian == 1, na.rm = TRUE)) {
  boot_third_one <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, OneAsian == 1),
    generation  = "third",
    ancestry_filter = "OneAsian",
    label_slug  = "third_oneAsian",
    label_pretty = "Third gen: One Asian grandparent",
    seed = 2026
  )
}
if (any(CPS_IAT_multinomial$ThirdGen_Asian == 1 & CPS_IAT_multinomial$TwoAsian == 1, na.rm = TRUE)) {
  boot_third_two <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, TwoAsian == 1),
    generation  = "third",
    ancestry_filter = "TwoAsian",
    label_slug  = "third_twoAsian",
    label_pretty = "Third gen: Two Asian grandparents",
    seed = 2026 + 1
  )
}
if (any(CPS_IAT_multinomial$ThirdGen_Asian == 1 & CPS_IAT_multinomial$ThreeAsian == 1, na.rm = TRUE)) {
  boot_third_three <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, ThreeAsian == 1),
    generation  = "third",
    ancestry_filter = "ThreeAsian",
    label_slug  = "third_threeAsian",
    label_pretty = "Third gen: Three Asian grandparents",
    seed = 2026 + 2
  )
}
if (any(CPS_IAT_multinomial$ThirdGen_Asian == 1 & CPS_IAT_multinomial$FourAsian == 1, na.rm = TRUE)) {
  boot_third_four <- run_boot_and_save(
    data_subset = CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, FourAsian == 1),
    generation  = "third",
    ancestry_filter = "FourAsian",
    label_slug  = "third_fourAsian",
    label_pretty = "Third gen: Four Asian grandparents",
    seed = 2026 + 3
  )
}

cat("\nAll bootstrap plots and summaries saved in: ", figures_wd, "\n")
cat("Predicted probability plots saved for each variable (bias, parental education, etc.)\n")
cat("Marginal effects plots saved for all variables combined\n")
