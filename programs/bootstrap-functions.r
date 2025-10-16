# ================================================================
# BOOTSTRAP SCRIPT - ABSOLUTELY FOOLPROOF VERSION
# ================================================================

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(parallel)
  library(scales)
})

# ================================================================
# SETUP
# ================================================================

N_BOOTSTRAP <- 500
N_CORES <- max(1, detectCores() - 1)
outcome_levels <- c("Asian_only", "White_only", "Asian_and_White")

cat("╔════════════════════════════════════════════════════════╗\n")
cat("║  Bootstrap Analysis - Foolproof Version               ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# ================================================================
# LOAD DATA
# ================================================================

cat("Loading data...\n")

CPS_IAT <- read_csv(file.path(datasets, "CPS_IAT_asian.csv"), show_col_types = FALSE) %>%
  rename(value = lw_index) %>%
  mutate(
    OneAsian = case_when(Grandparent_Type %in% c("AWWW","WAWW","WWAW","WWWA") ~ 1, TRUE ~ 0),
    TwoAsian = case_when(Grandparent_Type %in% c("AAWW","AWAW","AWWA","WAAW","WWAA","WAWA") ~ 1, TRUE ~ 0),
    ThreeAsian = case_when(Grandparent_Type %in% c("AAAW","AAWA","AWAA","WAAA") ~ 1, TRUE ~ 0),
    FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1, TRUE ~ 0)
  )

categorize_race_multinomial <- function(race_code) {
  dplyr::case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only",
    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White",
    TRUE ~ "Other"
  )
}

CPS_IAT_multinomial <- CPS_IAT %>%
  mutate(
    identity_choice = categorize_race_multinomial(race),
    identity_choice = factor(identity_choice, levels = c("Asian_only","White_only","Asian_and_White"))
  ) %>%
  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White"),
         !is.na(value), !is.na(identity_choice)) %>%
  mutate(region_year = interaction(region, year, drop = TRUE),
         identity_choice = relevel(identity_choice, ref = "Asian_only"))

CPS_IAT_adults <- read_csv(file.path(datasets, "CPS_IAT_asian_adults.csv"), show_col_types = FALSE) %>%
  rename(value = lw_index) %>%
  mutate(
    identity_choice = categorize_race_multinomial(race),
    identity_choice = factor(identity_choice, levels = c("Asian_only","White_only","Asian_and_White"))
  ) %>%
  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White"),
         !is.na(value), !is.na(identity_choice)) %>%
  mutate(region_year = interaction(region, year, drop = TRUE),
         identity_choice = relevel(identity_choice, ref = "Asian_only"))

CPS_IAT_adults_aw <- CPS_IAT_adults %>% filter(SecondGen_Asian == 1, AW_0bj == 1)
CPS_IAT_adults_wa <- CPS_IAT_adults %>% filter(SecondGen_Asian == 1, WA_0bj == 1)

cat(sprintf("Main dataset: %d observations\n", nrow(CPS_IAT_multinomial)))
cat(sprintf("Adult AW: %d observations\n", nrow(CPS_IAT_adults_aw)))
cat(sprintf("Adult WA: %d observations\n\n", nrow(CPS_IAT_adults_wa)))

# ================================================================
# BOOTSTRAP FUNCTION - SIMPLIFIED
# ================================================================

bootstrap_me_simple <- function(formula_string, data_df, variable, gen_label, 
                               outcome_lvls, n_boot, n_cores) {
  
  cat(sprintf("\nBootstrapping: %s (%d iterations, %d cores)\n", variable, n_boot, n_cores))
  
  # Convert to plain data frame
  df <- as.data.frame(data_df)
  has_wt <- "weight" %in% names(df)
  
  # Fit original model
  fml <- as.formula(formula_string)
  if (has_wt) {
    mod <- multinom(fml, data = df, weights = df$weight, trace = FALSE, maxit = 500)
  } else {
    mod <- multinom(fml, data = df, trace = FALSE, maxit = 500)
  }
  
  # Calculate original ME
  calc_me <- function(model, data, var) {
    rep_row <- data[1, , drop = FALSE]
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        rep_row[[col]] <- mean(data[[col]], na.rm = TRUE)
      } else {
        tbl <- table(data[[col]])
        rep_row[[col]] <- names(tbl)[which.max(tbl)]
      }
    }
    
    if (var %in% c("Female", "MomGradCollege", "DadGradCollege", "EducYears")) {
      nd0 <- nd1 <- rep_row
      nd0[[var]] <- 0; nd1[[var]] <- 1
      p0 <- predict(model, nd0, type = "probs")
      p1 <- predict(model, nd1, type = "probs")
      if (is.vector(p0)) {
        p0 <- matrix(p0, 1); p1 <- matrix(p1, 1)
        colnames(p0) <- colnames(p1) <- model$lev
      }
      return(as.vector(p1 - p0))
    } else {
      delta <- 0.01
      val <- rep_row[[var]]
      ndl <- ndh <- rep_row
      ndl[[var]] <- val - delta; ndh[[var]] <- val + delta
      pl <- predict(model, ndl, type = "probs")
      ph <- predict(model, ndh, type = "probs")
      if (is.vector(pl)) {
        pl <- matrix(pl, 1); ph <- matrix(ph, 1)
        colnames(pl) <- colnames(ph) <- model$lev
      }
      return(as.vector((ph - pl) / (2 * delta)))
    }
  }
  
  me_orig <- calc_me(mod, df, variable)
  
  # Save data as RDS for workers
  temp_data_file <- tempfile(fileext = ".rds")
  saveRDS(list(df = df, fml_str = formula_string, var = variable, has_wt = has_wt), 
          temp_data_file)
  
  # Bootstrap worker
  boot_fun <- function(i, data_file) {
    set.seed(2000 + i)
    obj <- readRDS(data_file)
    d <- obj$df
    v <- obj$var
    hw <- obj$has_wt
    f <- as.formula(obj$fml_str)
    
    # Resample
    if (hw) {
      idx <- sample(nrow(d), nrow(d), replace = TRUE, prob = d$weight)
    } else {
      idx <- sample(nrow(d), nrow(d), replace = TRUE)
    }
    bd <- d[idx, ]
    
    # Fit
    bm <- tryCatch({
      if (hw) {
        multinom(f, data = bd, weights = bd$weight, trace = FALSE, maxit = 500)
      } else {
        multinom(f, data = bd, trace = FALSE, maxit = 500)
      }
    }, error = function(e) NULL)
    
    if (is.null(bm)) return(NULL)
    
    # Calc ME
    rr <- bd[1, , drop = FALSE]
    for (col in names(bd)) {
      if (is.numeric(bd[[col]])) {
        rr[[col]] <- mean(bd[[col]], na.rm = TRUE)
      } else {
        tbl <- table(bd[[col]])
        rr[[col]] <- names(tbl)[which.max(tbl)]
      }
    }
    
    if (v %in% c("Female", "MomGradCollege", "DadGradCollege", "EducYears")) {
      nd0 <- nd1 <- rr
      nd0[[v]] <- 0; nd1[[v]] <- 1
      p0 <- predict(bm, nd0, type = "probs")
      p1 <- predict(bm, nd1, type = "probs")
      if (is.vector(p0)) {
        p0 <- matrix(p0, 1); p1 <- matrix(p1, 1)
        colnames(p0) <- colnames(p1) <- bm$lev
      }
      return(as.vector(p1 - p0))
    } else {
      delta <- 0.01
      val <- rr[[v]]
      ndl <- ndh <- rr
      ndl[[v]] <- val - delta; ndh[[v]] <- val + delta
      pl <- predict(bm, ndl, type = "probs")
      ph <- predict(bm, ndh, type = "probs")
      if (is.vector(pl)) {
        pl <- matrix(pl, 1); ph <- matrix(ph, 1)
        colnames(pl) <- colnames(ph) <- bm$lev
      }
      return(as.vector((ph - pl) / (2 * delta)))
    }
  }
  
  # Run parallel
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, library(nnet))
  clusterExport(cl, "temp_data_file", envir = environment())
  
  boot_res <- parLapply(cl, 1:n_boot, function(i) boot_fun(i, temp_data_file))
  stopCluster(cl)
  
  # Clean up temp file
  unlink(temp_data_file)
  
  # Process
  boot_res <- boot_res[!sapply(boot_res, is.null)]
  n_success <- length(boot_res)
  cat(sprintf("Success: %d/%d (%.1f%%)\n", n_success, n_boot, 100*n_success/n_boot))
  
  if (n_success < 50) warning("Less than 50 successful iterations!")
  
  bmat <- do.call(rbind, boot_res)
  
  data.frame(
    variable = variable,
    outcome = outcome_lvls,
    marginal_effect = me_orig,
    std_error = apply(bmat, 2, sd, na.rm = TRUE),
    conf_low = apply(bmat, 2, quantile, 0.025, na.rm = TRUE),
    conf_high = apply(bmat, 2, quantile, 0.975, na.rm = TRUE),
    type = ifelse(variable %in% c("Female", "MomGradCollege", "DadGradCollege", "EducYears"),
                 "discrete_change", "derivative"),
    generation = gen_label,
    n_boot_success = n_success,
    stringsAsFactors = FALSE
  )
}

# Wrapper for multiple variables
bootstrap_all <- function(formula_string, data_df, gen_label, outcome_lvls, 
                         variables, n_boot, n_cores) {
  cat(sprintf("\n╔════════════════════════════════════════════════════════╗\n"))
  cat(sprintf("║  %s\n", gen_label))
  cat(sprintf("╚════════════════════════════════════════════════════════╝\n"))
  
  results <- lapply(variables, function(v) {
    bootstrap_me_simple(formula_string, data_df, v, gen_label, 
                       outcome_lvls, n_boot, n_cores)
  })
  
  combined <- do.call(rbind, results)
  combined$significant <- (combined$conf_low > 0 & combined$conf_high > 0) | 
                         (combined$conf_low < 0 & combined$conf_high < 0)
  return(combined)
}

# Plot function
plot_me_bootstrap <- function(me_results, gen_label) {
  var_labels <- c("value" = "Anti-Asian Bias", "Female" = "Female",
                  "MomGradCollege" = "College Graduate: Mother",
                  "DadGradCollege" = "College Graduate: Father",
                  "EducYears" = "Years of Education",
                  "lnftotval" = "Log Family Income")
  
  outcome_labels <- c("Asian_only" = "Asian only", "White_only" = "White only",
                     "Asian_and_White" = "Asian & White")
  
  me_results$variable_label <- factor(me_results$variable, 
    levels = names(var_labels)[names(var_labels) %in% unique(me_results$variable)],
    labels = var_labels[names(var_labels) %in% unique(me_results$variable)])
  
  me_results$outcome_label <- factor(me_results$outcome, 
    levels = names(outcome_labels), labels = outcome_labels)
  
  me_results$alpha_val <- ifelse(me_results$significant, 1.0, 0.5)
  me_results$coef_label <- ifelse(me_results$significant,
    paste0(sprintf("%.3f", me_results$marginal_effect), "*"),
    sprintf("%.3f", me_results$marginal_effect))
  
  x_range <- range(c(me_results$conf_low, me_results$conf_high), na.rm = TRUE)
  
  ggplot(me_results, aes(x = marginal_effect, y = variable_label, color = outcome_label)) +
    geom_point(aes(alpha = I(alpha_val)), size = 4, position = position_dodge(0.8)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, alpha = I(alpha_val)),
                   height = 0.2, linewidth = 1.2, position = position_dodge(0.8)) +
    ggrepel::geom_text_repel(aes(label = coef_label, alpha = I(alpha_val)), 
                            size = 3.5, max.overlaps = Inf, position = position_dodge(0.8),
                            point.padding = 0.3, box.padding = 0.5,
                            fontface = "bold", show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    scale_color_manual(values = c("Asian only" = "#2E8B57", "White only" = "#4169E1",
                                  "Asian & White" = "#FF8C00"), name = "Identity Choice") +
    scale_x_continuous(breaks = pretty(x_range, 6), name = "Marginal Effect (percentage points)") +
    labs(y = "", subtitle = paste(gen_label, "(Bootstrap 95% CI)"),
         caption = paste0("* p < 0.05. Based on ", unique(me_results$n_boot_success)[1], 
                         " successful bootstrap iterations.")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.subtitle = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
          plot.caption = element_text(size = 9, color = "grey60", hjust = 0))
}

# ================================================================
# RUN BOOTSTRAP
# ================================================================

start_time <- Sys.time()

# All generations
me_all <- bootstrap_all(
  "identity_choice ~ value + Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + FirstGen_Asian + SecondGen_Asian + region_year",
  CPS_IAT_multinomial, "All generations", outcome_levels,
  c("value", "Female", "MomGradCollege", "DadGradCollege"), N_BOOTSTRAP, N_CORES)

# First generation
me_first <- bootstrap_all(
  "identity_choice ~ value + Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year",
  CPS_IAT_multinomial %>% filter(FirstGen_Asian == 1), "First generation", outcome_levels,
  c("value", "Female", "MomGradCollege", "DadGradCollege"), N_BOOTSTRAP, N_CORES)

# Second gen AW
me_aw <- bootstrap_all(
  "identity_choice ~ value + Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year",
  CPS_IAT_multinomial %>% filter(SecondGen_Asian == 1, AW_0bj == 1), "Second gen: AW", outcome_levels,
  c("value", "Female", "MomGradCollege", "DadGradCollege"), N_BOOTSTRAP, N_CORES)

# Second gen WA
me_wa <- bootstrap_all(
  "identity_choice ~ value + Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year",
  CPS_IAT_multinomial %>% filter(SecondGen_Asian == 1, WA_0bj == 1), "Second gen: WA", outcome_levels,
  c("value", "Female", "MomGradCollege", "DadGradCollege"), N_BOOTSTRAP, N_CORES)

# Third generation
me_third <- bootstrap_all(
  "identity_choice ~ value + Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type + region_year",
  CPS_IAT_multinomial %>% filter(ThirdGen_Asian == 1), "Third generation", outcome_levels,
  c("value", "Female", "MomGradCollege", "DadGradCollege"), N_BOOTSTRAP, N_CORES)

# Adults AW
me_aw_adults <- bootstrap_all(
  "identity_choice ~ value + Female + EducYears + frac_asian + lnftotval + Age + Age_sq + Age_cube + Age_quad + region_year",
  CPS_IAT_adults_aw, "Adults: AW", outcome_levels,
  c("lnftotval", "EducYears", "Female", "value"), N_BOOTSTRAP, N_CORES)

# Adults WA
me_wa_adults <- bootstrap_all(
  "identity_choice ~ value + Female + EducYears + frac_asian + lnftotval + Age + Age_sq + Age_cube + Age_quad + region_year",
  CPS_IAT_adults_wa, "Adults: WA", outcome_levels,
  c("lnftotval", "EducYears", "Female", "value"), N_BOOTSTRAP, N_CORES)

end_time <- Sys.time()

# ================================================================
# SAVE RESULTS
# ================================================================

cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  Saving Results                                        ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# Create and save plots
for (name in c("all", "first", "aw", "wa", "third", "aw_adults", "wa_adults")) {
  me_obj <- get(paste0("me_", name))
  plot_obj <- plot_me_bootstrap(me_obj, me_obj$generation[1])
  ggsave(file.path(figures_wd, paste0("boot_me_", name, ".png")), 
         plot_obj, width = 10, height = 6, dpi = 300)
  write_csv(me_obj, file.path(figures_wd, paste0("boot_me_", name, ".csv")))
}

cat(sprintf("\n✓ Complete! Time: %.2f minutes\n", as.numeric(end_time - start_time, units = "mins")))
cat(sprintf("✓ Results saved to: %s\n", figures_wd))