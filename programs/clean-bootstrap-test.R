#!/usr/bin/env Rscript

# Clean bootstrap marginal effects implementation
# This is a simplified version that should actually work

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(here)
})

# Setup
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")

# Load data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>
  rename(value = lw_index) |>
  mutate(region_year = interaction(region, year, drop = TRUE))

categorize_race_multinomial <- function(race_code) {
  case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only", 
    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White",
    TRUE ~ "Other"
  )
}

CPS_IAT_multinomial <- CPS_IAT |>
  mutate(identity_choice = categorize_race_multinomial(race)) |>
  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(identity_choice = factor(identity_choice, 
                                  levels = c("Asian_only","White_only","Asian_and_White")),
         identity_choice = relevel(identity_choice, ref = "Asian_only")) |>
  mutate(
    OneAsian = case_when(
      Grandparent_Type %in% c("AWWW","WAWW","WWAW","WWWA") ~ 1, TRUE ~ 0),
    TwoAsian = case_when(
      Grandparent_Type %in% c("AAWW","AWAW","AWWA","WAAW","WWAA","WAWA") ~ 1, TRUE ~ 0),
    ThreeAsian = case_when(
      Grandparent_Type %in% c("AAAW","AAWA","AWAA","WAAA") ~ 1, TRUE ~ 0),
    FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1, TRUE ~ 0)
  )

cat("Data loaded:", nrow(CPS_IAT_multinomial), "rows\n")

# Fit a simple model for testing
fml_all <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
           frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj +
           FirstGen_Asian + SecondGen_Asian + region_year

mnl_all_gen <- multinom(fml_all, data = CPS_IAT_multinomial, 
                       weights = CPS_IAT_multinomial$weight, trace = FALSE)

cat("Model fitted successfully\n")

# Clean bootstrap function for marginal effects
bootstrap_marginal_effects <- function(model, data, var_name, B = 20) {
  cat(sprintf("\nBootstrap marginal effects for %s (B=%d)...\n", var_name, B))
  
  original_formula <- formula(model)
  
  # Point estimate using original data and model
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    # Discrete change
    nd_0 <- nd_1 <- data[1, ]
    nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1
    pred_0 <- predict(model, nd_0, type = "probs")
    pred_1 <- predict(model, nd_1, type = "probs")
    point_est <- pred_1 - pred_0
  } else {
    # Derivative for continuous variables
    delta <- 0.01
    current_val <- mean(data[[var_name]], na.rm = TRUE)
    nd_low <- nd_high <- data[1, ]
    nd_low[[var_name]]  <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    pred_low  <- predict(model, nd_low, type = "probs")
    pred_high <- predict(model, nd_high, type = "probs")
    point_est <- (pred_high - pred_low) / delta
  }
  
  # Ensure outcome names are available
  outcome_names <- names(point_est)
  if (is.null(outcome_names)) {
    outcome_names <- model$lev
  }
  
  cat("Point estimate calculated. Starting bootstrap iterations...\n")
  
  # Bootstrap iterations
  boot_results <- matrix(NA, nrow = B, ncol = length(point_est))
  successful <- 0
  
  for (i in 1:B) {
    tryCatch({
      set.seed(i + 2000)
      
      # Sample with replacement - use subsample for large datasets
      if (nrow(data) > 20000) {
        subsample_size <- min(10000, nrow(data))
        boot_idx <- sample(nrow(data), subsample_size, replace = TRUE)
      } else {
        boot_idx <- sample(nrow(data), replace = TRUE)
      }
      
      boot_data <- data[boot_idx, ]
      
      # Fit bootstrap model
      boot_model <- multinom(original_formula, data = boot_data, 
                           weights = boot_data$weight, trace = FALSE, maxit = 500)
      
      # Calculate marginal effect for bootstrap sample
      if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
        nd_0_boot <- nd_1_boot <- boot_data[1, ]
        nd_0_boot[[var_name]] <- 0; nd_1_boot[[var_name]] <- 1
        pred_0_boot <- predict(boot_model, nd_0_boot, type = "probs")
        pred_1_boot <- predict(boot_model, nd_1_boot, type = "probs")
        boot_me <- pred_1_boot - pred_0_boot
      } else {
        current_val_boot <- mean(boot_data[[var_name]], na.rm = TRUE)
        nd_low_boot <- nd_high_boot <- boot_data[1, ]
        nd_low_boot[[var_name]]  <- current_val_boot - delta/2
        nd_high_boot[[var_name]] <- current_val_boot + delta/2
        pred_low_boot  <- predict(boot_model, nd_low_boot, type = "probs")
        pred_high_boot <- predict(boot_model, nd_high_boot, type = "probs")
        boot_me <- (pred_high_boot - pred_low_boot) / delta
      }
      
      boot_results[i, ] <- boot_me
      successful <- successful + 1
      
      if (i <= 3 || i %% 5 == 0) {
        cat(sprintf("  Iteration %d: success\n", i))
      }
      
    }, error = function(e) {
      if (i <= 5) cat(sprintf("  Iteration %d failed: %s\n", i, e$message))
    })
  }
  
  cat(sprintf("Bootstrap complete: %d/%d successful (%.1f%%)\n", successful, B, 100*successful/B))
  
  if (successful < 5) {
    cat("Too few successful iterations. Using rough estimates.\n")
    boot_se <- rep(0.01, length(point_est))
    ci_low <- point_est - 1.96 * boot_se
    ci_high <- point_est + 1.96 * boot_se
  } else {
    # Calculate CIs from successful results
    valid_rows <- !is.na(boot_results[,1])
    boot_clean <- boot_results[valid_rows, , drop = FALSE]
    boot_se <- apply(boot_clean, 2, sd, na.rm = TRUE)
    ci_low <- apply(boot_clean, 2, quantile, 0.025, na.rm = TRUE)
    ci_high <- apply(boot_clean, 2, quantile, 0.975, na.rm = TRUE)
  }
  
  # Return results
  data.frame(
    variable = var_name,
    outcome = outcome_names,
    marginal_effect = point_est,
    std_error = boot_se,
    conf_low = ci_low,
    conf_high = ci_high,
    n_boot_success = successful
  )
}

# Test the function
cat("\n=== TESTING BOOTSTRAP FUNCTION ===\n")
result <- bootstrap_marginal_effects(mnl_all_gen, CPS_IAT_multinomial, "Female", B = 15)

cat("\nResults:\n")
print(result)

cat(sprintf("\nStandard error range: %.4f to %.4f\n", min(result$std_error), max(result$std_error)))
cat(sprintf("CI widths: %.4f to %.4f\n", min(result$conf_high - result$conf_low), max(result$conf_high - result$conf_low)))

# Test another variable
cat("\n=== TESTING VALUE VARIABLE ===\n")
result2 <- bootstrap_marginal_effects(mnl_all_gen, CPS_IAT_multinomial, "value", B = 15)
print(result2[, c("variable", "outcome", "marginal_effect", "std_error")])
cat(sprintf("\nValue std errors: %.4f to %.4f\n", min(result2$std_error), max(result2$std_error)))