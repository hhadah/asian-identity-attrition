#!/usr/bin/env Rscript

# Test bootstrap confidence intervals
suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(parallel)
  library(here)
})

# Setup directories
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")

set.seed(123)
N_BOOTSTRAP <- 10  # Small for testing

# Load and prepare small test dataset
cat("Loading data...\n")
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

CPS_IAT_test <- CPS_IAT |>
  mutate(identity_choice = categorize_race_multinomial(race)) |>
  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(identity_choice = factor(identity_choice, 
                                  levels = c("Asian_only","White_only","Asian_and_White")),
         identity_choice = relevel(identity_choice, ref = "Asian_only")) |>
  slice_sample(n = 2000)  # Small sample for testing

cat("Test data:", nrow(CPS_IAT_test), "rows\n")

# Fit simple test model
fml <- identity_choice ~ value + Female + Age
test_model <- multinom(fml, data = CPS_IAT_test, weights = CPS_IAT_test$weight, trace = FALSE)

# Bootstrap function for discrete variables
bootstrap_discrete_me <- function(model, data, var_name, B = 10) {
  cat("Running bootstrap for", var_name, "with", B, "iterations...\n")
  
  original_formula <- formula(model)
  
  # Point estimate
  nd_0 <- nd_1 <- data[1,]
  nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1
  pred_0 <- predict(model, nd_0, type = "probs")
  pred_1 <- predict(model, nd_1, type = "probs")
  point_est <- pred_1 - pred_0
  
  # Bootstrap iterations
  boot_effects <- matrix(NA, nrow = B, ncol = length(point_est))
  successful <- 0
  
  for (i in 1:B) {
    tryCatch({
      boot_idx <- sample(nrow(data), replace = TRUE)
      boot_data <- data[boot_idx,]
      
      # Fit bootstrap model
      boot_model <- multinom(original_formula, data = boot_data, 
                            weights = boot_data$weight, trace = FALSE, maxit = 200)
      
      # Calculate marginal effect
      nd_0_boot <- nd_1_boot <- boot_data[1,]
      nd_0_boot[[var_name]] <- 0; nd_1_boot[[var_name]] <- 1
      pred_0_boot <- predict(boot_model, nd_0_boot, type = "probs")
      pred_1_boot <- predict(boot_model, nd_1_boot, type = "probs")
      
      boot_effects[i,] <- pred_1_boot - pred_0_boot
      successful <- successful + 1
    }, error = function(e) {
      if (i <= 2) cat("Bootstrap iteration", i, "failed:", e$message, "\n")
    })
  }
  
  cat("Successful bootstrap iterations:", successful, "out of", B, "\n")
  
  # Calculate CIs from successful iterations
  valid_rows <- !is.na(boot_effects[,1])
  boot_effects_clean <- boot_effects[valid_rows, , drop = FALSE]
  
  if (nrow(boot_effects_clean) > 0) {
    se_boot <- apply(boot_effects_clean, 2, sd, na.rm = TRUE)
    ci_low <- apply(boot_effects_clean, 2, quantile, 0.025, na.rm = TRUE)
    ci_high <- apply(boot_effects_clean, 2, quantile, 0.975, na.rm = TRUE)
  } else {
    # Fallback to rough estimates
    se_boot <- rep(0.01, length(point_est))
    ci_low <- point_est - 1.96 * se_boot
    ci_high <- point_est + 1.96 * se_boot
  }
  
  return(data.frame(
    variable = var_name,
    outcome = names(point_est),
    marginal_effect = point_est,
    std_error = se_boot,
    conf_low = ci_low,
    conf_high = ci_high,
    n_successful = successful
  ))
}

# Test bootstrap
cat("\n=== Testing bootstrap ===\n")
result <- bootstrap_discrete_me(test_model, CPS_IAT_test, "Female", B = N_BOOTSTRAP)

cat("\nBootstrap results:\n")
print(result)

cat("\nStandard error range:", range(result$std_error), "\n")
cat("CI widths:", result$conf_high - result$conf_low, "\n")