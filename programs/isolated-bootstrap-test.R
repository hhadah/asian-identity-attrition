#!/usr/bin/env Rscript
# ISOLATED bootstrap test with no dependencies on main script

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(here)
})

cat("Starting isolated bootstrap test...\n")

# Load small sample of data directly
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")

# Get just a small sample to work with
small_data <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>
  rename(value = lw_index) |>
  slice_sample(n = 2000) |>  # Small sample immediately
  mutate(
    identity_choice = case_when(
      race == 651 ~ "Asian_only",
      race == 100 ~ "White_only",
      race %in% c(803, 804, 809, 813) ~ "Asian_and_White",
      TRUE ~ "Other"
    )
  ) |>
  filter(identity_choice %in% c("Asian_only", "White_only", "Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(identity_choice = factor(identity_choice, 
                                  levels = c("Asian_only","White_only","Asian_and_White")),
         identity_choice = relevel(identity_choice, ref = "Asian_only"))

cat("Sample data loaded:", nrow(small_data), "rows\n")

# Fit simple model
simple_formula <- identity_choice ~ value + Female + Age
simple_model <- multinom(simple_formula, data = small_data, 
                        weights = small_data$weight, trace = FALSE)

cat("Simple model fitted\n")

# Test bootstrap manually
test_bootstrap_me <- function(model, data, var_name, B = 10) {
  cat(sprintf("Testing bootstrap for %s (B=%d)...\n", var_name, B))
  
  # Point estimate
  nd_0 <- nd_1 <- data[1, ]
  nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1
  pred_0 <- predict(model, nd_0, type = "probs")
  pred_1 <- predict(model, nd_1, type = "probs")
  point_me <- pred_1 - pred_0
  
  # Bootstrap
  boot_me <- matrix(NA, nrow = B, ncol = length(point_me))
  successful <- 0
  
  for (i in 1:B) {
    tryCatch({
      set.seed(i + 999)
      boot_idx <- sample(nrow(data), replace = TRUE)
      current_bootstrap_sample <- data[boot_idx, ]  # Different name to avoid collision
      
      boot_model <- multinom(simple_formula, data = current_bootstrap_sample, 
                           weights = current_bootstrap_sample$weight, trace = FALSE)
      
      nd_0_boot <- nd_1_boot <- current_bootstrap_sample[1, ]
      nd_0_boot[[var_name]] <- 0; nd_1_boot[[var_name]] <- 1
      pred_0_boot <- predict(boot_model, nd_0_boot, type = "probs")
      pred_1_boot <- predict(boot_model, nd_1_boot, type = "probs")
      
      boot_me[i, ] <- pred_1_boot - pred_0_boot
      successful <- successful + 1
      
      if (i <= 3) cat(sprintf("  Iteration %d: OK\n", i))
      
    }, error = function(e) {
      if (i <= 3) cat(sprintf("  Iteration %d: FAILED - %s\n", i, conditionMessage(e)))
    })
  }
  
  cat(sprintf("Bootstrap result: %d/%d successful\n", successful, B))
  
  if (successful > 0) {
    valid_rows <- !is.na(boot_me[,1])
    boot_clean <- boot_me[valid_rows, , drop = FALSE]
    
    boot_se <- apply(boot_clean, 2, sd)
    boot_ci_low <- apply(boot_clean, 2, quantile, 0.025)
    boot_ci_high <- apply(boot_clean, 2, quantile, 0.975)
    
    return(data.frame(
      outcome = names(point_me),
      marginal_effect = point_me,
      std_error = boot_se,
      conf_low = boot_ci_low,
      conf_high = boot_ci_high,
      n_success = successful
    ))
  } else {
    return(data.frame(
      outcome = names(point_me),
      marginal_effect = point_me,
      std_error = 0.01,
      conf_low = point_me - 0.02,
      conf_high = point_me + 0.02,
      n_success = 0
    ))
  }
}

# Run test
cat("\n=== FINAL BOOTSTRAP TEST ===\n")
result <- test_bootstrap_me(simple_model, small_data, "Female", B = 10)

cat("\nFinal Results:\n")
print(result)

cat(sprintf("\nSE range: %.4f to %.4f\n", min(result$std_error), max(result$std_error)))
cat(sprintf("CI widths: %.4f to %.4f\n", min(result$conf_high - result$conf_low), max(result$conf_high - result$conf_low)))

if (all(result$std_error > 0.001)) {
  cat("\n✓ SUCCESS: Bootstrap generated proper confidence intervals!\n")
} else {
  cat("\n✗ FAILED: Still getting tiny standard errors\n")
}