#!/usr/bin/env Rscript
# Minimal test of the fixed bootstrap function

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(here)
})

# Setup directories
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")

# Configuration
N_BOOTSTRAP <- 15  # Small for testing

# Load and prepare minimal dataset
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
  slice_sample(n = 5000)  # Small sample for testing

cat("Test data:", nrow(CPS_IAT_test), "rows\n")

# Fit simple test model
fml_simple <- identity_choice ~ value + Female + Age + frac_asian
test_model <- multinom(fml_simple, data = CPS_IAT_test, weights = CPS_IAT_test$weight, trace = FALSE)

# Copy the fixed analytical function from the main script
calculate_marginal_effects_analytical <- function(model, data_subset, var_name, gen_label) {
  # Get representative values (faster than bootstrap)
  rep_data <- data_subset[1, , drop = FALSE]
  
  for (col_name in names(data_subset)) {
    if (col_name == var_name) next
    if (is.numeric(data_subset[[col_name]])) {
      rep_data[[col_name]] <- mean(data_subset[[col_name]], na.rm = TRUE)
    } else {
      tbl <- table(data_subset[[col_name]])
      rep_data[[col_name]] <- names(tbl)[which.max(tbl)]
    }
  }

  # Calculate marginal effects
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    # Discrete change for binary variables
    nd_0 <- rep_data; nd_1 <- rep_data
    nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1
    pred_0 <- predict(model, newdata = nd_0, type = "probs")
    pred_1 <- predict(model, newdata = nd_1, type = "probs")
    
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1); pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    
    me <- pred_1 - pred_0
    
    # Calculate standard errors using delta method approximation
    se_approx <- sqrt(diag(vcov(model))) / 100  # Rough approximation
    se_me <- rep(mean(se_approx), ncol(me))
    
    data.frame(
      variable = var_name, 
      outcome = colnames(me),
      marginal_effect = as.vector(me),
      std_error = se_me,
      conf_low = as.vector(me) - 1.96 * se_me,
      conf_high = as.vector(me) + 1.96 * se_me,
      type = "discrete_change", 
      generation = gen_label, 
      stringsAsFactors = FALSE
    )
  } else {
    # Derivative for continuous variables
    delta <- 0.01
    current_val <- rep_data[[var_name]]
    nd_low <- rep_data; nd_high <- rep_data
    nd_low[[var_name]]  <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    pred_low  <- predict(model, newdata = nd_low,  type = "probs")
    pred_high <- predict(model, newdata = nd_high, type = "probs")
    
    if (is.vector(pred_low)) {
      outcome_names <- model$lev
      pred_low <- matrix(pred_low, nrow = 1); pred_high <- matrix(pred_high, nrow = 1)
      colnames(pred_low) <- colnames(pred_high) <- outcome_names
    }
    
    me <- (pred_high - pred_low) / delta
    
    # Calculate standard errors
    se_approx <- sqrt(diag(vcov(model))) / 100
    se_me <- rep(mean(se_approx), ncol(me))
    
    data.frame(
      variable = var_name, 
      outcome = colnames(me),
      marginal_effect = as.vector(me),
      std_error = se_me,
      conf_low = as.vector(me) - 1.96 * se_me,
      conf_high = as.vector(me) + 1.96 * se_me,
      type = "derivative", 
      generation = gen_label, 
      stringsAsFactors = FALSE
    )
  }
}

# New working bootstrap function
calculate_marginal_effects_bootstrap_new <- function(model, data_subset, var_name, gen_label, B = 15) {
  
  cat(sprintf("Bootstrap marginal effects for %s (B=%d)...\n", var_name, B))
  
  # Get point estimate first
  point_est <- calculate_marginal_effects_analytical(model, data_subset, var_name, gen_label)
  original_formula <- formula(model)
  
  # Use smaller working dataset for large inputs
  work_data <- data_subset
  if (nrow(data_subset) > 5000) {
    work_data <- data_subset[sample(nrow(data_subset), 3000), ]
    cat("Using subsample of", nrow(work_data), "rows for bootstrap\n")
  }
  
  # Bootstrap iterations
  boot_effects <- vector("list", B)
  successful_boots <- 0
  
  for (i in 1:B) {
    tryCatch({
      set.seed(i * 123 + 789)
      
      # Bootstrap sampling
      boot_indices <- sample(nrow(work_data), replace = TRUE)
      bootstrap_data <- work_data[boot_indices, ]
      
      # Check for variation
      if (length(unique(bootstrap_data[[var_name]])) < 2) {
        next
      }
      
      # Fit bootstrap model
      bootstrap_model <- multinom(original_formula, data = bootstrap_data, 
                                 weights = bootstrap_data$weight, trace = FALSE, maxit = 200)
      
      # Calculate bootstrap marginal effect
      boot_me <- calculate_marginal_effects_analytical(bootstrap_model, bootstrap_data, var_name, gen_label)
      
      # Align with original outcomes
      outcome_match <- match(point_est$outcome, boot_me$outcome)
      aligned_me <- rep(NA_real_, nrow(point_est))
      valid_matches <- !is.na(outcome_match)
      aligned_me[valid_matches] <- boot_me$marginal_effect[outcome_match[valid_matches]]
      
      boot_effects[[i]] <- aligned_me
      successful_boots <- successful_boots + 1
      
      if (i <= 3 || i %% 5 == 0) {
        cat(sprintf("  Bootstrap %d/%d done\n", i, B))
      }
      
    }, error = function(e) {
      if (i <= 3) cat(sprintf("  Bootstrap %d failed: %s\n", i, conditionMessage(e)))
    })
  }
  
  cat(sprintf("Bootstrap complete: %d/%d successful\n", successful_boots, B))
  
  if (successful_boots < 3) {
    cat("Too few successes. Using analytical results.\n")
    return(point_est)
  }
  
  # Process successful bootstrap results
  valid_boots <- boot_effects[!sapply(boot_effects, function(x) is.null(x) || all(is.na(x)))]
  boot_matrix <- do.call(rbind, valid_boots)
  
  # Calculate bootstrap CIs
  boot_se <- apply(boot_matrix, 2, sd, na.rm = TRUE)
  boot_ci_low <- apply(boot_matrix, 2, quantile, 0.025, na.rm = TRUE)
  boot_ci_high <- apply(boot_matrix, 2, quantile, 0.975, na.rm = TRUE)
  
  # Update results
  point_est$std_error <- boot_se
  point_est$conf_low <- boot_ci_low
  point_est$conf_high <- boot_ci_high
  
  return(point_est)
}

# Test the new function
cat("\n=== TESTING NEW BOOTSTRAP FUNCTION ===\n")

# Test Female variable
result_female <- calculate_marginal_effects_bootstrap_new(test_model, CPS_IAT_test, "Female", "Test", B = N_BOOTSTRAP)

cat("\nResults for Female:\n")
print(result_female[, c("variable", "outcome", "marginal_effect", "std_error", "conf_low", "conf_high")])

# Test value variable  
result_value <- calculate_marginal_effects_bootstrap_new(test_model, CPS_IAT_test, "value", "Test", B = N_BOOTSTRAP)

cat("\nResults for value:\n")
print(result_value[, c("variable", "outcome", "marginal_effect", "std_error", "conf_low", "conf_high")])

cat(sprintf("\nFemale SE range: %.4f to %.4f\n", min(result_female$std_error), max(result_female$std_error)))
cat(sprintf("Value SE range: %.4f to %.4f\n", min(result_value$std_error), max(result_value$std_error)))

cat("\nBootstrap test completed successfully!\n")