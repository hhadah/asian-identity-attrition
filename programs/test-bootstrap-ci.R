# Test bootstrap confidence intervals# Quick test of bootstrap CIs

library(readr)suppressPackageStartupMessages({

library(nnet)  library(readr)

library(dplyr)  library(nnet)

library(ggplot2)  library(dplyr)

library(parallel)  library(here)

library(here)  library(parallel)

})

# Setup directories

git_mdir <- here::here()# Setup

datasets <- paste0(git_mdir,"/data/datasets")git_mdir <- here::here()

datasets <- paste0(git_mdir,"/data/datasets")

set.seed(123)set.seed(123)

N_BOOTSTRAP <- 20  # Small for testing

N_CORES <- 2# Test with small sample and explicit settings

N_BOOTSTRAP <- 20

# Load dataN_CORES <- 2

CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>USE_BOOTSTRAP <- TRUE

  rename(value = lw_index) |>

  mutate(region_year = interaction(region, year, drop = TRUE))# Load and prepare a small sample

CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>

categorize_race_multinomial <- function(race_code) {  rename(value = lw_index) |>

  dplyr::case_when(  slice_sample(n = 2000)  # Small sample for testing

    race_code %in% c(651) ~ "Asian_only",

    race_code %in% c(100) ~ "White_only", categorize_race_multinomial <- function(race_code) {

    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White",  dplyr::case_when(

    TRUE ~ "Other"    race_code %in% c(651) ~ "Asian_only",

  )    race_code %in% c(100) ~ "White_only", 

}    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White",

    TRUE ~ "Other"

CPS_IAT_test <- CPS_IAT |>  )

  mutate(identity_choice = categorize_race_multinomial(race)) |>}

  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White")) |>

  filter(!is.na(value), !is.na(identity_choice)) |>test_data <- CPS_IAT |>

  mutate(identity_choice = factor(identity_choice,   mutate(

                                  levels = c("Asian_only","White_only","Asian_and_White")),    identity_choice = categorize_race_multinomial(race),

         identity_choice = relevel(identity_choice, ref = "Asian_only")) |>    identity_choice = factor(identity_choice, levels = c("Asian_only","White_only","Asian_and_White"))

  slice_sample(n = 5000)  # Small sample for testing  ) |>

  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White")) |>

cat("Test data:", nrow(CPS_IAT_test), "rows\n")  filter(!is.na(value), !is.na(identity_choice)) |>

  mutate(

# Fit test model    region_year = interaction(region, year, drop = TRUE),

fml <- identity_choice ~ value + Female + Age    identity_choice = relevel(identity_choice, ref = "Asian_only")

test_model <- multinom(fml, data = CPS_IAT_test, weights = CPS_IAT_test$weight, trace = FALSE)  )



# Simple bootstrap function# Simple model

simple_bootstrap_me <- function(model, data, var_name, B = 20) {cat("Fitting simple test model...\n")

  cat("Running bootstrap for", var_name, "with", B, "iterations...\n")fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 

         frac_asian + Age + Age_sq + FirstGen_Asian + SecondGen_Asian

  original_formula <- formula(model)

  test_model <- multinom(fml, data = test_data, weights = test_data$weight, trace = FALSE)

  # Point estimate

  nd_0 <- nd_1 <- data[1,]# Test bootstrap function directly

  nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1calculate_marginal_effects_analytical <- function(model, data_subset, var_name, gen_label) {

  pred_0 <- predict(model, nd_0, type = "probs")  rep_data <- data_subset[1, , drop = FALSE]

  pred_1 <- predict(model, nd_1, type = "probs")  

  point_est <- pred_1 - pred_0  for (col_name in names(data_subset)) {

      if (col_name == var_name) next

  # Bootstrap    if (is.numeric(data_subset[[col_name]])) {

  boot_effects <- replicate(B, {      rep_data[[col_name]] <- mean(data_subset[[col_name]], na.rm = TRUE)

    boot_idx <- sample(nrow(data), replace = TRUE)    } else {

    boot_data <- data[boot_idx,]      tbl <- table(data_subset[[col_name]])

    boot_model <- multinom(original_formula, data = boot_data,       rep_data[[col_name]] <- names(tbl)[which.max(tbl)]

                          weights = boot_data$weight, trace = FALSE, maxit = 200)    }

      }

    nd_0_boot <- nd_1_boot <- boot_data[1,]

    nd_0_boot[[var_name]] <- 0; nd_1_boot[[var_name]] <- 1  delta <- 0.01

    pred_0_boot <- predict(boot_model, nd_0_boot, type = "probs")  current_val <- rep_data[[var_name]]

    pred_1_boot <- predict(boot_model, nd_1_boot, type = "probs")  nd_low <- rep_data; nd_high <- rep_data

    pred_1_boot - pred_0_boot  nd_low[[var_name]]  <- current_val - delta/2

  })  nd_high[[var_name]] <- current_val + delta/2

    pred_low  <- predict(model, newdata = nd_low,  type = "probs")

  # Calculate CIs  pred_high <- predict(model, newdata = nd_high, type = "probs")

  if (is.matrix(boot_effects)) {  

    se_boot <- apply(boot_effects, 1, sd, na.rm = TRUE)  if (is.vector(pred_low)) {

    ci_low <- apply(boot_effects, 1, quantile, 0.025, na.rm = TRUE)    outcome_names <- model$lev

    ci_high <- apply(boot_effects, 1, quantile, 0.975, na.rm = TRUE)    pred_low <- matrix(pred_low, nrow = 1); pred_high <- matrix(pred_high, nrow = 1)

  } else {    colnames(pred_low) <- colnames(pred_high) <- outcome_names

    se_boot <- sd(boot_effects, na.rm = TRUE)  }

    ci_low <- quantile(boot_effects, 0.025, na.rm = TRUE)  

    ci_high <- quantile(boot_effects, 0.975, na.rm = TRUE)  me <- (pred_high - pred_low) / delta

  }  se_me <- rep(0.01, ncol(me))  # Simple SE for testing

    

  return(data.frame(  data.frame(

    variable = var_name,    variable = var_name, 

    outcome = names(point_est),    outcome = colnames(me),

    marginal_effect = point_est,    marginal_effect = as.vector(me),

    std_error = se_boot,    std_error = se_me,

    conf_low = ci_low,    conf_low = as.vector(me) - 1.96 * se_me,

    conf_high = ci_high    conf_high = as.vector(me) + 1.96 * se_me,

  ))    type = "derivative", 

}    generation = gen_label, 

    stringsAsFactors = FALSE

# Test bootstrap  )

result <- simple_bootstrap_me(test_model, CPS_IAT_test, "Female", B = N_BOOTSTRAP)}

cat("\nBootstrap results:\n")

print(result)# Simple bootstrap test

cat("Testing bootstrap for 'value' variable...\n")

cat("\nStandard error range:", range(result$std_error), "\n")bootstrap_iteration <- function(i) {

cat("CI widths:", result$conf_high - result$conf_low, "\n")  tryCatch({
    set.seed(i + 1000)
    boot_idx <- sample(nrow(test_data), replace = TRUE)
    boot_data <- test_data[boot_idx, ]
    
    boot_model <- multinom(fml, data = boot_data, 
                         weights = boot_data$weight, trace = FALSE, maxit = 300)
    
    boot_me <- calculate_marginal_effects_analytical(boot_model, boot_data, "value", "Test")
    return(boot_me$marginal_effect)
  }, error = function(e) {
    cat(sprintf("Bootstrap %d failed: %s\n", i, e$message))
    return(rep(NA_real_, 3))  # 3 outcomes
  })
}

# Run bootstrap
cat("Running bootstrap iterations...\n")
boot_results <- lapply(1:N_BOOTSTRAP, bootstrap_iteration)

# Process results
valid_results <- boot_results[!sapply(boot_results, function(x) all(is.na(x)))]
successful <- length(valid_results)

cat(sprintf("Bootstrap completed: %d successful out of %d iterations\n", successful, N_BOOTSTRAP))

if (successful > 5) {
  boot_effects <- do.call(rbind, valid_results)
  
  # Calculate CIs
  conf_low  <- apply(boot_effects, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  conf_high <- apply(boot_effects, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  boot_se   <- apply(boot_effects, 2, function(x) sd(x, na.rm = TRUE))
  
  cat("Bootstrap CIs calculated successfully!\n")
  cat("Standard errors:", round(boot_se, 4), "\n")
  cat("95% CI lower bounds:", round(conf_low, 4), "\n")
  cat("95% CI upper bounds:", round(conf_high, 4), "\n")
} else {
  cat("Not enough successful bootstrap iterations for reliable CIs\n")
}