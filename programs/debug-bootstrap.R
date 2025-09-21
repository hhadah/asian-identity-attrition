# Debug bootstrap marginal effects
suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(here)
  library(parallel)
})

# Setup
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")
set.seed(123)
N_BOOTSTRAP <- 5  # Very small for testing
N_CORES <- 2

# Load small sample
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>
  rename(value = lw_index) |>
  slice_head(n = 5000)  # Small sample for testing

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
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(region_year = interaction(region, year, drop = TRUE),
         identity_choice = relevel(identity_choice, ref = "Asian_only"))

# Simple analytical marginal effects
calculate_marginal_effects_analytical <- function(model, data_subset, var_name, gen_label) {
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
  se_me <- rep(0.01, ncol(me))  # Simple SE for testing
  
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

# Fit simple model
cat("Fitting simple model...\n")
fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
  frac_asian + Age + Age_sq + Age_cube + Age_quad + FirstGen_Asian + 
  SecondGen_Asian + region_year

model <- multinom(fml, data = CPS_IAT_multinomial, weights = CPS_IAT_multinomial$weight, trace = FALSE)
cat("Model fitted!\n")

# Test bootstrap with just one variable
cat("Testing bootstrap with 'value'...\n")

# Single bootstrap iteration
bootstrap_iteration <- function(i) {
  tryCatch({
    set.seed(i + 1000)
    boot_idx <- sample(nrow(CPS_IAT_multinomial), replace = TRUE)
    boot_data <- CPS_IAT_multinomial[boot_idx, ]
    
    cat(sprintf("Bootstrap %d: Sample size %d\n", i, nrow(boot_data)))
    
    boot_model <- multinom(fml, data = boot_data, 
                         weights = boot_data$weight, trace = FALSE, maxit = 100)
    
    boot_me <- calculate_marginal_effects_analytical(boot_model, boot_data, "value", "Test")
    cat(sprintf("Bootstrap %d: Success\n", i))
    return(boot_me$marginal_effect)
  }, error = function(e) {
    cat(sprintf("Bootstrap %d: Error - %s\n", i, e$message))
    return(rep(NA_real_, 3))  # 3 outcomes
  })
}

# Test sequential bootstrap
cat("Running sequential bootstrap...\n")
results <- lapply(1:N_BOOTSTRAP, bootstrap_iteration)
successful <- sum(!sapply(results, function(x) all(is.na(x))))
cat(sprintf("Sequential bootstrap: %d successful out of %d\n", successful, N_BOOTSTRAP))

if (successful > 0) {
  cat("Bootstrap is working! The issue was likely with parallel processing or data handling.\n")
} else {
  cat("Bootstrap still failing. Need to investigate model convergence issues.\n")
}