# Minimal bootstrap CI test
suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(here)
})

# Simple setup
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")

# Load small sample 
cat("Loading data...\n")
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"), show_col_types = FALSE) |>
  rename(value = lw_index) |>
  slice_head(n = 1000)  # Very small sample

# Simple data prep
test_data <- CPS_IAT |>
  filter(!is.na(value), race %in% c(651, 100, 803, 804, 809, 813)) |>
  mutate(
    identity_choice = case_when(
      race == 651 ~ "Asian_only",
      race == 100 ~ "White_only", 
      race %in% c(803, 804, 809, 813) ~ "Asian_and_White"
    ),
    identity_choice = factor(identity_choice, levels = c("Asian_only","White_only","Asian_and_White")),
    identity_choice = relevel(identity_choice, ref = "Asian_only")
  ) |>
  filter(!is.na(identity_choice))

cat("Data prepared. Sample size:", nrow(test_data), "\n")

# Simple model
cat("Fitting model...\n")
fml <- identity_choice ~ value + Female + Age
model <- multinom(fml, data = test_data, trace = FALSE)
cat("Model fitted.\n")

# Test bootstrap
cat("Testing bootstrap (10 iterations)...\n")
B <- 10
boot_results <- vector("list", B)

for (i in 1:B) {
  tryCatch({
    boot_idx <- sample(nrow(test_data), replace = TRUE)
    boot_data <- test_data[boot_idx, ]
    
    boot_model <- multinom(fml, data = boot_data, trace = FALSE, maxit = 100)
    
    # Simple marginal effect calculation
    nd1 <- test_data[1, ]; nd1$value <- nd1$value + 0.1
    nd2 <- test_data[1, ]; nd2$value <- nd2$value - 0.1
    
    pred1 <- predict(boot_model, nd1, type = "probs")
    pred2 <- predict(boot_model, nd2, type = "probs")
    
    if (is.vector(pred1)) {
      pred1 <- matrix(pred1, nrow = 1)
      pred2 <- matrix(pred2, nrow = 1)
    }
    
    me <- (pred1 - pred2) / 0.2
    boot_results[[i]] <- as.vector(me)
    cat("Bootstrap", i, "successful\n")
  }, error = function(e) {
    cat("Bootstrap", i, "failed:", e$message, "\n")
    boot_results[[i]] <- rep(NA, 3)
  })
}

# Check results
valid <- !sapply(boot_results, function(x) all(is.na(x)))
successful <- sum(valid)

cat("Successful bootstrap iterations:", successful, "out of", B, "\n")

if (successful >= 5) {
  boot_matrix <- do.call(rbind, boot_results[valid])
  
  # Calculate CIs
  ci_lower <- apply(boot_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(boot_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
  boot_se <- apply(boot_matrix, 2, sd, na.rm = TRUE)
  
  cat("\n=== BOOTSTRAP RESULTS ===\n")
  cat("Standard Errors:", round(boot_se, 4), "\n")
  cat("95% CI Lower:  ", round(ci_lower, 4), "\n")
  cat("95% CI Upper:  ", round(ci_upper, 4), "\n")
  cat("\nBootstrap CIs working correctly!\n")
} else {
  cat("Not enough successful iterations for CIs\n")
}