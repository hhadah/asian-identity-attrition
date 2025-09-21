#!/usr/bin/env Rscript
# Quick test of the margins package compatibility

suppressPackageStartupMessages({
  library(nnet)
  library(margins)
})

cat("=== Testing margins package compatibility with multinom ===\n")

# Create simple test data
set.seed(123)
n <- 500
test_data <- data.frame(
  x1 = rnorm(n),
  x2 = rbinom(n, 1, 0.5),
  group = sample(1:3, n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
)
test_data$y <- factor(test_data$group, levels = 1:3, labels = c("A", "B", "C"))

cat(sprintf("Test data: %d observations with 3 outcomes\n", n))
print(table(test_data$y))

# Fit multinomial model
cat("\nFitting multinomial model...\n")
model <- multinom(y ~ x1 + x2, data = test_data, trace = FALSE)

# Test margins package compatibility
cat("Testing margins package...\n")
tryCatch({
  # Try basic margins calculation
  marg_results <- margins(model, data = test_data)
  marg_summary <- summary(marg_results)
  
  cat("✓ SUCCESS: margins package works with multinom!\n")
  cat(sprintf("Results: %d marginal effects calculated\n", nrow(marg_summary)))
  
  # Show sample results
  if (nrow(marg_summary) > 0) {
    cat("Sample output:\n")
    print(head(marg_summary, 8))
    
    cat(sprintf("\nAME range: [%.4f, %.4f]\n", 
               min(marg_summary$AME), max(marg_summary$AME)))
    cat(sprintf("SE range: [%.4f, %.4f]\n", 
               min(marg_summary$SE), max(marg_summary$SE)))
    
    cat("\n✓ The margins package approach should work for our analysis!\n")
  }
  
}, error = function(e) {
  cat("✗ FAILED: margins package incompatible with multinom\n")
  cat("Error:", conditionMessage(e), "\n")
  cat("\n→ Recommendation: Continue using analytical method\n")
})

cat("\n=== Test completed ===\n")