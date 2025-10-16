# Quick test of the bootstrap functionality
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

# Load data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>
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
  mutate(region_year = interaction(region, year, drop = TRUE))

CPS_IAT_multinomial <- CPS_IAT_multinomial |>
  mutate(identity_choice = relevel(identity_choice, ref = "Asian_only"))

# Simple test model
fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
  frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj +
  FirstGen_Asian + SecondGen_Asian + region_year

# Take a smaller sample for testing
test_data <- CPS_IAT_multinomial[sample(nrow(CPS_IAT_multinomial), 1000), ]

cat("Fitting test model...\n")
test_model <- multinom(fml, data = test_data, weights = test_data$weight, trace = FALSE)

cat("Model fitted successfully!\n")
cat("Convergence:", test_model$converged, "\n")
cat("Sample size:", nrow(test_data), "\n")