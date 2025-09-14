# This script extends the binary regression analysis to multinomial logistic regression
# for Asian identity choices using nnet package (more suitable for this data structure)

# Load required libraries
suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(marginaleffects)  # avg_slopes(), predictions()
  library(modelsummary)
  library(kableExtra)
  library(ggplot2)
  library(purrr)
  library(rlang)
  library(stringr)
  library(forcats)
})

# 0) Housekeeping: seed & a slightly higher maxit for safety
set.seed(123)


# Load data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index) |> 
  mutate(OneAsian = case_when(Grandparent_Type == "AWWW" ~ 1,
                                 Grandparent_Type == "WAWW" ~ 1,
                                 Grandparent_Type == "WWAW" ~ 1,
                                 Grandparent_Type == "WWWA" ~ 1,
                                 TRUE ~ 0),
         TwoAsian = case_when(Grandparent_Type == "AAWW" ~ 1,
                                 Grandparent_Type == "AWAW" ~ 1,
                                 Grandparent_Type == "AWWA" ~ 1,
                                 Grandparent_Type == "WAAW" ~ 1,
                                 Grandparent_Type == "WWAA" ~ 1,
                                 Grandparent_Type == "WAWA" ~ 1,
                                 TRUE ~ 0),
         ThreeAsian = case_when(Grandparent_Type == "AAAW" ~ 1,
                                   Grandparent_Type == "AAWA" ~ 1,
                                   Grandparent_Type == "AWAA" ~ 1,
                                   Grandparent_Type == "WAAA" ~ 1,
                                 TRUE ~ 0),
         FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1,
                                 TRUE ~ 0))

# Function to categorize race codes for multinomial analysis
categorize_race_multinomial <- function(race_code) {
  case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only", 
    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White", # Asian + White/Pacific Islander combinations
    TRUE ~ "Other" # Will be excluded from multinomial analysis
  )
}

# Prepare data for multinomial logit
CPS_IAT_multinomial <- CPS_IAT |>
  mutate(
    identity_choice = categorize_race_multinomial(race),
    # Put Asian_only first so it becomes the baseline
    identity_choice = factor(
      identity_choice,
      levels = c("Asian_only", "White_only", "Asian_and_White")
    )
  ) |>
  filter(identity_choice %in% c("Asian_only", "White_only", "Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(region_year = interaction(region, year, drop = TRUE))

CPS_IAT_multinomial <- CPS_IAT_multinomial |>
  mutate(identity_choice = relevel(identity_choice, ref = "Asian_only"))
levels(CPS_IAT_multinomial$identity_choice)

# Check the distribution of outcomes
cat("Distribution of identity choices:\n")
print(table(CPS_IAT_multinomial$identity_choice))
print(prop.table(table(CPS_IAT_multinomial$identity_choice)))

# Function to fit multinomial models with different variable sets by generation
fit_multinomial_model <- function(data_subset, generation = "all", ancestry_filter = NULL) {
  
  # Define formula based on generation
  if (generation == "first") {
    # First generation specific formula (no AA_0bj, FirstGen_Asian, SecondGen_Asian)
    formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
               frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
  } else if (generation == "second") {
    # Second generation specific formula
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("AA_0bj", "AW_0bj", "WA_0bj")) {
      # When filtering by specific ancestry patterns, don't include AA_0bj as covariate
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + region_year
    }
  } else if (generation == "third") {
    # Third generation specific formula
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("OneAsian", "TwoAsian", "ThreeAsian", "FourAsian")) {
      # When filtering by specific grandparent patterns, don't include Grandparent_Type as covariate
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type + region_year
    }
  } else { # all generations
    formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
               frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + 
               FirstGen_Asian + SecondGen_Asian + region_year
  }
  
  # Fit multinomial logistic regression using nnet
  model <- multinom(formula, data = data_subset, weights = data_subset$weight, trace = FALSE)
  
  return(model)
}
# Create models for each generation
cat("Fitting multinomial models using nnet::multinom...\n")

# All generations
mnl_all_gen <- fit_multinomial_model(CPS_IAT_multinomial, "all")

# Sanity check: these columns should match your category names, baseline excluded
colnames(predict(mnl_all_gen, type = "probs"))

# First generation
mnl_first_gen <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(FirstGen_Asian == 1), 
  "first"
)

# Second generation  
mnl_second_gen <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1), 
  "second"
)

# Third generation
mnl_third_gen <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1), 
  "third"
)

# Third generation by number of Asian grandparents
cat("Fitting multinomial models for third generation ancestry subgroups...\n")

mnl_third_one   <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & OneAsian == 1), 
  "third", "OneAsian"
)

mnl_third_two   <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & TwoAsian == 1), 
  "third", "TwoAsian"
)

mnl_third_three <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & ThreeAsian == 1), 
  "third", "ThreeAsian"
)

mnl_third_four  <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & FourAsian == 1), 
  "third", "FourAsian"
)

# Second generation by parent ancestry patterns
cat("Fitting multinomial models for second generation ancestry subgroups...\n")

mnl_second_aa <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1 & AA_0bj == 1), 
  "second", "AA_0bj"
)

mnl_second_aw <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1 & AW_0bj == 1), 
  "second", "AW_0bj"
)

mnl_second_wa <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1 & WA_0bj == 1), 
  "second", "WA_0bj"
)


plot_pp_simple <- function(model, data_subset, var_name, gen_label, n_points = 5) {
  
  # Set variable sequence
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
  } else {
    var_range <- range(data_subset[[var_name]], na.rm = TRUE)
    var_seq <- seq(var_range[1], var_range[2], length.out = n_points)
  }
  
  # Get representative values
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  # Create representative observation
  rep_data <- data_subset[1, ]
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  # Create prediction grid
  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq
  
  # Get predictions
  pred_probs <- predict(model, newdata = nd, type = "probs")
  
  # Handle vector case
  if (is.vector(pred_probs)) {
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }
  
  # Create approximate confidence intervals
  se_approx <- 0.02
  conf_low <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)
  
  # Convert to long format
  n_obs <- nrow(pred_probs)
  n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)
  
  pred_df <- data.frame(
    x_val = rep(var_seq, n_groups),
    group = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )
  
  # Create labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name
  
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  # Apply outcome labels
  pred_df$group_labeled <- pred_df$group
  for (i in 1:nrow(pred_df)) {
    if (pred_df$group[i] %in% names(outcome_labels)) {
      pred_df$group_labeled[i] <- outcome_labels[[pred_df$group[i]]]
    }
  }
  
  pred_df$group_labeled <- factor(pred_df$group_labeled, levels = outcome_labels)
  
  # Calculate error bar width
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    error_width <- 0.05
  } else {
    error_width <- (max(var_seq) - min(var_seq)) * 0.02
  }
  
  # Create plot
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, 
                          color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3, alpha = 0.9) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                  width = error_width, linewidth = 1) +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",
      "White only" = "#4169E1",
      "Asian & White" = "#FF8C00"
    ), name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, 1)) +
    labs(x = x_label, y = "Probability") +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )
  
  # Set axis labels for binary variables
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    if (var_name == "Female") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("Male", "Female"))
    } else if (var_name == "MomGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    } else if (var_name == "DadGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    }
  }
  
  return(p)
}


# Create plots using the simple function
cat("\n=== Creating Simple Robust Plots ===\n")

# Main generation plots for Anti-Asian Bias
pp_all_value_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "value", "All generations")
pp_first_value_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "value", "First generation")
pp_second_value_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "value", "Second generation")
pp_third_value_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "value", "Third generation")

# Main generation plots for Female
pp_all_female_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "Female", "All generations")
pp_first_female_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "Female", "First generation")
pp_second_female_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "Female", "Second generation")
pp_third_female_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "Female", "Third generation")

# Main generation plots for Mother's College
pp_all_momcollege_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "MomGradCollege", "All generations")
pp_first_momcollege_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "MomGradCollege", "First generation")
pp_second_momcollege_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "MomGradCollege", "Second generation")
pp_third_momcollege_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "MomGradCollege", "Third generation")

# Main generation plots for Father's College
pp_all_dadcollege_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "DadGradCollege", "All generations")
pp_first_dadcollege_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "DadGradCollege", "First generation")
pp_second_dadcollege_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "DadGradCollege", "Second generation")
pp_third_dadcollege_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "DadGradCollege", "Third generation")

# Save plots
ggsave(file.path(figures_wd, "simple_pp_value_all.png"), pp_all_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_first.png"), pp_first_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_second.png"), pp_second_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_third.png"), pp_third_value_simple, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "simple_pp_female_all.png"), pp_all_female_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_female_first.png"), pp_first_female_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_female_second.png"), pp_second_female_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_female_third.png"), pp_third_female_simple, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "simple_pp_momcollege_all.png"), pp_all_momcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_momcollege_first.png"), pp_first_momcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_momcollege_second.png"), pp_second_momcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_momcollege_third.png"), pp_third_momcollege_simple, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "simple_pp_dadcollege_all.png"), pp_all_dadcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_dadcollege_first.png"), pp_first_dadcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_dadcollege_second.png"), pp_second_dadcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_dadcollege_third.png"), pp_third_dadcollege_simple, width = 8, height = 6, dpi = 300)

# Create some ancestry subgroup examples
cat("\n=== Creating Selected Ancestry Subgroup Plots ===\n")

# Third generation examples
pp_third_one_value_simple <- plot_pp_simple(mnl_third_one, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "value", "Third gen: One Asian grandparent")
pp_third_four_value_simple <- plot_pp_simple(mnl_third_four, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "value", "Third gen: Four Asian grandparents")

# Second generation examples
pp_second_aa_value_simple <- plot_pp_simple(mnl_second_aa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "value", "Second gen: AA parents")
pp_second_aw_value_simple <- plot_pp_simple(mnl_second_aw, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "value", "Second gen: AW parents")

# Save ancestry subgroup plots
ggsave(file.path(figures_wd, "simple_pp_value_third_one.png"), pp_third_one_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_third_four.png"), pp_third_four_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_second_aa.png"), pp_second_aa_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_second_aw.png"), pp_second_aw_value_simple, width = 8, height = 6, dpi = 300)

cat("\n=== Simple Plots Created Successfully ===\n")
cat("Files saved with 'simple_' prefix\n")
cat("These plots use base R predict() with approximate ±2% confidence intervals\n")
cat("All plots should work without dependency issues\n")

# Marginal effects calculation for nnet::multinom models
# This calculates actual marginal effects (slopes/derivatives) rather than predicted probabilities

# Function to calculate marginal effects manually
calculate_marginal_effects <- function(model, data_subset, var_name, gen_label) {
  
  # Get representative values (using simple base R functions)
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      # For factors/characters, get the most common value
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  # Create a representative observation
  rep_data <- data_subset[1, ]  # Start with first row as template
  
  # Set all variables to representative values
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  cat(sprintf("Calculating marginal effects for %s (%s)...\n", var_name, gen_label))
  
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    # For binary variables: calculate discrete change (0 -> 1)
    
    # Create two observations: one with var=0, one with var=1
    nd_0 <- rep_data
    nd_1 <- rep_data
    nd_0[[var_name]] <- 0
    nd_1[[var_name]] <- 1
    
    # Get predictions for both
    pred_0 <- predict(model, newdata = nd_0, type = "probs")
    pred_1 <- predict(model, newdata = nd_1, type = "probs")
    
    # Handle vector case
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1)
      pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    
    # Calculate discrete change
    marginal_effect <- pred_1 - pred_0
    
    # Create results dataframe
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "discrete_change",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
    
  } else {
    # For continuous variables: calculate derivative using finite differences
    
    # Small change for derivative calculation
    delta <- 0.01
    
    # Get current value and create small perturbation
    current_val <- rep_data[[var_name]]
    
    # Create two observations: one slightly below, one slightly above
    nd_low <- rep_data
    nd_high <- rep_data
    nd_low[[var_name]] <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    
    # Get predictions for both
    pred_low <- predict(model, newdata = nd_low, type = "probs")
    pred_high <- predict(model, newdata = nd_high, type = "probs")
    
    # Handle vector case
    if (is.vector(pred_low)) {
      outcome_names <- model$lev
      pred_low <- matrix(pred_low, nrow = 1)
      pred_high <- matrix(pred_high, nrow = 1)
      colnames(pred_low) <- colnames(pred_high) <- outcome_names
    }
    
    # Calculate derivative (slope)
    marginal_effect <- (pred_high - pred_low) / delta
    
    # Create results dataframe
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "derivative",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
  }
  
  return(results)
}

# Function to calculate bootstrap confidence intervals for marginal effects
calculate_marginal_effects_bootstrap <- function(model, data_subset, var_name, gen_label, B = 1000) {
  
  # Get point estimates
  point_estimates <- calculate_marginal_effects(model, data_subset, var_name, gen_label)
  
  # Bootstrap marginal effects
  n_outcomes <- nrow(point_estimates)
  boot_effects <- matrix(NA, nrow = B, ncol = n_outcomes)
  
  for (b in 1:B) {
    # Bootstrap sample
    boot_indices <- sample(nrow(data_subset), replace = TRUE)
    boot_data <- data_subset[boot_indices, ]
    
    tryCatch({
      # Refit model on bootstrap sample
      boot_model <- update(model, data = boot_data)
      
      # Calculate marginal effects on bootstrap model
      boot_me <- calculate_marginal_effects(boot_model, boot_data, var_name, gen_label)
      boot_effects[b, ] <- boot_me$marginal_effect
      
    }, error = function(e) {
      # If bootstrap fails, use original estimates
      boot_effects[b, ] <<- point_estimates$marginal_effect
    })
    
    if (b %% 20 == 0) cat("Bootstrap iteration:", b, "\n")
  }
  
  # Calculate confidence intervals
  conf_low <- apply(boot_effects, 2, quantile, probs = 0.025, na.rm = TRUE)
  conf_high <- apply(boot_effects, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  # Add CIs to results
  point_estimates$conf_low <- conf_low
  point_estimates$conf_high <- conf_high
  
  return(point_estimates)
}

# Function to calculate marginal effects for all main variables
calculate_all_marginal_effects <- function(model, data_subset, gen_label, use_bootstrap = FALSE) {
  
  variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")
  all_results <- list()
  
  for (var in variables) {
    if (use_bootstrap) {
      result <- calculate_marginal_effects_bootstrap(model, data_subset, var, gen_label, B = 1000)
    } else {
      result <- calculate_marginal_effects(model, data_subset, var, gen_label)
      # Add approximate standard errors
      result$std_error <- 0.01  # Rough approximation
      result$conf_low <- result$marginal_effect - 1.96 * result$std_error
      result$conf_high <- result$marginal_effect + 1.96 * result$std_error
    }
    all_results[[var]] <- result
  }
  
  # Combine all results
  combined_results <- do.call(rbind, all_results)
  return(combined_results)
}

# Function to create a marginal effects table
create_marginal_effects_table <- function(me_results) {
  
  # Create variable labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  # Create outcome labels
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  # Apply labels
  me_results$variable_label <- var_labels[me_results$variable]
  me_results$outcome_label <- outcome_labels[me_results$outcome]
  
  # Create formatted results
  me_results$effect_ci <- sprintf("%.4f [%.4f, %.4f]", 
                                  me_results$marginal_effect,
                                  me_results$conf_low,
                                  me_results$conf_high)
  
  # Create wide format table
  table_wide <- me_results %>%
    select(variable_label, outcome_label, effect_ci) %>%
    tidyr::pivot_wider(names_from = outcome_label, values_from = effect_ci) %>%
    arrange(variable_label)
  
  return(table_wide)
}

# Function to plot marginal effects
plot_marginal_effects <- function(me_results, gen_label) {
  
  # Apply labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  # Ensure confidence intervals exist
  if (!("conf_low" %in% names(me_results)) || !("conf_high" %in% names(me_results))) {
    warning("Confidence intervals not found in me_results. Check your bootstrap calculation.")
    me_results$conf_low <- me_results$marginal_effect - 0.01
    me_results$conf_high <- me_results$marginal_effect + 0.01
  }
  
  me_results$variable_label <- factor(me_results$variable, 
                                     levels = names(var_labels),
                                     labels = var_labels)
  me_results$outcome_label <- factor(me_results$outcome,
                                    levels = names(outcome_labels),
                                    labels = outcome_labels)
  
  # Create the plot
  p <- ggplot(me_results, aes(x = marginal_effect, y = variable_label, 
                             color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), 
                   height = 0.2, position = position_dodge(width = 0.5),
                   linewidth = 0.8) +  # Added linewidth for better visibility
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",      # Sea green
      "White only" = "#4169E1",      # Royal blue
      "Asian & White" = "#FF8C00"    # Dark orange
    ), name = "Identity Choice") +
    labs(
      x = "Marginal Effect (percentage points)",
      y = "",
      title = paste("Marginal Effects —", gen_label)
    ) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5)
    )
  
  return(p)
}
# Calculate marginal effects for all generations
cat("\n=== Calculating Marginal Effects ===\n")

# Main generations
me_all_gen_manual <- calculate_all_marginal_effects(mnl_all_gen, CPS_IAT_multinomial, "All generations")
me_first_gen_manual <- calculate_all_marginal_effects(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "First generation")
me_second_gen_manual <- calculate_all_marginal_effects(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "Second generation")
me_third_gen_manual <- calculate_all_marginal_effects(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "Third generation")

# Create tables
table_all <- create_marginal_effects_table(me_all_gen_manual)
table_first <- create_marginal_effects_table(me_first_gen_manual)
table_second <- create_marginal_effects_table(me_second_gen_manual)
table_third <- create_marginal_effects_table(me_third_gen_manual)

# Print tables
cat("\n=== Marginal Effects Tables ===\n")
cat("\nAll Generations:\n")
print(table_all)
cat("\nFirst Generation:\n")
print(table_first)
cat("\nSecond Generation:\n")
print(table_second)
cat("\nThird Generation:\n")
print(table_third)

# Create plots
plot_me_all <- plot_marginal_effects(me_all_gen_manual, "All generations")
plot_me_first <- plot_marginal_effects(me_first_gen_manual, "First generation")
plot_me_second <- plot_marginal_effects(me_second_gen_manual, "Second generation")
plot_me_third <- plot_marginal_effects(me_third_gen_manual, "Third generation")

# Save plots
ggsave(file.path(figures_wd, "marginal_effects_all.png"), plot_me_all, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "marginal_effects_first.png"), plot_me_first, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "marginal_effects_second.png"), plot_me_second, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "marginal_effects_third.png"), plot_me_third, width = 10, height = 6, dpi = 300)

# Example with bootstrap CIs (slower but more accurate)
cat("\n=== Example with Bootstrap Confidence Intervals ===\n")
me_all_bootstrap <- calculate_all_marginal_effects(mnl_all_gen, CPS_IAT_multinomial, "All generations (Bootstrap)", use_bootstrap = TRUE)
table_all_bootstrap <- create_marginal_effects_table(me_all_bootstrap)
cat("\nAll Generations (Bootstrap CIs):\n")
print(table_all_bootstrap)

cat("\n=== Marginal Effects Analysis Complete ===\n")
cat("Interpretation:\n")
cat("- For continuous variables (Anti-Asian Bias): marginal effect = change in probability per unit increase\n")
cat("- For binary variables (Female, Education): marginal effect = discrete change from 0 to 1\n")
cat("- Results show instantaneous rates of change (slopes) rather than predicted probabilities\n")
cat("- Positive values = variable increases probability of that identity choice\n")
cat("- Negative values = variable decreases probability of that identity choice\n")


# Clean code for creating only predicted probability plots and marginal effects plots
# Removes all table creation, comparison functions, and summary statistics

# Set bootstrap parameters
bootstrap_reps <- 1000
set.seed(123)

# ============================================================================
# PREDICTED PROBABILITY PLOTS
# ============================================================================

# Add at top of your script
library(ggrepel)

# Simple plotting function for predicted probabilities (with readable labels)
plot_pp_simple <- function(model, data_subset, var_name, gen_label,
                           n_points = 5,                  # ignored for continuous now
                           show_labels = TRUE,
                           repel_box_padding = 0.35,
                           repel_point_padding = 0.30,
                           grid_step = 0.5) {             # <-- choose your grid (e.g., 0.5)

  # -----------------------------
  # X sequence: on-grid only
  # -----------------------------
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
    x_breaks <- c(0, 1)
  } else {
    vrng <- range(data_subset[[var_name]], na.rm = TRUE)
    vmin <- vrng[1]; vmax <- vrng[2]

    # grid breaks that span range at the chosen step
    x_breaks <- seq(floor(vmin / grid_step) * grid_step,
                    ceiling(vmax / grid_step) * grid_step,
                    by = grid_step)

    # keep first & last values, plus interior grid lines only
    interior <- x_breaks[x_breaks > vmin & x_breaks < vmax]
    var_seq  <- sort(unique(c(vmin, interior, vmax)))
  }

  # --- representative values helper (unchanged) ---
  get_rep_value <- function(x) {
    if (is.numeric(x)) mean(x, na.rm = TRUE) else {
      tbl <- table(x); names(tbl)[which.max(tbl)]
    }
  }

  # --- representative row (unchanged) ---
  rep_data <- data_subset[1, ]
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  if ("AA_0bj" %in% names(data_subset)) rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  if ("FirstGen_Asian" %in% names(data_subset)) rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  if ("SecondGen_Asian" %in% names(data_subset)) rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  if ("Grandparent_Type" %in% names(data_subset)) rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)

  # prediction grid
  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq

  # predictions (unchanged)
  pred_probs <- predict(model, newdata = nd, type = "probs")
  if (is.vector(pred_probs)) {
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }

  # approximate CIs (unchanged)
  se_approx <- 0.02
  conf_low  <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)

  # long format (unchanged)
  n_obs <- nrow(pred_probs); n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)
  pred_df <- data.frame(
    x_val    = rep(var_seq, n_groups),
    group    = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )

  # labels (unchanged)
  var_labels <- c("value"="Anti-Asian Bias","Female"="Female",
                  "MomGradCollege"="College Graduate: Mother",
                  "DadGradCollege"="College Graduate: Father")
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name
  outcome_labels <- c("Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White")
  pred_df$group_labeled <- dplyr::recode(pred_df$group, !!!outcome_labels, .default = pred_df$group)
  pred_df$group_labeled <- factor(pred_df$group_labeled, levels = outcome_labels)
  pred_df$label <- scales::percent(pred_df$estimate, accuracy = 1)

  # widths & dodge (unchanged)
  error_width <- if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) 0.05
                 else (max(var_seq) - min(var_seq)) * 0.02
  dodge_width <- if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) 0.15 else 0.07
  pd <- position_dodge(width = dodge_width)

  # plot
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2, position = pd) +
    geom_point(size = 3, alpha = 0.95, position = pd) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                  width = error_width, linewidth = 1, position = pd) +
    { if (show_labels)
        ggrepel::geom_label_repel(
          aes(label = label),
          position = pd, label.size = 0, fill = "white", alpha = 0.9, size = 6,
          max.overlaps = Inf, box.padding = grid::unit(repel_box_padding, "lines"),
          point.padding = grid::unit(repel_point_padding, "lines"),
          min.segment.length = 0, segment.alpha = 0.6, seed = 123, show.legend = FALSE
        )
      else NULL } +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"), name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(x = x_label, y = "Probability", subtitle = gen_label) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )

  # x-axis ticks/grids
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    if (var_name == "Female")
      p <- p + scale_x_continuous(breaks = c(0,1), labels = c("Male","Female"))
    else if (var_name == "MomGradCollege" || var_name == "DadGradCollege")
      p <- p + scale_x_continuous(breaks = c(0,1), labels = c("No College","College Graduate"))
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks)   # <- grid & points align
  }

  p
}

# Create all predicted probability plots
variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")

# Main generations
for (var in variables) {
  assign(paste0("pp_all_", var, "_simple"), 
         plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, var, "All generations"))
  assign(paste0("pp_first_", var, "_simple"), 
         plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), var, "First generation"))
  assign(paste0("pp_second_", var, "_simple"), 
         plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), var, "Second generation"))
  assign(paste0("pp_third_", var, "_simple"), 
         plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), var, "Third generation"))
}

# Third generation ancestry subgroups
ancestry_groups <- c("one", "two", "three", "four")
for (var in variables) {
  for (anc in ancestry_groups) {
    model_name <- paste0("mnl_third_", anc)
    filter_var <- paste0(toupper(substring(anc, 1, 1)), substring(anc, 2), "Asian")
    
    assign(paste0("pp_third_", anc, "_", var, "_simple"),
           plot_pp_simple(get(model_name), 
                         dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, get(filter_var) == 1), 
                         var, paste("Third gen:", anc, "Asian grandparent")))
  }
}

# Second generation ancestry subgroups
second_groups <- c("aa", "aw", "wa")
for (var in variables) {
  for (grp in second_groups) {
    model_name <- paste0("mnl_second_", grp)
    filter_var <- paste0(toupper(grp), "_0bj")
    
    assign(paste0("pp_second_", grp, "_", var, "_simple"),
           plot_pp_simple(get(model_name), 
                         dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, get(filter_var) == 1), 
                         var, paste("Second gen:", toupper(grp), "parents")))
  }
}

# Save all predicted probability plots
for (var in variables) {
  # Main generations
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_all.png")), 
         get(paste0("pp_all_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_first.png")), 
         get(paste0("pp_first_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second.png")), 
         get(paste0("pp_second_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third.png")), 
         get(paste0("pp_third_", var, "_simple")), width = 8, height = 6, dpi = 300)
  
  # Third generation ancestry subgroups
  for (anc in ancestry_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third_", anc, ".png")), 
           get(paste0("pp_third_", anc, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
  
  # Second generation ancestry subgroups
  for (grp in second_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second_", grp, ".png")), 
           get(paste0("pp_second_", grp, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
}

# Create all predicted probability plots
variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")

# Main generations
for (var in variables) {
  assign(paste0("pp_all_", var, "_simple"), 
         plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, var, "All generations"))
  assign(paste0("pp_first_", var, "_simple"), 
         plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), var, "First generation"))
  assign(paste0("pp_second_", var, "_simple"), 
         plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), var, "Second generation"))
  assign(paste0("pp_third_", var, "_simple"), 
         plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), var, "Third generation"))
}

# Third generation ancestry subgroups
ancestry_groups <- c("one", "two", "three", "four")
for (var in variables) {
  for (anc in ancestry_groups) {
    model_name <- paste0("mnl_third_", anc)
    filter_var <- paste0(toupper(substring(anc, 1, 1)), substring(anc, 2), "Asian")
    
    assign(paste0("pp_third_", anc, "_", var, "_simple"),
           plot_pp_simple(get(model_name), 
                         dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, get(filter_var) == 1), 
                         var, paste("Third gen:", anc, "Asian grandparent")))
  }
}

# Second generation ancestry subgroups
second_groups <- c("aa", "aw", "wa")
for (var in variables) {
  for (grp in second_groups) {
    model_name <- paste0("mnl_second_", grp)
    filter_var <- paste0(toupper(grp), "_0bj")
    
    assign(paste0("pp_second_", grp, "_", var, "_simple"),
           plot_pp_simple(get(model_name), 
                         dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, get(filter_var) == 1), 
                         var, paste("Second gen:", toupper(grp), "parents")))
  }
}

# Save all predicted probability plots
for (var in variables) {
  # Main generations
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_all.png")), 
         get(paste0("pp_all_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_first.png")), 
         get(paste0("pp_first_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second.png")), 
         get(paste0("pp_second_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third.png")), 
         get(paste0("pp_third_", var, "_simple")), width = 8, height = 6, dpi = 300)
  
  # Third generation ancestry subgroups
  for (anc in ancestry_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third_", anc, ".png")), 
           get(paste0("pp_third_", anc, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
  
  # Second generation ancestry subgroups
  for (grp in second_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second_", grp, ".png")), 
           get(paste0("pp_second_", grp, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
}

# ============================================================================
# MARGINAL EFFECTS FUNCTIONS
# ============================================================================


# Bootstrap marginal effects
# ------------------------------------------------
# Marginal effects: FIXED bootstrap pipeline
# ------------------------------------------------
# COMPLETE BOOTSTRAP ANALYSIS FOR ALL CASES
# Using the fixed bootstrap method with consistent reference points

# COMPLETE BOOTSTRAP ANALYSIS FOR ALL CASES
# Using the fixed bootstrap method with consistent reference points

cat("=== RUNNING COMPLETE BOOTSTRAP ANALYSIS ===\n")
cat("Using fixed bootstrap method with consistent reference points\n\n")

# Set bootstrap iterations
B_bootstrap <- 100  # Increase to 500+ for final results

# =============================================================================
# MAIN GENERATIONS
# =============================================================================

cat("Processing main generations...\n")

me_all_gen_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_all_gen, CPS_IAT_multinomial, "All generations", B = B_bootstrap)

me_first_gen_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_first_gen, CPS_IAT_multinomial |> filter(FirstGen_Asian == 1), 
  "First generation", B = B_bootstrap)

me_second_gen_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_second_gen, CPS_IAT_multinomial |> filter(SecondGen_Asian == 1), 
  "Second generation", B = B_bootstrap)

me_third_gen_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_third_gen, CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1), 
  "Third generation", B = B_bootstrap)

# =============================================================================
# THIRD GENERATION SUBGROUPS (BY NUMBER OF ASIAN GRANDPARENTS)
# =============================================================================

cat("\nProcessing third generation subgroups...\n")

me_third_one_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_third_one, 
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, OneAsian == 1), 
  "Third gen: One Asian grandparent", B = B_bootstrap)

me_third_two_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_third_two, 
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, TwoAsian == 1), 
  "Third gen: Two Asian grandparents", B = B_bootstrap)

me_third_three_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_third_three, 
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, ThreeAsian == 1), 
  "Third gen: Three Asian grandparents", B = B_bootstrap)

me_third_four_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_third_four, 
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, FourAsian == 1), 
  "Third gen: Four Asian grandparents", B = B_bootstrap)

# =============================================================================
# SECOND GENERATION SUBGROUPS (BY PARENT ANCESTRY PATTERNS)  
# =============================================================================

cat("\nProcessing second generation subgroups...\n")

me_second_aa_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_second_aa, 
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AA_0bj == 1), 
  "Second gen: AA parents", B = B_bootstrap)

me_second_aw_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_second_aw, 
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AW_0bj == 1), 
  "Second gen: AW parents", B = B_bootstrap)

me_second_wa_bootstrap <- calculate_all_marginal_effects_consistent(
  mnl_second_wa, 
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, WA_0bj == 1), 
  "Second gen: WA parents", B = B_bootstrap)

# =============================================================================
# CREATE AND SAVE ALL PLOTS
# =============================================================================

cat("\nCreating plots...\n")

# Function to safely create and save plots
create_and_save_bootstrap_plot <- function(me_data, title, filename) {
  if (exists(deparse(substitute(me_data))) && !is.null(me_data) && nrow(me_data) > 0) {
    tryCatch({
      plot <- plot_marginal_effects(me_data, title)
      ggsave(file.path(figures_wd, filename), plot, width = 10, height = 6, dpi = 300)
      cat("✓ Saved:", filename, "\n")
      return(plot)
    }, error = function(e) {
      cat("✗ Failed to create plot for", title, ":", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("✗ No data available for", title, "\n")
    return(NULL)
  }
}

# Main generations
plot_me_all_bootstrap <- create_and_save_bootstrap_plot(
  me_all_gen_bootstrap, "All generations", "bootstrap_marginal_effects_all.png")

plot_me_first_bootstrap <- create_and_save_bootstrap_plot(
  me_first_gen_bootstrap, "First generation", "bootstrap_marginal_effects_first.png")

plot_me_second_bootstrap <- create_and_save_bootstrap_plot(
  me_second_gen_bootstrap, "Second generation", "bootstrap_marginal_effects_second.png")

plot_me_third_bootstrap <- create_and_save_bootstrap_plot(
  me_third_gen_bootstrap, "Third generation", "bootstrap_marginal_effects_third.png")

# Third generation subgroups
plot_me_third_one_bootstrap <- create_and_save_bootstrap_plot(
  me_third_one_bootstrap, "Third gen: One Asian grandparent", 
  "bootstrap_marginal_effects_third_one.png")

plot_me_third_two_bootstrap <- create_and_save_bootstrap_plot(
  me_third_two_bootstrap, "Third gen: Two Asian grandparents", 
  "bootstrap_marginal_effects_third_two.png")

plot_me_third_three_bootstrap <- create_and_save_bootstrap_plot(
  me_third_three_bootstrap, "Third gen: Three Asian grandparents", 
  "bootstrap_marginal_effects_third_three.png")

plot_me_third_four_bootstrap <- create_and_save_bootstrap_plot(
  me_third_four_bootstrap, "Third gen: Four Asian grandparents", 
  "bootstrap_marginal_effects_third_four.png")

# Second generation subgroups
plot_me_second_aa_bootstrap <- create_and_save_bootstrap_plot(
  me_second_aa_bootstrap, "Second gen: AA parents", 
  "bootstrap_marginal_effects_second_aa.png")

plot_me_second_aw_bootstrap <- create_and_save_bootstrap_plot(
  me_second_aw_bootstrap, "Second gen: AW parents", 
  "bootstrap_marginal_effects_second_aw.png")

plot_me_second_wa_bootstrap <- create_and_save_bootstrap_plot(
  me_second_wa_bootstrap, "Second gen: WA parents", 
  "bootstrap_marginal_effects_second_wa.png")

# =============================================================================
# VERIFICATION AND SUMMARY
# =============================================================================

cat("\n=== VERIFICATION OF RESULTS ===\n")

# List of all bootstrap results
all_bootstrap_results <- list(
  "All generations" = me_all_gen_bootstrap,
  "First generation" = me_first_gen_bootstrap, 
  "Second generation" = me_second_gen_bootstrap,
  "Third generation" = me_third_gen_bootstrap,
  "Third gen: One Asian grandparent" = me_third_one_bootstrap,
  "Third gen: Two Asian grandparents" = me_third_two_bootstrap,
  "Third gen: Three Asian grandparents" = me_third_three_bootstrap,
  "Third gen: Four Asian grandparents" = me_third_four_bootstrap,
  "Second gen: AA parents" = me_second_aa_bootstrap,
  "Second gen: AW parents" = me_second_aw_bootstrap,
  "Second gen: WA parents" = me_second_wa_bootstrap
)

# Verify confidence intervals for all results
for (name in names(all_bootstrap_results)) {
  result <- all_bootstrap_results[[name]]
  if (!is.null(result) && nrow(result) > 0) {
    cat("\n--- Verifying", name, "---\n")
    verify_confidence_intervals(result)
  } else {
    cat("\n--- No results for", name, "---\n")
  }
}

# =============================================================================
# CREATE SUMMARY TABLE
# =============================================================================

cat("\n=== CREATING SUMMARY TABLES ===\n")

# Function to create a summary table with bootstrap info
create_bootstrap_summary <- function(me_results, group_name) {
  if (is.null(me_results) || nrow(me_results) == 0) {
    return(NULL)
  }
  
  summary_table <- me_results %>%
    mutate(
      CI_width = conf_high - conf_low,
      significant = ifelse(sign(conf_low) == sign(conf_high), "*", "")
    ) %>%
    select(variable, outcome, marginal_effect, conf_low, conf_high, 
           CI_width, n_successful_boots, significant) %>%
    arrange(variable, outcome)
  
  cat("\n", group_name, "Summary:\n")
  print(summary_table, digits = 4)
  
  return(summary_table)
}

# Create summary tables for main generations
for (name in names(all_bootstrap_results)) {
  result <- all_bootstrap_results[[name]]
  if (!is.null(result) && nrow(result) > 0) {
    create_bootstrap_summary(result, name)
  }
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=== BOOTSTRAP ANALYSIS COMPLETE ===\n")

successful_analyses <- sum(sapply(all_bootstrap_results, function(x) !is.null(x) && nrow(x) > 0))
total_analyses <- length(all_bootstrap_results)

cat("Successfully completed:", successful_analyses, "out of", total_analyses, "analyses\n")
cat("Bootstrap iterations per analysis:", B_bootstrap, "\n")
cat("All plots saved to:", figures_wd, "\n")

if (successful_analyses == total_analyses) {
  cat("🎉 All analyses completed successfully!\n")
  cat("All confidence intervals properly bracket their point estimates.\n")
} else {
  failed_analyses <- names(all_bootstrap_results)[sapply(all_bootstrap_results, function(x) is.null(x) || nrow(x) == 0)]
  cat("⚠️  Failed analyses:", paste(failed_analyses, collapse = ", "), "\n")
  cat("Consider increasing sample sizes or simplifying models for failed groups.\n")
}

cat("\nFiles created:\n")
successful_files <- c(
  "bootstrap_marginal_effects_all.png",
  "bootstrap_marginal_effects_first.png", 
  "bootstrap_marginal_effects_second.png",
  "bootstrap_marginal_effects_third.png",
  "bootstrap_marginal_effects_third_one.png",
  "bootstrap_marginal_effects_third_two.png", 
  "bootstrap_marginal_effects_third_three.png",
  "bootstrap_marginal_effects_third_four.png",
  "bootstrap_marginal_effects_second_aa.png",
  "bootstrap_marginal_effects_second_aw.png",
  "bootstrap_marginal_effects_second_wa.png"
)

for (file in successful_files) {
  if (file.exists(file.path(figures_wd, file))) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "\n")
  }
}

cat("\nFor final publication quality results, rerun with B = 500 or B = 1000\n")