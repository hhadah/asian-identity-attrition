# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# table with different FE

# Load data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index)

# Function to calculate marginal effects for logit
marginal_effects_logit <- function(model) {
  pred_prob <- predict(model, type = "response")
  mean_marginal <- mean(pred_prob * (1 - pred_prob), na.rm = TRUE)
  coefs <- coef(model)
  return(coefs * mean_marginal)
}

# Function to calculate marginal effects for probit
marginal_effects_probit <- function(model) {
  linear_pred <- predict(model, type = "link")
  mean_marginal <- mean(dnorm(linear_pred), na.rm = TRUE)
  coefs <- coef(model)
  return(coefs * mean_marginal)
}

# Function to create models with marginal effects
create_models_with_me <- function(data_subset) {
  # Fit original models
  lpm_model <- feols(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                     frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + 
                     FirstGen_Asian + SecondGen_Asian | region^year, 
                     data = data_subset, weights = ~weight, vcov = ~statefip)
  
  logit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                       frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + 
                       FirstGen_Asian + SecondGen_Asian | region^year, 
                       data = data_subset, family = "logit", weights = ~weight, vcov = ~statefip)
  
  probit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                        frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + 
                        FirstGen_Asian + SecondGen_Asian | region^year, 
                        data = data_subset, family = "binomial", weights = ~weight, vcov = ~statefip)
  
  # Calculate marginal effects
  logit_me <- marginal_effects_logit(logit_model)
  probit_me <- marginal_effects_probit(probit_model)
  
  # Create modified models with marginal effects as coefficients
  logit_me_model <- logit_model
  logit_me_model$coefficients <- logit_me
  
  probit_me_model <- probit_model
  probit_me_model$coefficients <- probit_me
  
  return(list(
    "LPM" = lpm_model,
    "Logit (ME)" = logit_me_model,
    "Probit (ME)" = probit_me_model
  ))
}

# Function for generation-specific models
create_gen_models_with_me <- function(data_subset, generation) {
  if (generation == "first") {
    # First generation specific formula (no AA_0bj, FirstGen_Asian, SecondGen_Asian)
    lpm_model <- feols(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                       frac_asian + Age + Age_sq + Age_cube + Age_quad | region^year, 
                       data = data_subset, weights = ~weight, vcov = ~statefip)
    
    logit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                         frac_asian + Age + Age_sq + Age_cube + Age_quad | region^year, 
                         data = data_subset, family = "logit", weights = ~weight, vcov = ~statefip)
    
    probit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                          frac_asian + Age + Age_sq + Age_cube + Age_quad | region^year, 
                          data = data_subset, family = "binomial", weights = ~weight, vcov = ~statefip)
  } else if (generation == "second") {
    # Second generation specific formula
    lpm_model <- feols(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                       frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj | region^year, 
                       data = data_subset, weights = ~weight, vcov = ~statefip)
    
    logit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                         frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj | region^year, 
                         data = data_subset, family = "logit", weights = ~weight, vcov = ~statefip)
    
    probit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                          frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj | region^year, 
                          data = data_subset, family = "binomial", weights = ~weight, vcov = ~statefip)
  } else if (generation == "third") {
    # Third generation specific formula
    lpm_model <- feols(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                       frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type | region^year, 
                       data = data_subset, weights = ~weight, vcov = ~statefip)
    
    logit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                         frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type | region^year, 
                         data = data_subset, family = "logit", weights = ~weight, vcov = ~statefip)
    
    probit_model <- feglm(Asian ~ value + Female + MomGradCollege + DadGradCollege + 
                          frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type | region^year, 
                          data = data_subset, family = "binomial", weights = ~weight, vcov = ~statefip)
  }
  
  # Calculate marginal effects
  logit_me <- marginal_effects_logit(logit_model)
  probit_me <- marginal_effects_probit(probit_model)
  
  # Create modified models with marginal effects as coefficients
  logit_me_model <- logit_model
  logit_me_model$coefficients <- logit_me
  
  probit_me_model <- probit_model
  probit_me_model$coefficients <- probit_me
  
  return(list(
    "LPM" = lpm_model,
    "Logit (ME)" = logit_me_model,
    "Probit (ME)" = probit_me_model
  ))
}

# Coefficient map and formatting functions
cm <- c("value" = "Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father"
        )

f1 <- function(x) format(round(x, 4), big.mark=".")  # Increased precision for marginal effects
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: region", clean = "Region FE", fmt = 0),
  list(raw = "FE: year", clean = "Year FE", fmt = 0),
  list(raw = "FE: region:year", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

options(modelsummary_format_numeric_latex = "plain")

# ALL GENERATIONS MODELS
all_gen_models <- create_models_with_me(CPS_IAT)

# Calculate means for all generations
means_all <- CPS_IAT |> 
  summarise(Asian = mean(Asian, na.rm = T))

mean_row_all <- data.frame(Coefficients = c('Mean', 
                                            round(means_all[1], digits = 2),
                                            round(means_all[1], digits = 2),
                                            round(means_all[1], digits = 2)))
colnames(mean_row_all) <- c("A", "B", "C", "D")
attr(mean_row_all, 'position') <- c(9)

# All generations table
modelsummary(all_gen_models, fmt = f1,  
                            output = "kableExtra", 
                            coef_map = cm,
                            gof_map = gm,
                            add_rows = mean_row_all,
                            escape = F,
                            stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                            title = "Asian Identity Models - All Generations (with Marginal Effects) \\label{regtab-all-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  # add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{All models include region × year fixed effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)
all_gen_tab <- modelsummary(all_gen_models, fmt = f1,  
                            output = "latex", 
                            coef_map = cm,
                            gof_map = gm,
                            add_rows = mean_row_all,
                            escape = F,
                            stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                            title = "Asian Identity Models - All Generations (with Marginal Effects) \\label{regtab-all-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  footnote(number = c("\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{All models include region × year fixed effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)

# FIRST GENERATION MODELS
first_gen_models <- create_gen_models_with_me(CPS_IAT |> filter(FirstGen_Asian == 1), "first")

# Calculate means for first generation
means_first <- CPS_IAT |> 
  filter(FirstGen_Asian == 1) |> 
  summarise(Asian = mean(Asian, na.rm = T))

mean_row_first <- data.frame(Coefficients = c('Mean', 
                                              round(means_first[1], digits = 2),
                                              round(means_first[1], digits = 2),
                                              round(means_first[1], digits = 2)))
colnames(mean_row_first) <- c("A", "B", "C", "D")
attr(mean_row_first, 'position') <- c(9)

# First generation table
modelsummary(first_gen_models, fmt = f1,  
                              output = "kableExtra", 
                              coef_map = cm,
                              gof_map = gm,
                              add_rows = mean_row_first,
                              escape = F,
                              stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                              title = "Asian Identity Models - First Generation (with Marginal Effects) \\label{regtab-first-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{First generation Asian immigrants only.}",
                      "\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)
first_gen_tab <- modelsummary(first_gen_models, fmt = f1,  
                              output = "latex", 
                              coef_map = cm,
                              gof_map = gm,
                              add_rows = mean_row_first,
                              escape = F,
                              stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                              title = "Asian Identity Models - First Generation (with Marginal Effects) \\label{regtab-first-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{First generation Asian immigrants only.}",
                      "\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)

# SECOND GENERATION MODELS
second_gen_models <- create_gen_models_with_me(CPS_IAT |> filter(SecondGen_Asian == 1), "second")

# Calculate means for second generation
means_second <- CPS_IAT |> 
  filter(SecondGen_Asian == 1) |> 
  summarise(Asian = mean(Asian, na.rm = T))

mean_row_second <- data.frame(Coefficients = c('Mean', 
                                               round(means_second[1], digits = 2),
                                               round(means_second[1], digits = 2),
                                               round(means_second[1], digits = 2)))
colnames(mean_row_second) <- c("A", "B", "C", "D")
attr(mean_row_second, 'position') <- c(9)

# Second generation table
modelsummary(second_gen_models, fmt = f1,  
                               output = "kableExtra", 
                               coef_map = cm,
                               gof_map = gm,
                               add_rows = mean_row_second,
                               escape = F,
                               stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Asian Identity Models - Second Generation (with Marginal Effects) \\label{regtab-second-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{Second generation Asian immigrants only.}",
                      "\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)
second_gen_tab <- modelsummary(second_gen_models, fmt = f1,  
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               add_rows = mean_row_second,
                               escape = F,
                               stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Asian Identity Models - Second Generation (with Marginal Effects) \\label{regtab-second-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{Second generation Asian immigrants only.}",
                      "\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)

# THIRD GENERATION MODELS
third_gen_models <- create_gen_models_with_me(CPS_IAT |> filter(ThirdGen_Asian == 1), "third")

# Calculate means for third generation
means_third <- CPS_IAT |> 
  filter(ThirdGen_Asian == 1) |> 
  summarise(Asian = mean(Asian, na.rm = T))

mean_row_third <- data.frame(Coefficients = c('Mean', 
                                              round(means_third[1], digits = 2),
                                              round(means_third[1], digits = 2),
                                              round(means_third[1], digits = 2)))
colnames(mean_row_third) <- c("A", "B", "C", "D")
attr(mean_row_third, 'position') <- c(9)

# Third generation table
modelsummary(third_gen_models, fmt = f1,  
                              output = "kableExtra", 
                              coef_map = cm,
                              gof_map = gm,
                              add_rows = mean_row_third,
                              escape = F,
                              stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                              title = "Asian Identity Models - Third Generation (with Marginal Effects) \\label{regtab-third-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{Third generation Asian immigrants only.}",
                      "\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)
third_gen_tab <- modelsummary(third_gen_models, fmt = f1,  
                              output = "latex", 
                              coef_map = cm,
                              gof_map = gm,
                              add_rows = mean_row_third,
                              escape = F,
                              stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                              title = "Asian Identity Models - Third Generation (with Marginal Effects) \\label{regtab-third-gen}") |>
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c("Model Type" = 1, "LPM" = 1, "Logit (ME)" = 1, "Probit (ME)" = 1)) |>
  footnote(number = c("\\\\footnotesize{Third generation Asian immigrants only.}",
                      "\\\\footnotesize{Linear Probability Model (LPM) shows coefficients. Logit and Probit show marginal effects.}",
                      "\\\\footnotesize{Standard errors clustered at state level.}",
                      "\\\\footnotesize{ME = Marginal Effects calculated at sample means.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T)

# Save tables
all_gen_tab |>
  save_kable(file.path(tables_wd,"tab49-all_gen_comparison_with_me.tex"))

first_gen_tab |>
  save_kable(file.path(tables_wd,"tab50-first_gen_comparison_with_me.tex"))

second_gen_tab |>
  save_kable(file.path(tables_wd,"tab51-second_gen_comparison_with_me.tex"))

third_gen_tab |>
  save_kable(file.path(tables_wd,"tab52-third_gen_comparison_with_me.tex"))

all_gen_tab |>
  save_kable(file.path(thesis_tabs,"tab49-all_gen_comparison_with_me.tex"))

first_gen_tab |>
  save_kable(file.path(thesis_tabs,"tab50-first_gen_comparison_with_me.tex"))

second_gen_tab |>
  save_kable(file.path(thesis_tabs,"tab51-second_gen_comparison_with_me.tex"))

third_gen_tab |>
  save_kable(file.path(thesis_tabs,"tab52-third_gen_comparison_with_me.tex"))


# Print summary of all marginal effects
cat("\n=== SUMMARY OF ALL VARIABLE MARGINAL EFFECTS ===\n")

print("ALL GENERATIONS:")
print("LPM Coefficients:")
print(coef(all_gen_models$LPM))
print("Logit Marginal Effects:")
print(all_gen_models$`Logit (ME)`$coefficients)
print("Probit Marginal Effects:")
print(all_gen_models$`Probit (ME)`$coefficients)

print("\nFIRST GENERATION:")
print("LPM Coefficients:")
print(coef(first_gen_models$LPM))
print("Logit Marginal Effects:")
print(first_gen_models$`Logit (ME)`$coefficients)
print("Probit Marginal Effects:")
print(first_gen_models$`Probit (ME)`$coefficients)

print("\nSECOND GENERATION:")
print("LPM Coefficients:")
print(coef(second_gen_models$LPM))
print("Logit Marginal Effects:")
print(second_gen_models$`Logit (ME)`$coefficients)
print("Probit Marginal Effects:")
print(second_gen_models$`Probit (ME)`$coefficients)

print("\nTHIRD GENERATION:")
print("LPM Coefficients:")
print(coef(third_gen_models$LPM))
print("Logit Marginal Effects:")
print(third_gen_models$`Logit (ME)`$coefficients)
print("Probit Marginal Effects:")
print(third_gen_models$`Probit (ME)`$coefficients)