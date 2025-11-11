# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# open data and demean value
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index) |> 
  mutate(value_dm = value - mean(value, na.rm = TRUE))


# Only interethnic groups: AW and WA
# Add interaction terms: value_dm*Female, value_dm*MomGradCollege, value_dm*DadGradCollege

# AW: Asian Father, White Mother
aw_data <- CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1)
aw_model <- feols(Asian ~ value_dm*Female + value_dm*MomGradCollege + value_dm*DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad | region:year, data = aw_data, weights = ~weight, vcov = ~statefip)

# WA: White Father, Asian Mother
wa_data <- CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1)
wa_model <- feols(Asian ~ value_dm*Female + value_dm*MomGradCollege + value_dm*DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad | region:year, data = wa_data, weights = ~weight, vcov = ~statefip)




# Plot only the interaction coefficients from the regression results
library(broom)
library(ggplot2)


# Helper to extract interaction terms and make labels readable
get_interaction_terms <- function(model) {
  dplyr::mutate(
    dplyr::filter(broom::tidy(model), grepl(":", term)),
    label = dplyr::case_when(
      grepl(":Female", term) ~ "Bias × Female",
      grepl(":MomGradCollege", term) ~ "Bias × Mom Grad College",
      grepl(":DadGradCollege", term) ~ "Bias × Dad Grad College",
      TRUE ~ term
    )
  )
}

# AW interaction coefficients
aw_inter <- get_interaction_terms(aw_model)
wa_inter <- get_interaction_terms(wa_model)

# Plot interaction coefficients for AW (coefficients on y-axis)
ggplot(aw_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(title = "AW: Interaction Coefficients", y = "Interaction Term", x = "Estimate") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_AW.png"), width = 7, height = 4)

# Plot interaction coefficients for WA (coefficients on y-axis)
ggplot(wa_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(title = "WA: Interaction Coefficients", y = "Interaction Term", x = "Estimate") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_WA.png"), width = 7, height = 4)

