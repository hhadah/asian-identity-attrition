# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# open data and demean value
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
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
                                 TRUE ~ 0)) |> 
  rename(value = lw_index) |> 
  mutate(value_dm = value - mean(value, na.rm = TRUE))




# Only interethnic groups: One, Two, Three Asian grandparents
# Add interaction terms: value_dm*Female, value_dm*MomGradCollege, value_dm*DadGradCollege

# Only interethnic groups: One, Two, Three Asian grandparents
# Add interaction terms: value_dm*Female, value_dm*MomGradCollege, value_dm*DadGradCollege

# One Asian grandparent
one_data <- CPS_IAT |> filter(ThirdGen_Asian == 1 & OneAsian == 1)
one_model <- feols(Asian ~ value_dm*Female + value_dm*MomGradCollege + value_dm*DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad | region:year, data = one_data, weights = ~weight, vcov = ~statefip)

# Two Asian grandparents
two_data <- CPS_IAT |> filter(ThirdGen_Asian == 1 & TwoAsian == 1)
two_model <- feols(Asian ~ value_dm*Female + value_dm*MomGradCollege + value_dm*DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad | region:year, data = two_data, weights = ~weight, vcov = ~statefip)

# Three Asian grandparents
three_data <- CPS_IAT |> filter(ThirdGen_Asian == 1 & ThreeAsian == 1)
three_model <- feols(Asian ~ value_dm*Female + value_dm*MomGradCollege + value_dm*DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad | region:year, data = three_data, weights = ~weight, vcov = ~statefip)


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

# Extract interaction coefficients for each group
one_inter <- get_interaction_terms(one_model)
two_inter <- get_interaction_terms(two_model)
three_inter <- get_interaction_terms(three_model)

# Plot interaction coefficients for One Asian Grandparent
ggplot(one_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(title = "One Asian Grandparent: Interaction Coefficients", y = "Interaction Term", x = "Estimate") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_OneAsian.png"), width = 7, height = 4)

# Plot interaction coefficients for Two Asian Grandparents
ggplot(two_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(title = "Two Asian Grandparents: Interaction Coefficients", y = "Interaction Term", x = "Estimate") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_TwoAsian.png"), width = 7, height = 4)

# Plot interaction coefficients for Three Asian Grandparents
ggplot(three_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(title = "Three Asian Grandparents: Interaction Coefficients", y = "Interaction Term", x = "Estimate") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_ThreeAsian.png"), width = 7, height = 4)
