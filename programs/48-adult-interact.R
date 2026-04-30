# This a script to
# run interaction regressions
# for second-generation adults with cps and
# project implicit data
# Based on the interaction approach from
# 40-interaction-iat-byparent-plot.R
# using the adult sample from
# 37-figure-five-skin-iat-regression-by-gen-plot-adults.R

# Date: April 24th, 2026

# open data and demean value
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian_adults.csv")) |>
  rename(value = lw_index) |>
  mutate(value_dm = value - mean(value, na.rm = TRUE))

# Create binary indicators
# College graduate (BA+, i.e. 16+ years) vs. non-college
CPS_IAT <- CPS_IAT |>
  mutate(CollegeGrad = as.numeric(EducYears >= 16),
         AboveMedianInc = as.numeric(ftotval > median(ftotval, na.rm = TRUE)))

library(broom)
library(ggplot2)

# =========================================================
# Interaction by Parent Type — Second-Gen Adults
# =========================================================

# Pooled AW + WA
awwa_data <- CPS_IAT |> filter((AW_0bj == 1 | WA_0bj == 1) & SecondGen_Asian == 1)
awwa_model <- feols(Asian ~ value_dm*Female +
                      value_dm*CollegeGrad + value_dm*AboveMedianInc +
                      frac_asian +
                      Age + Age_sq + Age_cube + Age_quad | region:year,
                    data = awwa_data, weights = ~weight, vcov = ~statefip)

# AW: Asian Father, White Mother
aw_data <- CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1)
aw_model <- feols(Asian ~ value_dm*Female +
                    value_dm*CollegeGrad + value_dm*AboveMedianInc +
                    frac_asian +
                    Age + Age_sq + Age_cube + Age_quad | region:year,
                  data = aw_data, weights = ~weight, vcov = ~statefip)

# WA: White Father, Asian Mother
wa_data <- CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1)
wa_model <- feols(Asian ~ value_dm*Female +
                    value_dm*CollegeGrad + value_dm*AboveMedianInc +
                    frac_asian +
                    Age + Age_sq + Age_cube + Age_quad | region:year,
                  data = wa_data, weights = ~weight, vcov = ~statefip)

# Helper to extract interaction terms and make labels readable
get_interaction_terms <- function(model) {
  dplyr::mutate(
    dplyr::filter(broom::tidy(model), grepl(":", term)),
    label = dplyr::case_when(
      grepl(":Female", term)         ~ "Bias × Female",
      grepl(":CollegeGrad", term)    ~ "Bias × College Graduate",
      grepl(":AboveMedianInc", term) ~ "Bias × Above Median Income",
      TRUE ~ term
    )
  )
}

# Extract interaction coefficients
awwa_inter <- get_interaction_terms(awwa_model)
aw_inter   <- get_interaction_terms(aw_model)
wa_inter   <- get_interaction_terms(wa_model)

# Plot interaction coefficients for pooled AW + WA
ggplot(awwa_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(title = "AW + WA: Interaction Coefficients", y = "Interaction Term", x = "Estimate") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_AWWA_adults.png"), width = 7, height = 4)

# Plot interaction coefficients for AW
ggplot(aw_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(y = "Interaction Term", x = "Estimate (95% CIs)") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_AW_adults.png"), width = 7, height = 4)

# Plot interaction coefficients for WA
ggplot(wa_inter, aes(y = label, x = estimate)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height=0.2) +
  labs(y = "Interaction Term", x = "Estimate (95% CIs)") +
  theme_customs() +
  theme(axis.text.y = element_text(size=13))
ggsave(paste0(figures_wd, "/interaction_coefficients_WA_adults.png"), width = 7, height = 4)
