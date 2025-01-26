# This a script for
# a multi logit reg-
# ression where the 
# LHS variable is
# parents types
# and RHS is bias

# Date: Oct 12th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  filter(SecondGen_Asian == 1) |> 
  mutate(ParentType2 = as.ordered(ParentType2),
         Endogamouse = case_when(ParentType2 == "Asian-Asian" ~ 1,
                                 ParentType2 == "Asian-White" ~ 0,
                                 ParentType2 == "White-Asian" ~ 0),
         Interethnic = case_when(ParentType2 == "Asian-Asian" ~ 0,
                                 ParentType2 == "Asian-White" ~ 1,
                                 ParentType2 == "White-Asian" ~ 1)) |> 
  rename("Bias" = "lw_index")
reg <- list(
  "\\specialcell{(1) \\\\ Interracial}" = feols(Interethnic ~ Bias + 
                                                  MomGradCollege + DadGradCollege + age_mom + age_pop +
                                                  yrimmig_mom + yrimmig_pop
                                            | region:year, 
                                          data = CPS_IAT, weights = ~hwtfinl, vcov = ~serial),
  "\\specialcell{(2) \\\\ Interracial}" = feols(Interethnic ~ 1 + Bias + 
                                                  MomGradCollege + DadGradCollege + age_mom + age_pop +
                                                  yrimmig_mom + yrimmig_pop
                                                | region:year, 
                                                data = CPS_IAT |> filter(Asian_Dad == 1), weights = ~hwtfinl, vcov = ~serial),
  "\\specialcell{(3) \\\\ Interracial}" = feols(Interethnic ~ 1 + Bias + 
                                                    MomGradCollege + DadGradCollege + age_mom + age_pop +
                                                    yrimmig_mom + yrimmig_pop
                                                  | region:year, 
                                                  data = CPS_IAT |> filter(Asian_Mom == 1), weights = ~hwtfinl, vcov = ~serial)
)


cm <- c(
  "Bias"                             = "Bias",
  "MomGradCollege"                   = "College Graduate: Wife",
  "DadGradCollege"                   = "College Graduate: Husband"
  # "age_mom"                          = "Wife's Age",
  # "age_pop"                          = "Husband's Age",                   
  # "yrimmig_mom"                      = "Year Immigrated: Wife",
  # "yrimmig_pop"                      = "Year Immigrated: Husband"
) 

f1 <- function(x) format(round(x, 2), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: region:year", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

# mfx <- marginaleffects(reg[[2]], variables = "Bias")

modelsummary(reg,
             fmt = f1,
             coef_map = cm,
             gof_map = gm,
             # gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
             stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), 
                latex_options = "scale_down"
  ) |> 
  add_header_above(c(" " = 2, #"Multinomial Logit" = 1,
                     "Asian Men" = 1, "Asian Women" = 1))

regression_tab <- modelsummary(reg, #exponentiate = c(F, T, F, F),
                               fmt = 2,
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               # gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Relationship Between Bias and Interracial Marriages \\label{regtab-logit-02}")  |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),
                latex_options = c("HOLD_position")
  ) |>
  footnote(number = c("\\\\footnotesize{This is the result to estimating (\\\\ref{eq:inter-interracial}) as a
                      linear probability model.}",
                      "\\\\footnotesize{I include controls for partners' sex, age, education, 
                      and years since immigrating to the United States.
                      Standard errors are clustered on the household level.}",
                      "\\\\footnotesize{Data source is the 2004-2020 Current Population Survey Data.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T) |> 
  add_header_above(c(" " = 2, "Asian Men" = 1, "Asian Women" = 1))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab41a-marriage.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab41a-marriage.tex"))

regression_tab <- modelsummary(reg, #exponentiate = c(F, T, F, F),
                               fmt = 2,
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               # gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)
                               )  |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),
                latex_options = c("HOLD_position")
  ) |> 
  add_header_above(c(" " = 2, "Asian Men" = 1, "Asian Women" = 1))


reg <- list(
  "\\specialcell{(1) \\\\ Interracial}" = feglm(Interethnic ~ Bias + 
                                                  MomGradCollege + DadGradCollege + age_mom + age_pop +
                                                  yrimmig_mom + yrimmig_pop
                                            | region:year, family = "logit",
                                          data = CPS_IAT, weights = ~hwtfinl, vcov = ~serial),
  "\\specialcell{(2) \\\\ Interracial}" = feglm(Interethnic ~ 1 + Bias + 
                                                  MomGradCollege + DadGradCollege + age_mom + age_pop +
                                                  yrimmig_mom + yrimmig_pop
                                                | region:year, family = "logit", 
                                                data = CPS_IAT |> filter(Asian_Dad == 1), weights = ~hwtfinl, vcov = ~serial),
  "\\specialcell{(3) \\\\ Interracial}" = feglm(Interethnic ~ 1 + Bias + 
                                                    MomGradCollege + DadGradCollege + age_mom + age_pop +
                                                    yrimmig_mom + yrimmig_pop
                                                  | region:year, family = "logit", 
                                                  data = CPS_IAT |> filter(Asian_Mom == 1), weights = ~hwtfinl, vcov = ~serial)
)

cm <- c(
  "Bias"                             = "Bias",
  "MomGradCollege"                   = "College Graduate: Wife",
  "DadGradCollege"                   = "College Graduate: Husband"
  # "age_mom"                          = "Wife's Age",
  # "age_pop"                          = "Husband's Age",                   
  # "yrimmig_mom"                      = "Year Immigrated: Wife",
  # "yrimmig_pop"                      = "Year Immigrated: Husband"
) 

f1 <- function(x) format(round(x, 2), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: region:year", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

# mfx <- marginaleffects(reg[[2]], variables = "Bias")

modelsummary(reg,
             fmt = f1,
             coef_map = cm,
             gof_map = gm,
            exponentiate = TRUE,
             # gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
             stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), 
                latex_options = "scale_down"
  ) |> 
  add_header_above(c(" " = 2, #"Multinomial Logit" = 1,
                     "Asian Men" = 1, "Asian Women" = 1))

regression_tab <- modelsummary(reg, #exponentiate = c(F, T, F, F),
                               fmt = 2,
                               output = "latex", 
                               exponentiate = TRUE,
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               # gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Logistic Regression Analysis of Bias and Interracial Marriages \\label{regtab-logit-03}")  |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),
                latex_options = c("HOLD_position")
  ) |>
  footnote(number = c("\\\\footnotesize{This is the result to estimating (\\\\ref{eq:inter-interracial}) as a
                        logistic regression. The coefficients are exponentiated, thus should be interpreted as odds ratios.}",
                      "\\\\footnotesize{I include controls for partners' sex, age, education, 
                      and years since immigrating to the United States.
                      Standard errors are clustered on the household level.}",
                      "\\\\footnotesize{Data source is the 2004-2020 Current Population Survey Data.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T) |> 
  add_header_above(c(" " = 2, "Asian Men" = 1, "Asian Women" = 1))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab47-marriage.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab47-marriage.tex"))

regression_tab <- modelsummary(reg, #exponentiate = c(F, T, F, F),
                               fmt = 2,
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               # gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)
                               )  |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),
                latex_options = c("HOLD_position")
  ) |> 
  add_header_above(c(" " = 2, "Asian Men" = 1, "Asian Women" = 1))
