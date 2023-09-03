# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT.csv"))

# fixed effects regression

reg1 <- list(
  "\\specialcell{(1) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad + factor(ParentType2) | region:year, 
                     data = CPS_IAT |> filter(SecondGen == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad| region:year, 
                     data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip)
  # "\\specialcell{(5) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                         + MomGradCollege + DadGradCollege + 
  #                                           Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
  #                                         data = CPS_IAT |> filter(SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(6) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                         + MomGradCollege + DadGradCollege + 
  #                                           Age + Age_sq + Age_cube + Age_quad| region:year, 
  #                                         data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(7) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                         + MomGradCollege + DadGradCollege + 
  #                                           Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                                         data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(8) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                         + MomGradCollege + DadGradCollege + 
  #                                           Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                                         data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(9) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                         + MomGradCollege + DadGradCollege + 
  #                                           Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
  #                                         data = CPS_IAT |> filter(SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(10) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + 
  #                                            Age + Age_sq + Age_cube + Age_quad| region:year, 
  #                                          data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(11) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + 
  #                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                                          data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(12) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + 
  #                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                                          data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(13) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + lnftotval_mom +
  #                                            Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
  #                                          data = CPS_IAT |> filter(SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(14) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + lnftotval_mom +
  #                                            Age + Age_sq + Age_cube + Age_quad| region:year, 
  #                                          data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(15) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + lnftotval_mom +
  #                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                                          data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  # "\\specialcell{(16) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
  #                                          + MomGradCollege + DadGradCollege + lnftotval_mom +
  #                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                                          data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip)
  # "Hispanic" = feols(Hispanic ~ 1 + value + Female 
  #                    + MomGradCollege + DadGradCollege + 
  #                      Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                    data = CPS_IAT |> filter(WW_0bj == 1 & ThirdGen == 1), weights = ~weight, vcov = ~statefip)
  
)


cm <- c("value" = "Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom" = "Log Total Family Income"
        #"age" = "Age",
        #"HH" = "Both parents Hispanic",
        # "FirstGen" = "First Gen",
        # "SecondGen" = "Second Gen",
        # "ThirdGen" = "Third Generation"
) 

f1 <- function(x) format(round(x, 2), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: region", clean = "Region FE", fmt = 0),
  list(raw = "FE: year", clean = "Year FE", fmt = 0),
  list(raw = "FE: region:year", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

options(modelsummary_format_numeric_latex = "plain")

# calculate means to add
# as a row
means_gen <- CPS_IAT |> 
  filter(SecondGen == 1) |> 
  group_by(ParentType2) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

means_all <- CPS_IAT |> 
  filter(SecondGen == 1) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

means_gen_asec <- CPS_IAT |> 
  filter(SecondGen == 1 & asecflag == 1) |> 
  group_by(ParentType2) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

means_all_asec <- CPS_IAT |> 
  filter(SecondGen == 1 & asecflag == 1) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

means_gen_notasec <- CPS_IAT |> 
  filter(SecondGen == 1 & (asecflag != 1 | is.na(asecflag))) |> 
  group_by(ParentType2) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

means_all_notasec <- CPS_IAT |> 
  filter(SecondGen == 1 & (asecflag != 1 | is.na(asecflag))) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

mean_row <-  data.frame(Coefficients = c('Mean', round(means_all[1], digits = 2),      round(means_gen[1,2], digits = 2),      round(means_gen[2,2], digits = 2),      round(means_gen[3,2], digits = 2),
                                                 round(means_all_asec[1], digits = 2), round(means_gen_asec[1,2], digits = 2), round(means_gen_asec[2,2], digits = 2), round(means_gen_asec[3,2], digits = 2),
                                                 round(means_all_notasec[1], digits = 2), round(means_gen_notasec[1,2], digits = 2), round(means_gen_notasec[2,2], digits = 2), round(means_gen_notasec[3,2], digits = 2),
                                                 round(means_all_asec[1], digits = 2), round(means_gen_asec[1,2], digits = 2), round(means_gen_asec[2,2], digits = 2), round(means_gen_asec[3,2], digits = 2)
                                         ))

colnames(mean_row)<-LETTERS[1:17]

attr(mean_row, 'position') <- c(11)

regression_tab <- modelsummary(reg1, fmt = f1,  
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               add_rows = mean_row[1:5],
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Relationship Between Bias and Self-Reported Hispanic identity Among Second-Generation Hispanic Immigrants: By Parental Type \\label{regtab-byparent-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{Each column is an estimation of a heterogeneous effect of regression (\\\\ref{eq:identity_reg_bias}) by 
                      type of parents with region × year fixed effects. 
                      I include controls for sex, quartic age, fraction of Hispanics in a state, and parental education.
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{The samples include second-generation Hispanic children ages 17 and below who live in intact families. 
                      Native-born second-generation Hispanic 
                      immigrant children with at least one parent born in a Spanish-speaking 
                      country.}",
                      "\\\\footnotesize{Column (1) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on all second-generation immigrants, 
                                        column (2) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on second-generation immigrants that who has a father and mother that were born in a Spanish-speaking country (HH),
                                        column (3) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on second-generation immigrants that who has a father that was born in a Spanish-speaking country and a native-born mother (HW), and
                                        column (4) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on second-generation immigrants that who has a native-born father and a mother that was born in a Spanish-speaking country (WH).}",
                      "\\\\footnotesize{Data source is the 2004-2021 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T
  ) |> 
  add_header_above(c("Parents Type" = 1, "All" = 1, "Both Parents \n from Spanish \n Speaking Country \n (HH)" = 1, "Father \n from Spanish \n Speaking Country \n (HW)" = 1, "Mother  \n from Spanish \n Speaking Country \n (WH)" = 1#,
                              # "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1,
                              # "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1,
                              # "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1
                     )) 
# |> 
#   add_header_above(c("Sample" = 1, "All Sample" = 1,
#                      "March Sup." = 1,
#                      "non-March Sup." = 1,
#                      "March Sup. and Control for Income" = 4
#   ))


regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab05-iat_regression_tab_byparent_type.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab05-iat_regression_tab_byparent_type.tex"))

# fixed effects regression

# reg2 <- list(
#   "\\specialcell{(1) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
#                                           data = CPS_IAT |> filter(SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(2) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad| region:year, 
#                                           data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(3) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                           data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(4) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                           data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(5) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
#                                           data = CPS_IAT |> filter(SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(6) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad| region:year, 
#                                           data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(7) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                           data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(8) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                           data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(9) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
#                                           data = CPS_IAT |> filter(SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(10) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + 
#                                              Age + Age_sq + Age_cube + Age_quad| region:year, 
#                                            data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(11) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + 
#                                              Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                            data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(12) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + 
#                                              Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                            data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(13) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + lnftotval_mom +
#                                              Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
#                                            data = CPS_IAT |> filter(SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(14) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + lnftotval_mom +
#                                              Age + Age_sq + Age_cube + Age_quad| region:year, 
#                                            data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(15) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + lnftotval_mom +
#                                              Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                            data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(16) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + Explicit_value + Female 
#                                            + MomGradCollege + DadGradCollege + lnftotval_mom +
#                                              Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                            data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip)
#   # "Hispanic" = feols(Hispanic ~ 1 + Explicit_value + Female 
#   #                    + MomGradCollege + DadGradCollege + 
#   #                      Age + Age_sq + Age_cube + Age_quad | region:year, 
#   #                    data = CPS_IAT |> filter(WW_0bj == 1 & ThirdGen == 1), weights = ~weight, vcov = ~statefip)
#   
# )
# 
# 
# cm <- c("Explicit_value" = "Explicit Bias",
#         "Female" = "Female",
#         "MomGradCollege" = "College Graduate: Mother",
#         "DadGradCollege" = "College Graduate: Father",
#         "lnftotval_mom" = "Log Total Family Income"
#         #"age" = "Age",
#         #"HH" = "Both parents Hispanic",
#         # "FirstGen" = "First Gen",
#         # "SecondGen" = "Second Gen",
#         # "ThirdGen" = "Third Generation"
# ) 
# gm <- tibble::tribble(
#   ~raw,        ~clean,          ~fmt,
#   "nobs",      "N",             0,
#   "FE: region", "Region FE", 0,
#   "FE: year", "Year FE", 0,
#   "FE: region:year", "Year $\\times$ Region FE", 0,
#   "std.error.type", "Standard Errors", 0,
#   #"r.squared", "R squared", 3
# )
# 
# regression_tab2 <- modelsummary(reg2, fmt = f1,  
#                                output = "latex", 
#                                coef_map = cm,
#                                gof_map = gm,
#                                add_rows = mean_row,
#                                escape = F,
#                                #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
#                                stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
#                                title = "Subjective Hispanic identity and Explicit Bias Among Second Generation Hispanic Immigrants: By Parental Type \\label{regtab-byparent-05}") %>%
#   kable_styling(bootstrap_options = c("hover", "condensed"), 
#                 latex_options = c("scale_down", "HOLD_position")
#   ) %>%
#   footnote(number = c("I include controls for sex, quartic age, parental education.",
#                       "Standard errors are clustered on the state level"),
#            escape = F, fixed_small_size = T,
#            footnote_as_chunk = F, title_format = c("italic"), threeparttable = T
#   ) |> 
#   add_header_above(c("Parents Type" = 1, "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1,
#                      "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1,
#                      "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1,
#                      "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1
#   )) |> 
#   add_header_above(c("Sample" = 1, "All Sample" = 4,
#                      "March Sup." = 4,
#                      "non-March Sup." = 4,
#                      "March Sup. and Control for Income" = 4
#   ))
# 
# 
# regression_tab2 %>%
#   #save_kable(file = "Objective_Subjective.html", self_contained = T)
#   save_kable(file.path(tables_wd,"tab32-Explicit_regression_tab_byparent_type.tex"))
# 
# regression_tab2 %>%
#   landscape() |> 
#   #save_kable(file = "Objective_Subjective.html", self_contained = T)
#   save_kable(file.path(thesis_tabs,"tab32-Explicit_regression_tab_byparent_type.tex"))
# 
# # presentation
# 
# reg1 <- list(
#   "\\specialcell{(1) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad + HH_0bj| region:year, 
#                                           data = CPS_IAT |> filter(SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(2) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad| region:year, 
#                                           data = CPS_IAT |> filter(HH_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(3) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                           data = CPS_IAT |> filter(HW_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip),
#   "\\specialcell{(4) \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
#                                           + MomGradCollege + DadGradCollege + 
#                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
#                                           data = CPS_IAT |> filter(WH_0bj == 1 & SecondGen == 1), weights = ~weight, vcov = ~statefip)
#   # "Hispanic" = feols(Hispanic ~ 1 + Explicit_value + Female
#   #                    + MomGradCollege + DadGradCollege +
#   #                      Age + Age_sq + Age_cube + Age_quad | region:year,
#   #                    data = CPS_IAT |> filter(WW_0bj == 1 & ThirdGen == 1), weights = ~weight, vcov = ~statefip)
# 
# )
# 
# gm <- tibble::tribble(
#   ~raw,        ~clean,          ~fmt,
#   "nobs",      "N",             0,
#   # "FE: region", "Region FE", 0,
#   # "FE: year", "Year FE", 0,
#   "FE: region:year", "Year $\\times$ Region FE", 0,
#   # "std.error.type", "Standard Errors", 0,
#   #"r.squared", "R squared", 3
# )
# cm <- c("value" = "Bias",
#         "Female" = "Female",
#         "MomGradCollege" = "College Graduate: Mother",
#         "DadGradCollege" = "College Graduate: Father",
#         "lnftotval_mom" = "Log Total Family Income"
#         #"age" = "Age",
#         #"HH" = "Both parents Hispanic",
#         # "FirstGen" = "First Gen",
#         # "SecondGen" = "Second Gen",
#         # "ThirdGen" = "Third Generation"
# ) 
# regression_tab <- modelsummary(reg1, fmt = f1,
#                                output = "latex",
#                                coef_map = cm,
#                                gof_map = gm,
#                                add_rows = mean_row[1:5],
#                                escape = F,
#                                #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
#                                stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)
#                                ) %>%
#   kable_styling(bootstrap_options = c("hover", "condensed"), 
#                 latex_options = c("HOLD_position"),
#                 full_width = F, font_size = 10
#   ) %>%
#   # footnote(number = c("I include controls for sex, quartic age, and parental education.",
#   #                     "Standard errors are clustered on the state level"),
#   #          escape = F,
#   #          footnote_as_chunk = F, title_format = c("italic")
#   # ) |>
#   add_header_above(c(" " = 1, "All" = 1, "HH" = 1, "HW" = 1, "WH" = 1#, "WW" = 1
#                      )) |>
#   row_spec(c(1,2), bold = T, color = "black", background = "yellow")
# 
# 
# 
# regression_tab %>%
#   save_kable(file.path(pres_tabs,"tab05-iat_regression_tab_byparent_type.tex"))
rm(CPS_IAT)
