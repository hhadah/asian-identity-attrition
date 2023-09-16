# This a script to
# run a regression
# with cps and 
# project implicit
# Asian IAT
# data

# Date: Sep 27th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))


# fixed effects regression

reg1 <- list(
  "\\specialcell{(1) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                                          data = CPS_IAT |> filter(SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad| region:year, 
                                          data = CPS_IAT |> filter(AA_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
                                          data = CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
                                          data = CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(5) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                                          data = CPS_IAT |> filter(SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(6) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad| region:year, 
                                          data = CPS_IAT |> filter(AA_0bj == 1 & SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(7) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
                                          data = CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(8) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad | region:year, 
                                          data = CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(9) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                                          data = CPS_IAT |> filter(SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(10) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + 
                                             Age + Age_sq + Age_cube + Age_quad| region:year, 
                                           data = CPS_IAT |> filter(AA_0bj == 1 & SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(11) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + 
                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
                                           data = CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(12) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + 
                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
                                           data = CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(13) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + lnftotval_mom +
                                             Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                                           data = CPS_IAT |> filter(SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(14) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + lnftotval_mom +
                                             Age + Age_sq + Age_cube + Age_quad| region:year, 
                                           data = CPS_IAT |> filter(AA_0bj == 1 & SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(15) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + lnftotval_mom +
                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
                                           data = CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(16) \\\\ $H^2$}" = feols(AsianChild ~ 1 + value + Female 
                                           + MomGradCollege + DadGradCollege + lnftotval_mom +
                                             Age + Age_sq + Age_cube + Age_quad | region:year, 
                                           data = CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1 & asecflag == 1), weights = ~weight, vcov = ~statefip)
  # "AsianChild" = feols(AsianChild ~ 1 + value + Female 
  #                    + MomGradCollege + DadGradCollege + 
  #                      Age + Age_sq + Age_cube + Age_quad | region:year, 
  #                    data = CPS_IAT |> filter(WW_0bj == 1 & ThirdGen == 1), weights = ~weight, vcov = ~statefip)
  
)


cm <- c("value" = "Implicit Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom" = "Log Total Family Income"
        #"age" = "Age",
        #"AA" = "Both parents AsianChild",
        # "FirstGen" = "First Gen",
        # "SecondGen_Asian" = "Second Gen",
        # "ThirdGen" = "Third Generation"
) 
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "FE: region", "Region FE", 0,
  "FE: year", "Year FE", 0,
  "FE: region:year", "Year-Region FE", 0,
  "std.error.type", "Standard Errors", 0,
  #"r.squared", "R squared", 3
)

# calculate means to add
# as a row
means_gen <- CPS_IAT |> 
  filter(SecondGen_Asian == 1) |> 
  group_by(ParentType2) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

means_all <- CPS_IAT |> 
  filter(SecondGen_Asian == 1) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

means_gen_asec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & asecflag == 1) |> 
  group_by(ParentType2) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

means_all_asec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & asecflag == 1) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

means_gen_notasec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))) |> 
  group_by(ParentType2) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

means_all_notasec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

mean_row <-  data.frame(Coefficients = c('Mean', round(means_all[1], digits = 2),      round(means_gen[1,2], digits = 2),      round(means_gen[2,2], digits = 2),      round(means_gen[3,2], digits = 2),
                                         round(means_all_asec[1], digits = 2), round(means_gen_asec[1,2], digits = 2), round(means_gen_asec[2,2], digits = 2), round(means_gen_asec[3,2], digits = 2),
                                         round(means_all_notasec[1], digits = 2), round(means_gen_notasec[1,2], digits = 2), round(means_gen_notasec[2,2], digits = 2), round(means_gen_notasec[3,2], digits = 2),
                                         round(means_all_asec[1], digits = 2), round(means_gen_asec[1,2], digits = 2), round(means_gen_asec[2,2], digits = 2), round(means_gen_asec[3,2], digits = 2)
))

colnames(mean_row)<-LETTERS[1:17]

attr(mean_row, 'position') <- c(11)

regression_tab <- modelsummary(reg1, fmt = 2,  
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               add_rows = mean_row,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Subjective Asian Identity and Implicit Bias Among Second Generation Asian Immigrants: By Parental Type (State)\\label{regtab-byparent-06}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("I include controls for sex, quartic age, parental education.",
                      "Standard errors are clustered on the state level"),
           escape = F,
           footnote_as_chunk = F, title_format = c("italic")
  ) |> 
  add_header_above(c("Parents Type" = 1, "All" = 1, "AA" = 1, "AW" = 1, "WA" = 1,
                     "All" = 1, "AA" = 1, "AW" = 1, "WA" = 1,
                     "All" = 1, "AA" = 1, "AW" = 1, "WA" = 1,
                     "All" = 1, "AA" = 1, "AW" = 1, "WA" = 1
  )) |> 
  add_header_above(c("Sample" = 1, "All Sample" = 4,
                     "March Sup." = 4,
                     "non-March Sup." = 4,
                     "March Sup. and Control for Income" = 4
  ))


regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab35-asianiat_regression_tab_byparent_type.tex"))

regression_tab %>%
  landscape() |> 
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab35-asianiat_regression_tab_byparent_type.tex"))

rm(CPS_IAT)
