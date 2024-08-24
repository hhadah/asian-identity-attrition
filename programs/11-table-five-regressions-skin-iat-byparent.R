# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index)

# fixed effects regression

reg1 <- list(
  "\\specialcell{(1) \\\\ $A^2_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad + factor(ParentType2) | region:year, 
                     data = CPS_IAT |> filter(SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $A^2_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad| region:year, 
                     data = CPS_IAT |> filter(AA_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $A^2_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $A^2_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip)
)


cm <- c("value" = "Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom" = "Log Total Family Income"
        #"age" = "Age",
        #"HH" = "Both parents Asian",
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
  filter(SecondGen_Asian == 1) |> 
  group_by(ParentType2) |> 
  summarise(Asian = mean(Asian, na.rm = T))

means_all <- CPS_IAT |> 
  filter(SecondGen_Asian == 1) |> 
  summarise(Asian = mean(Asian, na.rm = T))

means_gen_asec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & asecflag == 1) |> 
  group_by(ParentType2) |> 
  summarise(Asian = mean(Asian, na.rm = T))

means_all_asec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & asecflag == 1) |> 
  summarise(Asian = mean(Asian, na.rm = T))

means_gen_notasec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))) |> 
  group_by(ParentType2) |> 
  summarise(Asian = mean(Asian, na.rm = T))

means_all_notasec <- CPS_IAT |> 
  filter(SecondGen_Asian == 1 & (asecflag != 1 | is.na(asecflag))) |> 
  summarise(Asian = mean(Asian, na.rm = T))

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
                               title = "Relationship Between Bias and Self-Reported Asian identity Among Second-Generation Asian Immigrants: By Parental Type \\label{regtab-byparent-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{Each column is an estimation of a heterogeneous effect of regression (\\\\ref{eq:identity_reg_bias}) by 
                      type of parents with region × year fixed effects. 
                      I include controls for sex, quartic age, fraction of Asians in a state, and parental education.
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{The samples include second-generation Asian children ages 17 and below who live in intact families. 
                      Native-born second-generation Asian 
                      immigrant children with at least one parent born in a Asian 
                      country.}",
                      "\\\\footnotesize{Column (1) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on all second-generation immigrants, 
                                        column (2) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on second-generation immigrants that who has a father and mother that were born in a Asian country (HH),
                                        column (3) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on second-generation immigrants that who has a father that was born in a Asian country and a native-born mother (HW), and
                                        column (4) includes the results to regression (\\\\ref{eq:identity_reg_bias}) on second-generation immigrants that who has a native-born father and a mother that was born in a Asian country (WH).}",
                      "\\\\footnotesize{Data source is the 2004-2021 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T
  ) |> 
  add_header_above(c("Parents Type" = 1, "All" = 1, "Both Parents \n from Spanish \n Speaking Country \n (HH)" = 1, "Father \n from Spanish \n Speaking Country \n (HW)" = 1, "Mother  \n from Spanish \n Speaking Country \n (WH)" = 1
                     )) 

regression_tab %>%
  save_kable(file.path(tables_wd,"tab05-iat_regression_tab_byparent_type.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab05-iat_regression_tab_byparent_type.tex"))

rm(CPS_IAT)
