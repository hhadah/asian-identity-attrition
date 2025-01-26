# This a script to
# run a regression
# with cps and 
# project implicit
# Asian IAT
# data

# Date: Sep 27th, 2022

# table with different FE

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index)

# fixed effects regression

reg1 <- list("\\specialcell{(1) \\\\ $A_i$}" = feols(AsianChild ~ value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj +
                                                       Age + Age_sq + Age_cube + Age_quad + FirstGen_Asian + SecondGen_Asian, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
             "\\specialcell{(2) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad   + FirstGen_Asian + SecondGen_Asian| year, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
              "\\specialcell{(3) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad   + FirstGen_Asian + SecondGen_Asian| statefip, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
              "\\specialcell{(4) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad   + FirstGen_Asian + SecondGen_Asian| statefip + year, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
             "\\specialcell{(5) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad  + FirstGen_Asian + SecondGen_Asian| region, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
             "\\specialcell{(6) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad + FirstGen_Asian + SecondGen_Asian| region + year, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
             "\\specialcell{(7) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad + FirstGen_Asian + SecondGen_Asian| region:year, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT),
             "\\specialcell{(8) \\\\ $A_i$}" = feols(AsianChild ~ 1 + value + Female 
                                                     + MomGradCollege + DadGradCollege + AA_0bj + 
                                                       Age + Age_sq + Age_cube + Age_quad + FirstGen_Asian + SecondGen_Asian| statefip + region:year, 
                                                     weights = ~weight, vcov = ~statefip,
                                                     data = CPS_IAT)
)

cm <- c("value" = "Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom" = "Log Total Family Income",
        #"age" = "Age",
        "AA_0bj" = "Both parents Asian",
        "FirstGen_Asian" = "First Gen",
        "SecondGen_Asian" = "Second Gen",
        "ThirdGen_Asian" = "Third Generation") 
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "FE: region", "Region FE", 0,
  "FE: year", "Year FE", 0,
  "FE: statefip", "State FE", 0,
  "FE: region:year", "Year-Region FE", 0,
  "std.error.type", "Standard Errors", 0,
  #"r.squared", "R squared", 3
)

# calculate means to add
# as a row
means <- CPS_IAT |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

# mean_row <-  data.frame(Coefficients = c('Mean', round(means, digits = 2), round(means, digits = 2), round(means, digits = 2), round(means, digits = 2), round(means, digits = 2)))

# colnames(mean_row)<-LETTERS[1:6]

# attr(mean_row, 'position') <- c(17)

modelsummary(reg1, fmt = 2,  
             output = "kableExtra", 
             coef_map = cm,
             gof_map = gm,
             escape = F,
            #  add_rows = mean_row,
             #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
             stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             title = "Subjective Asian Identity and Asian Implicit Bias (State) \\label{regtab-04}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("I include controls for sex, quartic age, and parental education.",
                      "Standard errors are clustered on the state level"),
           footnote_as_chunk = T, title_format = c("italic"),
           threeparttable = T
  )

regression_tab <- modelsummary(reg1, fmt = 2,  
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Subjective Asian Identity and Asian Bias \\label{regtab-all-fe}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{I include controls for sex, quartic age, and parental education.}",
                      "\\\\footnotesize{Standard errors are clustered on the state level.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T
  )


regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab33-asianiat_regression_tab.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab33-asianiat_regression_tab.tex"))
rm(CPS_IAT)
