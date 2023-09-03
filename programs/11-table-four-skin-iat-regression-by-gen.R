# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# table with different FE

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT.csv"))


feols(Hispanic ~ 1 + value + Female 
                                + MomGradCollege + DadGradCollege + frac_hispanic +
                                Age + Age_sq + Age_cube + Age_quad  + factor(ParentType2) + factor(Grandparent_Type)
                                + FirstGen + SecondGen| serial, 
                                data = CPS_IAT, weights = ~weight, vcov = ~statefip)                         
# By generation
reg1 <- list(
  "\\specialcell{(1) \\\\ All Gens \\\\ $H_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                                + MomGradCollege + DadGradCollege + frac_hispanic +
                                Age + Age_sq + Age_cube + Age_quad  + factor(ParentType2) + factor(Grandparent_Type)
                                + FirstGen + SecondGen| region:year, 
                                data = CPS_IAT, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\  First Gen \\\\ $H^1_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                               + MomGradCollege + DadGradCollege + frac_hispanic +
                               Age + Age_sq + Age_cube + Age_quad| region:year, 
                               data = CPS_IAT |> filter(FirstGen == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\  Second Gen \\\\ $H^2_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                               + MomGradCollege + DadGradCollege + frac_hispanic +
                               Age + Age_sq + Age_cube + Age_quad + factor(ParentType2)| region:year, 
                               data = CPS_IAT |> filter(SecondGen == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\  Third Gen \\\\ $H^3_{ist}$}" = feols(Hispanic ~ 1 + value + Female 
                               + MomGradCollege + DadGradCollege + frac_hispanic +
                               Age + Age_sq + Age_cube + Age_quad + factor(Grandparent_Type)| region:year, 
                               data = CPS_IAT |> filter(ThirdGen == 1), weights = ~weight, vcov = ~statefip)
  
)

# calculate means to add
# as a row
means_gen <- CPS_IAT |> 
  group_by(Type) |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

means_all <- CPS_IAT |> 
  summarise(hispanic = mean(Hispanic, na.rm = T))

mean_row <-  data.frame(Coefficients = c('Mean', round(means_all[1], digits = 2), round(means_gen[1,2], digits = 2), round(means_gen[2,2], digits = 2), round(means_gen[3,2], digits = 2)))

colnames(mean_row)<-LETTERS[1:5]

attr(mean_row, 'position') <- c(10)

cm <- c("value" = "Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father"#,
        #"frac_hispanic"  = "Fraction Hispanic"
        #"lnftotval_mom" = "Log Total Family Income"#,
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

modelsummary(reg1, fmt = f1,  
             coef_map = cm,
             add_rows = mean_row,
             gof_map = gm,
             escape = F,
             #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
             stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             title = "Self-Reported Hispanic Identity and Bias: By Generation \\label{regtab-bygen-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{I include controls for sex, quartic age, parental education.}",
                      "\\\\footnotesize{Standard errors are clustered on the state level.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  )

regression_tab <- modelsummary(reg1, fmt = f1,  
                               output = "latex", 
                               coef_map = cm,
                               add_rows = mean_row,
                               gof_map = gm,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Relationship Between Bias and Self-Reported Hispanic Identity: By Generation \\label{regtab-bygen-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{Each column is an estimation of a heterogeneous effect of regression (\\\\ref{eq:identity_reg_bias}) by 
                      generation with region × year fixed effects. 
                      I include controls for sex, quartic age, fraction of Hispanics in a state, and parental education.
                      I also added parents' (HH, HW, and WH) and grandparents' (HHHH, HHHW, HHWH, etc.) type dummy variables to the regression
                      on second and third generation immigrants, where H is objectively Hispanic (born in a Spanish-Speaking Country) and W is objectively White (native-born). 
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{The samples include children ages 17 and below who live in intact families. 
                      First-generation Hispanic immigrant children that were born in a 
                      Spanish-speaking county. Native-born second-generation Hispanic 
                      immigrant children with at least one parent born in a Spanish-speaking 
                      country. Finally, native-born third-generation Hispanic immigrant children 
                      with native-born parents and at least one grandparent born in a Spanish-
                      speaking country.}",
                      "\\\\footnotesize{Data source is the 2004-2021 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T
  )


regression_tab %>%
  save_kable(file.path(tables_wd,"tab04-regression_tab_bygen_skin.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab04-regression_tab_bygen_skin.tex"))
regression_tab %>%
  save_kable(file.path(Identity_paper_tab,"tab51-regression_tab_bygen_skin.tex"))

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  # "FE: region", "Region FE", 0,
  # "FE: year", "Year FE", 0,
  "FE: region:year", "Year\\timesRegion FE", 0,
  # "std.error.type", "Standard Errors", 0,
  #"r.squared", "R squared", 3
)
regression_tab <- modelsummary(reg1, fmt = f1,  
                               output = "latex", 
                               coef_map = cm,
                               add_rows = mean_row,
                               escape = F,
                               gof_map = gm,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("hold_position"),
                full_width = F, font_size = 10
  ) |>
  row_spec(c(1,2), bold = T, color = "black", background = "yellow")


regression_tab %>%
  save_kable(file.path(pres_tabs,"tab04-regression_tab_bygen_skin.tex"))

rm(CPS_IAT)
