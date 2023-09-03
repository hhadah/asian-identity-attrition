# This a script to
# run a regression
# with cps and 
# project implicit
# Asian IAT
# data

# Date: Sep 27th, 2022

# table with different FE

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))


# By generation
reg1 <- list(
  "\\specialcell{(1) \\\\ $H_i$}" = feols(AsianChild ~ 1 + value + Female 
                                          + MomGradCollege + DadGradCollege + 
                                            Age + Age_sq + Age_cube + Age_quad  + AA_0bj  + FirstGen_Asian + SecondGen_Asian| region:year, 
                                          data = CPS_IAT, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $H^1_i$}" = feols(AsianChild ~ 1 + value + Female 
                                            + MomGradCollege + DadGradCollege + 
                                              Age + Age_sq + Age_cube + Age_quad| region:year, 
                                            data = CPS_IAT |> filter(FirstGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $H^2_i$}" = feols(AsianChild ~ 1 + value + Female 
                                            + MomGradCollege + DadGradCollege + 
                                              Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                                            data = CPS_IAT |> filter(SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $H^3_i$}" = feols(AsianChild ~ 1 + value + Female 
                                            + MomGradCollege + DadGradCollege + 
                                              Age + Age_sq + Age_cube + Age_quad + Grandparent_Type_Asian| region:year, 
                                            data = CPS_IAT |> filter(ThirdGen_Asian == 1), weights = ~weight, vcov = ~statefip)
  
)

# calculate means to add
# as a row
means_gen <- CPS_IAT |> 
  group_by(Type_Asian) |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

means_all <- CPS_IAT |> 
  summarise(AsianChild = mean(AsianChild, na.rm = T))

mean_row <-  data.frame(Coefficients = c('Mean', round(means_all[1], digits = 2), round(means_gen[1,2], digits = 2), round(means_gen[2,2], digits = 2), round(means_gen[3,2], digits = 2)))

colnames(mean_row)<-LETTERS[1:5]

attr(mean_row, 'position') <- c(10)

cm <- c("value" = "Implicit Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father"#,
        #"lnftotval_mom" = "Log Total Family Income"#,
        #"age" = "Age",
        #"HH" = "Both parents AsianChild",
        # "FirstGen_Asian" = "First Gen",
        # "SecondGen_Asian" = "Second Gen",
        # "ThirdGen_Asian" = "Third Generation"
) 
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "FE: region", "Region FE", 0,
  "FE: year", "Year FE", 0,
  "FE: region:year", "Year-Region FE", 0,
  "std.error.Type_Asian", "Standard Errors", 0,
  #"r.squared", "R squared", 3
)
regression_tab <- modelsummary(reg1, fmt = 2,  
                               output = "latex", 
                               coef_map = cm,
                               add_rows = mean_row,
                               gof_map = gm,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Subjective Asian Child identity and Asian IAT: By Generation (State) \\label{regtab-bygen-04}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("I include controls for sex, quartic age, parental education.",
                      "Standard errors are clustered on the state level"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F,
  )


regression_tab %>%
  save_kable(file.path(tables_wd,"tab34-regression_tab_bygen_skin.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab34-regression_tab_bygen_skin.tex"))



rm(CPS_IAT)
