# This a script to
# run a regression
# with ACS and
# project implicit
# data
# check if IAT is
# correlated with
# migration

# Date: Oct 26th, 2022


# open data
ACS_IAT <- read_csv(file.path(datasets,"ACS_IAT.csv")) |> 
  mutate(m = year - 1,
         bplregion = case_when(bplregion == "East North Central" ~ 1,
                               bplregion == "East South Central" ~ 2,
                               bplregion == "Middle Atlantic"    ~ 3,
                               bplregion == "Mountain"           ~ 4,
                               bplregion == "New England"        ~ 5,
                               bplregion == "Pacific"            ~ 6,
                               bplregion == "South Atlantic"     ~ 7,
                               bplregion == "West North Central" ~ 8,
                               bplregion == "West South Central" ~ 9
                               ))
ACS_IAT <- ACS_IAT |> 
  filter(SecondGen_Asian == 1 & AA_0bj == 1)
table(ACS_IAT$SecondGen_Asian, ACS_IAT$AA_0bj)
table(ACS_IAT$AsianChild)

ACS_IAT <- ACS_IAT |> 
  mutate(bias_diff = value - bplvalue)

# as a row
mean_row <- data.frame(x1 = character(),
                       x2 = numeric(),
                       x3 = numeric(),
                       x4 = numeric(),
                    stringsAsFactors = FALSE)
means1 <- ACS_IAT |>
  summarise(
    mean_bpl_mover = mean(bpl_mover, na.rm = T))
means2 <- ACS_IAT |>
  filter(bpl_mover == 1) |>
  summarise(
    mean_bias_diff = mean(bias_diff, na.rm = T))


mean_row[1,] <-  list('Mean', round(means1[[1]], digits = 2), round(means1[[1]], digits = 2), round(means2[[1]], digits = 2))

colnames(mean_row)<-LETTERS[1:4]

attr(mean_row, 'position') <- c(14)

# fixed effects regression
CrossTable(ACS_IAT$AA_0bj, ACS_IAT$AsianChild)

reg1 <- list(
             "\\specialcell{(1) \\\\ Migrated from \\\\ Birth Place}" = feols(bpl_mover ~ 1 + value + Female 
                                                                              + MomGradCollege + DadGradCollege+ frac_asian+
                                                                                Age + Age_sq + Age_cube + Age_quad| year:region, 
                                                                              weights = ~weight, vcov = ~statefip,
                                                                              data = ACS_IAT),
             "\\specialcell{(2) \\\\ Migrated from \\\\ Birth Place}" = feols(bpl_mover ~ 1 + bplvalue + Female 
                                                                              + MomGradCollege + DadGradCollege+ frac_asian+
                                                                                Age + Age_sq + Age_cube + Age_quad| birthyr:bplregion, 
                                                                              weights = ~weight, vcov = ~statefip,
                                                                              data = ACS_IAT),
             "\\specialcell{(3) \\\\ $Bias_{ist} - Bias_{ilb}$}" = feols(bias_diff ~ AsianChild + Female 
                                                                              + MomGradCollege + DadGradCollege+ frac_asian+
                                                                                Age + Age_sq + Age_cube + Age_quad, 
                                                                              weights = ~weight, vcov = ~statefip,
                                                                              data = ACS_IAT |> filter(bpl_mover == 1))#,
             # "\\specialcell{(4) \\\\ $Bias_{ist} - Bias_{ilb}$}" = feols(bias_diff ~ Female 
             #                                                             + MomGradCollege + DadGradCollege+ frac_asian+
             #                                                               Age + Age_sq + Age_cube + Age_quad, 
             #                                                             weights = ~weight, vcov = ~statefip,
             #                                                             data = ACS_IAT |> filter(bpl_mover == 1))
)


cm <- c(
        # "(Intercept)"    = "Constant",
        "value"          = "$Bias_{st}$",
        "migvalue"       = "$Bias_{sm}$",
        "bplvalue"       = "$Bias_{lb}$",
        "AsianChild"       = "Asian",
        "bpl_mover"      = "Migrated from State of Birth",
        "Female"         = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom"  = "Log Total Family Income",
        #"age" = "Age",
        "HH_0bj" = "Both parents Asian",
        "FirstGen" = "First Gen") 

f1 <- function(x) format(round(x, 2), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: statefip", clean = "State FE", fmt = 0),
  list(raw = "FE: year", clean = "Year \\textit{(t)} FE", fmt = 0),
  list(raw = "FE: year:region", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "FE: birthyr:bplregion", clean = "Birthyear $\\times$ Birth Region FE", fmt = 0),
  list(raw = "FE: birthyr", clean = "Birthyear FE", fmt = 0),
  list(raw = "FE: m:region", clean = "$(m = t-1) \\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

options(modelsummary_format_numeric_latex = "plain")

modelsummary(reg1, fmt = f1,  
             output = "kableExtra", 
             coef_map = cm,
             gof_map = gm,
             #add_rows = mean_row,
             escape = F,
             #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
             stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             title = "Effect of Bias on Migration\\label{regtab-mig-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("I include controls for sex, quartic age, and parental education.",
                      "Standard errors are clustered on the state level"),
           footnote_as_chunk = T, title_format = c("italic"),
           threeparttable = T
  )#|> 
  #row_spec(c(1,2), bold = T, color = "white", background = "yellow")

regression_tab <- modelsummary(reg1, fmt = f1,  
                               output = "latex", 
                               coef_map = cm,
                               add_rows = mean_row,
                               gof_map = gm,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Relationship Between Bias and Migration \\label{regtab-mig-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{Each column is an estimation of equations (\\\\ref{eq:migration-3}) in column (1), 
                      (\\\\ref{eq:migration-4}) in column (2), and
                      (\\\\ref{eq:migration-5}) in column (3).}",
                      "\\\\footnotesize{Column (1) is a regression where the left hand side variable is 
                      a dummy variable that is equal to one if a person migrated from the state
                      were born in and the right hand side variable is bias the year of survey.
                      Column (2) is a regression where the left hand side variable is 
                      a dummy variable that is equal to one if a person migrated from the state
                      were born in and the right hand side variable is bias the year of birth in the state of birth.
                      Column (3) is a regression where the left hand side variable is 
                      the difference between state-level bias during the year of the survey in the current state the 
                      respondent is living in, and state-level bias during the year of birth in the state of birth 
                      and the right hand side variable is self-reported Asian identity. This regression captures
                      the selection of those that self-reported Asian identity into states with different levels of bias.
                      I include controls for sex, quartic age, parental education, fraction of Asians in a state, and region × year fixed effects.
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{The samples include children ages 17 and below who live in intact families. 
                      Native-born second-generation Asian immigrant children with both
                      parents born in a Asian country. The sample in the column (3) regression is further restricted to only those that migrated from their birth state.}",
                      "\\\\footnotesize{Data source is the 2004-2021 Census Data.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab46-iat-migration-tab.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab46-iat-migration-tab.tex"))
