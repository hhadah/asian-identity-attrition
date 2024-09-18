# open data
ACS_IAT <- read_csv(file.path(datasets,"ACS_IAT.csv")) |> 
  mutate(m = year - 1
  )
ACS_IAT <- ACS_IAT |>
  filter(SecondGen_Asian == 1 & AA_0bj == 1)

# Function to map state FIPS codes to Census divisions
map_to_census_division <- function(fips_code) {
  case_when(
    fips_code %in% c(9, 23, 25, 33, 44, 50) ~ 1,  # New England
    fips_code %in% c(34, 36, 42) ~ 2,            # Middle Atlantic
    fips_code %in% c(17, 18, 26, 39, 55) ~ 3,    # East North Central
    fips_code %in% c(19, 20, 27, 29, 31, 38, 46) ~ 4,  # West North Central
    fips_code %in% c(10, 11, 12, 13, 24, 37, 45, 51, 54) ~ 5,  # South Atlantic
    fips_code %in% c(1, 21, 28, 47) ~ 6,         # East South Central
    fips_code %in% c(5, 22, 40, 48) ~ 7,         # West South Central
    fips_code %in% c(4, 8, 16, 30, 32, 35, 49, 56) ~ 8,  # Mountain
    fips_code %in% c(2, 6, 15, 41, 53) ~ 9,      # Pacific
    TRUE ~ NA_real_  # Handle other cases
  )
}

# Apply the function to create the `bplregion` variable
ACS_IAT <- ACS_IAT %>%
  mutate(bplregion = map_to_census_division(bpl))

# Select relevant columns for Asian variables
relevant_columns <- c("Female", "MomGradCollege", "DadGradCollege", "frac_asian", "Age", "Age_sq", "Age_cube", "Age_quad", "AA_0bj", "Type_Asian")
# Remove rows with any missing values
ACS_IAT <- ACS_IAT[complete.cases(ACS_IAT[, relevant_columns]), ]

# Estimate Your Primary Model with feols
lw_model_1 <- feols(Asian ~ mean_skin + Mean_Index + + hate_crimes_per_100000 + Female 
                    + MomGradCollege + DadGradCollege + frac_asian
                    + Age + Age_sq + Age_cube + Age_quad + AA_0bj 
                    | region:year, 
                    data = ACS_IAT, weights = ~weight, vcov = ~statefip)

lw_model_2 <- feols(Asian ~ bplmean_skin + bplMean_Index + Female 
                    + MomGradCollege + DadGradCollege + frac_asian
                    + Age + Age_sq + Age_cube + Age_quad + HH_0bj 
                    | region:year, 
                    data = ACS_IAT, weights = ~weight, vcov = ~statefip)

# Residualize Proxies and Dependent Variable
control_vars <- c("Female", "MomGradCollege", "DadGradCollege", "frac_asian", "Age", "Age_sq", "Age_cube", "Age_quad", "HH_0bj")
X <- model.matrix(~ Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + HH_0bj, data = ACS_IAT)

residuals <- list()
for (var in c("Asian", "mean_skin", "Mean_Index", "bplmean_skin", "bplMean_Index")) {
  residuals[[var]] <- resid(lm(as.formula(paste(var, "~", paste(control_vars, collapse = "+"))), data = ACS_IAT))
}

names(residuals) <- c("r_Asian", "r_mean_skin", "r_Mean_Index", "r_bplmean_skin", "r_bplMean_Index")

# Calculate Covariances
covs_1 <- sapply(c("r_mean_skin", "r_Mean_Index"), function(var) {
  cov(residuals$r_Asian, residuals[[var]])
})
names(covs_1) <- paste0("rho_", sub("r_", "", names(covs_1)))

covs_2 <- sapply(c("r_bplmean_skin", "r_bplMean_Index"), function(var) {
  cov(residuals$r_Asian, residuals[[var]])
})
names(covs_2) <- paste0("rho_", sub("r_", "", names(covs_2)))

# Construct LW Equations
lw_part <- function(index, proxy) {
  paste0(proxy, "*rho_", proxy, "/rho_", index)
}

lw_string <- function(index, proxies) {
  paste(lapply(proxies, function(proxy) lw_part(index, proxy)), collapse = " + ")
}

# Construct LW Equations for both sets of proxies
lw_strings_1 <- lapply(c("mean_skin", "Mean_Index"), function(var) {
  lw_string("mean_skin", c("mean_skin", "Mean_Index"))
})

lw_strings_2 <- lapply(c("bplmean_skin", "bplMean_Index"), function(var) {
  lw_string("bplmean_skin", c("bplmean_skin", "bplMean_Index"))
})

# Calculate LW Coefficients Using Delta Method for both sets of proxies
lw_results_1 <- lapply(lw_strings_1, function(eq) {
  deltaMethod(lw_model_1, eq, vcov. = vcov(lw_model_1), constants = covs_1)
})

lw_results_2 <- lapply(lw_strings_2, function(eq) {
  deltaMethod(lw_model_2, eq, vcov. = vcov(lw_model_2), constants = covs_2)
})

coef_estimates_1 <- sapply(lw_results_1, function(res) res[["Estimate"]])
coef_estimates_2 <- sapply(lw_results_2, function(res) res[["Estimate"]])

se_estimates_1 <- sapply(lw_results_1, function(res) res[["SE"]])
se_estimates_2 <- sapply(lw_results_2, function(res) res[["SE"]])

lw_summary_1 <- list(coef = coef_estimates_1, se = se_estimates_1)
lw_summary_2 <- list(coef = coef_estimates_2, se = se_estimates_2)

# Print LW Summary
print(lw_summary_1)
print(lw_summary_2)

# Extract Coefficients
coef_mean_skin <- coef(lw_model_1)["mean_skin"]
coef_Mean_Index <- coef(lw_model_1)["Mean_Index"]
coef_bplmean_skin <- coef(lw_model_2)["bplmean_skin"]
coef_bplMean_Index <- coef(lw_model_2)["bplMean_Index"]

# Calculate LW Indices
# LW Index 1
lw_coef_mean_skin <- coef_estimates_1[1]
lw_coef_Mean_Index <- coef_estimates_1[2]

ACS_IAT$lw_index_1 <- with(ACS_IAT, (mean_skin * lw_coef_mean_skin + Mean_Index * lw_coef_Mean_Index) / lw_coef_mean_skin)

# LW Index 2
lw_coef_bplmean_skin <- coef_estimates_2[1]
lw_coef_bplMean_Index <- coef_estimates_2[2]

ACS_IAT$lw_index_2 <- with(ACS_IAT, (bplmean_skin * lw_coef_bplmean_skin + bplMean_Index * lw_coef_bplMean_Index) / lw_coef_bplmean_skin)

# Inspect Calculated LW Indices
print(head(ACS_IAT$lw_index_1))
print(head(ACS_IAT$lw_index_2))

ACS_IAT <- ACS_IAT |> 
  mutate(bias_diff = lw_index_1 - lw_index_2)

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
             "\\specialcell{(1) \\\\ Migrated from \\\\ Birth Place}" = feols(bpl_mover ~ 1 + lw_index_1 + Female 
                                                                              + MomGradCollege + DadGradCollege+ frac_asian+
                                                                                Age + Age_sq + Age_cube + Age_quad| year:region, 
                                                                              weights = ~weight, vcov = ~statefip,
                                                                              data = ACS_IAT),
             "\\specialcell{(2) \\\\ Migrated from \\\\ Birth Place}" = feols(bpl_mover ~ 1 + lw_index_2 + Female 
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
        "lw_index_1"          = "$Bias_{st}$",
        "migvalue"            = "$Bias_{sm}$",
        "lw_index_2"          = "$Bias_{lb}$",
        "AsianChild"          = "Asian",
        "bpl_mover"           = "Migrated from State of Birth",
        "Female"              = "Female",
        "MomGradCollege"      = "College Graduate: Mother",
        "DadGradCollege"      = "College Graduate: Father",
        "lnftotval_mom"       = "Log Total Family Income",
        #"age"                = "Age",
        "HH_0bj"              = "Both parents Asian",
        "FirstGen"            = "First Gen") 

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
