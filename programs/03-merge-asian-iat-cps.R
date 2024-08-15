# This a script to 
# merge the asian IAT
# and the Current
# Population Survey (CPS)

# Date: Sep 21st, 2022

### prejeduice index

ANES_GSS_Skin <- read_csv(file.path(datasets,"ANES_GSS_Skin.csv")) |> 
  rename(mean_skin = value,
         anes_year = year
         )

### Open CPS data
### of 17 year olds
### living with their
### parents

CPS <- fread(CPS_asian)
CPS <- as.data.frame(CPS)
CPS <- CPS[, !duplicated(as.list(CPS))]

CPS <- CPS |> 
  filter(age<18) |>
  filter(Type_Asian == "First Generation Asian"  | 
         Type_Asian == "Second Generation Asian" | 
         Type_Asian == "Third Generation Asian") |> 
  mutate(Proxy = case_when(hhrespln ==lineno ~ "Self",
                           hhrespln ==lineno_mom ~ "Mother",
                           hhrespln ==lineno_mom2 ~ "Mother",
                           hhrespln ==lineno_pop  ~ "Father",
                           hhrespln ==lineno_pop2 ~ "Father",
                           TRUE ~ "Other"),
        Join_year = case_when(year == 1994 | year == 1995 ~ 1994,
                              year == 1996 | year == 1997 ~ 1996,
                              year == 1998 | year == 1999 ~ 1998,
                              year == 2000 | year == 2001 ~ 2000,
                              year == 2002 | year == 2003 ~ 2002,
                              year == 2004 | year == 2005 ~ 2004,
                              year == 2006 | year == 2007 ~ 2006,
                              year == 2008 | year == 2009 ~ 2008,
                              year == 2010 | year == 2011 ~ 2010,
                              year == 2012 | year == 2013 ~ 2012,
                              year == 2014 | year == 2015 ~ 2014,
                              year == 2016 | year == 2017 ~ 2016,
                              year == 2018 | year == 2019 ~ 2018,
                              year == 2020 | year == 2021 ~ 2020,
                              year == 2022 | year == 2023 ~ 2022,
                              year == 2024 | year == 2025 ~ 2024
                              ))

# merge wit CPS data
CPS_IAT <- left_join(CPS,
                     ANES_GSS_Skin,
                     na_matches = "never",
                     by = c("statefip", "Join_year"#, 
                            #"month"
                            )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))


CPS_IAT <- CPS_IAT |>
  filter(Type_Asian != "") |> 
  filter(!is.na(Type_Asian)) |>
  mutate(ftotval_mom = ifelse(ftotval_mom <= 1, 1, ftotval_mom),
         lnftotval_mom = log(ftotval_mom),
         Age = age,
         Age_sq = age^2,
         Age_cube = age^3,
         Age_quad = age^4,
         AA = ifelse(Asian_Dad == 1 & Asian_Mom == 1, 1, 0),
         AW = ifelse(Asian_Dad == 1 & Asian_Mom == 0, 1, 0),
         WA = ifelse(Asian_Dad == 0 & Asian_Mom == 1, 1, 0),
         WW = ifelse(Asian_Dad == 0 & Asian_Mom == 0, 1, 0),
         AA_0bj = ifelse((AsianPOB_Father == 1 & AsianPOB_Mother == 1), 1, 0),
         AW_0bj = ifelse((AsianPOB_Father == 1 & AsianPOB_Mother == 0), 1, 0),
         WA_0bj = ifelse((AsianPOB_Father == 0 & AsianPOB_Mother == 1), 1, 0),
         WW_0bj = ifelse((AsianPOB_Father == 0 & AsianPOB_Mother == 0), 1, 0),
         ParentType2 = case_when(AA_0bj == 1 ~ "Asian-Asian",
                                 AW_0bj == 1 ~ "Asian-White",
                                 WA_0bj == 1 ~ "White-Asian",
                                 WW_0bj == 1 ~ "White-White"),
         ParentType2 = as.factor(ParentType2),
         
         AA_0bj_3 = ifelse((AsianPOB_PatGrandMother == 1 | AsianPOB_PatGrandFather == 1) 
                           & (AsianPOB_MatGrandMother == 1 | AsianPOB_MatGrandFather == 1), 1, 0),
         AW_0bj_3 = ifelse((AsianPOB_PatGrandMother == 1 | AsianPOB_PatGrandFather == 1) 
                           & (AsianPOB_MatGrandMother == 0 & AsianPOB_MatGrandFather == 0), 1, 0),
         WA_0bj_3 = ifelse((AsianPOB_PatGrandMother == 0 & AsianPOB_PatGrandFather == 0) 
                           & (AsianPOB_MatGrandMother == 1 | AsianPOB_MatGrandFather == 1), 1, 0),
         WW_0bj_3 = ifelse((AsianPOB_PatGrandMother == 0 & AsianPOB_PatGrandFather == 0) 
                           & (AsianPOB_MatGrandMother == 0 & AsianPOB_MatGrandFather == 0), 1, 0),
         
         Grandparent_Type = case_when((AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 0) ~ 'WWWW',
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 1) ~ "WWWA",
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 0) ~ 'WWAW',
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 1) ~ 'WWAA',
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 0) ~ 'WAWW',
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 1) ~ 'WAWA',
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 0) ~ 'WAAW',
                                      (AsianPOB_PatGrandFather == 0 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 1) ~ 'WAAA',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 0) ~ 'AWWW',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 1) ~ 'AWWA',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 0) ~ 'AWAW',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 0) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 1) ~ 'AWAA',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 0) ~ 'AAWW',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 0 & AsianPOB_MatGrandMother == 1) ~ 'AAWA',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 0) ~ 'AAAW',
                                      (AsianPOB_PatGrandFather == 1 & AsianPOB_PatGrandMother == 1) 
                                      & (AsianPOB_MatGrandFather == 1 & AsianPOB_MatGrandMother == 1) ~ 'AAAA'
         ),
         Grandparent_Type = as.factor(Grandparent_Type),
         weight = case_when(!is.na(hwtfinl) ~ hwtfinl,
                            !is.na(asecfwt) ~ asecfwt,
                            !is.na(asecwt04) ~ asecwt04))
# Open fraction Asian data

CPS_frac <- fread(CPS_asian_mean)
CPS_frac <- as.data.frame(CPS_frac)

CPS_IAT <- left_join(CPS_IAT,
                     CPS_frac,
                     na_matches = "never",
                     by = c("statefip", "year"#, 
                            #"month"
                     )) |> 
  rename(frac_asian = MeanAsian)

#---------------------------
# Apply the LW method
#---------------------------

# Prepare Your Data and Model
# Ensure your data (CPS_IAT) is loaded and contains all the necessary variables.

# Ensure Data Completeness
# Select relevant columns
relevant_columns <- c("Asian", "mean_skin", "Mean_Index", "hate_crimes_per_100000","Female", 
                      "MomGradCollege", "DadGradCollege", "frac_asian",
                      "Age", "Age_sq", "Age_cube", "Age_quad", "AA_0bj",
                      "Type_Asian")

# Remove rows with any missing values
CPS_IAT <- CPS_IAT[complete.cases(CPS_IAT[, relevant_columns]), ]

# Estimate Your Primary Model with feols
lw_model <- feols(Asian ~ mean_skin + Mean_Index + hate_crimes_per_100000 + Female 
                  + MomGradCollege + DadGradCollege + frac_asian
                  + Age + Age_sq + Age_cube + Age_quad + AA_0bj 
                  + Type_Asian | region:year, 
                  data = CPS_IAT, weights = ~weight, vcov = ~statefip)

# Residualize Proxies and Dependent Variable
control_vars <- c("Female", "MomGradCollege", "DadGradCollege", "frac_asian", "Age", "Age_sq", "Age_cube", "Age_quad", "AA_0bj", "Type_Asian")
X <- model.matrix(~ Female + MomGradCollege + DadGradCollege + frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + Type_Asian, data = CPS_IAT)

# Include `hate_crimes_per_100000` in the residuals calculation
residuals <- list()
for (var in c("Asian", "mean_skin", "Mean_Index", "hate_crimes_per_100000")) {
  residuals[[var]] <- resid(lm(as.formula(paste(var, "~", paste(control_vars, collapse = "+"))), data = CPS_IAT))
}

# Assign names to the residuals
names(residuals) <- c("r_Asian", "r_mean_skin", "r_Mean_Index", "r_hate_crimes_per_100000")

# Calculate Covariances
covs <- sapply(c("r_mean_skin", "r_Mean_Index", "r_hate_crimes_per_100000"), function(var) {
  cov(residuals$r_Asian, residuals[[var]])
})

# Assign correct names to the covariance vector
names(covs) <- c("rho_mean_skin", "rho_Mean_Index", "rho_hate_crimes_per_100000")

# Print covariances to ensure they are correct
print(covs)

# Construct LW Equations
lw_part <- function(index, proxy, var) {
  paste0(proxy, "*rho_", proxy, "/rho_", index)
}

lw_string <- function(index, proxies, var) {
  paste(lapply(proxies, function(proxy) lw_part(index, proxy, var)), collapse = " + ")
}

# Add `hate_crimes_per_100000` to the list of proxies
lw_strings <- lapply(c("mean_skin", "Mean_Index", "hate_crimes_per_100000"), function(var) {
  lw_string("mean_skin", c("mean_skin", "Mean_Index", "hate_crimes_per_100000"), var)
})

# Calculate LW Coefficients Using Delta Method
lw_results <- lapply(lw_strings, function(eq) {
  deltaMethod(lw_model, eq, vcov. = vcov(lw_model), constants = covs)
})

# Extract the estimates and standard errors
coef_estimates <- sapply(lw_results, function(res) res[["Estimate"]])
se_estimates <- sapply(lw_results, function(res) res[["SE"]])

# Print LW Results
print(lw_results)

# Calculate LW Index
# Extract Coefficients
lw_coef_mean_skin <- coef_estimates[1]
lw_coef_Mean_Index <- coef_estimates[2]
lw_coef_hate_crimes <- coef_estimates[3]

# Now calculate the LW index using all three proxies
CPS_IAT$lw_index <- with(CPS_IAT, (mean_skin * lw_coef_mean_skin + Mean_Index * lw_coef_Mean_Index + hate_crimes_per_100000 * lw_coef_hate_crimes) / lw_coef_mean_skin)

# Print the first few LW indices
print(head(CPS_IAT$lw_index))

# # Inspect Calculated LW Index
# print(head(CPS_IAT$lw_index))

# CPS_IAT |> 
#   group_by(statefip, year) |>
#   summarise(value = mean(lw_index, na.rm = TRUE))

reg1 <- list(
  "\\specialcell{(1) \\\\ $H_i$}" = feols(Asian ~ 1 + lw_index + Female 
                                + MomGradCollege + DadGradCollege + frac_asian +
                                Age + Age_sq + Age_cube + Age_quad  + AA_0bj  + Type_Asian| region:year, 
                                data = CPS_IAT, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $H^1_i$}" = feols(Asian ~ 1 + lw_index + Female 
                               + MomGradCollege + DadGradCollege + frac_asian +
                               Age + Age_sq + Age_cube + Age_quad| region:year, 
                               data = CPS_IAT |> filter(Type_Asian == "First Generation Asian"), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $H^2_i$}" = feols(Asian ~ 1 + lw_index + Female 
                               + MomGradCollege + DadGradCollege + frac_asian +
                               Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                               data = CPS_IAT |> filter(Type_Asian == "Second Generation Asian"), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $H^3_i$}" = feols(Asian ~ 1 + lw_index + Female 
                               + MomGradCollege + DadGradCollege + frac_asian +
                               Age + Age_sq + Age_cube + Age_quad + Grandparent_Type| region:year, 
                               data = CPS_IAT |> filter(Type_Asian == "Third Generation Asian"), weights = ~weight, vcov = ~statefip)
)
reg1[[1]]
reg1[[2]]
reg1[[3]]
reg1[[4]]

# save
write_csv(CPS_IAT, file.path(datasets,"CPS_IAT_asian.csv"))
