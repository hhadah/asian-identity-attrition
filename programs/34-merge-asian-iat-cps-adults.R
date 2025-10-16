# This a script to 
# merge the asian IAT
# and the Current
# Population Survey (CPS)

# Date: Sep 21st, 2022

### prejeduice index

ANES_GSS_Skin <- read_csv(file.path(datasets,"ANES_GSS_Skin.csv")) |> 
  rename(mean_skin = value
         )

### Open CPS data
### of 18+ year olds
### living with their
### parents

CPS <- fread(CPS_asian)
CPS <- as.data.frame(CPS)
CPS <- CPS[, !duplicated(names(CPS))]

CPS <- CPS |> 
  filter(age>18) |>
  filter(Type_Asian == "First Generation Asian"  | 
         Type_Asian == "Second Generation Asian") |> 
  mutate(
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
  mutate(
        # inverse hyperbolic sine transformation
        # to handle zero and negative income values
        lnftotval = log(ftotval + sqrt(ftotval^2 + 1)),
        #  lnftotval = log(ftotval),
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

#' Convert IPUMS education codes to years of education
#' @param educ_code Integer vector of IPUMS education codes
#' @return Numeric vector of years of education
convert_educ_to_years <- function(educ_code) {
  case_when(
    educ_code %in% c(0, 1, 2) ~ 0,          # No schooling/preschool
    educ_code %in% c(10, 11) ~ 1,           # Grade 1
    educ_code == 12 ~ 2,                     # Grade 2
    educ_code == 13 ~ 3,                     # Grade 3
    educ_code == 14 ~ 4,                     # Grade 4
    educ_code %in% c(20, 21) ~ 5,           # Grade 5
    educ_code == 22 ~ 6,                     # Grade 6
    educ_code %in% c(30, 31) ~ 7,           # Grade 7
    educ_code == 32 ~ 8,                     # Grade 8
    educ_code == 40 ~ 9,                     # Grade 9
    educ_code == 50 ~ 10,                    # Grade 10
    educ_code == 60 ~ 11,                    # Grade 11
    educ_code %in% c(70:73) ~ 12,           # Grade 12/High school
    educ_code %in% c(80, 81) ~ 13,          # Some college
    educ_code %in% c(90:92) ~ 14,           # Associate degree
    educ_code == 100 ~ 15,                   # 3 years college
    educ_code %in% c(110, 111) ~ 16,        # Bachelor's degree
    educ_code %in% c(120:122) ~ 17,         # Some graduate
    educ_code == 123 ~ 18,                   # Master's degree
    educ_code == 124 ~ 19,                   # Professional degree
    educ_code == 125 ~ 20,                   # Doctorate
    educ_code == 999 ~ NA_real_,             # Missing
    TRUE ~ NA_real_
  )
}

# Apply the conversion function to the relevant columns
CPS_IAT <- CPS_IAT |>
 mutate(
    EducYears = convert_educ_to_years(educ)
 )

summary(CPS_IAT$ftotval)
summary(CPS_IAT$lnftotval)
#---------------------------
# Apply the LW method
#---------------------------

# Prepare Your Data and Model
# Ensure your data (CPS_IAT) is loaded and contains all the necessary variables.

# Ensure Data Completeness
# Select relevant columns
relevant_columns <- c("Asian", "mean_skin", "Mean_Index", "hate_crimes_per_100000","Female", 
                      "EducYears", "frac_asian", "ftotval",
                      "Age", "Age_sq", "Age_cube", "Age_quad", "AA_0bj",
                      "Type_Asian")

# Remove rows with any missing values
names(CPS_IAT)
CPS_IAT <- CPS_IAT[complete.cases(CPS_IAT[, relevant_columns]), ]
names(CPS_IAT)

# Estimate Your Primary Model with feols
lw_model <- feols(Asian ~ mean_skin + Mean_Index + hate_crimes_per_100000 + Female 
                  + EducYears + lnftotval + frac_asian
                  + Age + Age_sq + Age_cube + Age_quad + AA_0bj 
                  + Type_Asian | region:year, 
                  data = CPS_IAT, weights = ~weight, vcov = ~statefip)

# Residualize Proxies and Dependent Variable
control_vars <- c("Female", "EducYears", "frac_asian", "lnftotval", "Age", "Age_sq", "Age_cube", "Age_quad", "AA_0bj", "Type_Asian")
X <- model.matrix(~ Female + EducYears + frac_asian + lnftotval + Age + Age_sq + Age_cube + Age_quad + AA_0bj + Type_Asian, data = CPS_IAT)

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
                                + EducYears + frac_asian + lnftotval +
                                Age + Age_sq + Age_cube + Age_quad  + AA_0bj  + Type_Asian| region:year, 
                                data = CPS_IAT, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $H^1_i$}" = feols(Asian ~ 1 + lw_index + Female 
                               + EducYears + frac_asian + lnftotval +
                               Age + Age_sq + Age_cube + Age_quad| region:year, 
                               data = CPS_IAT |> filter(Type_Asian == "First Generation Asian"), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $H^2_i$}" = feols(Asian ~ 1 + lw_index + Female 
                               + EducYears + frac_asian + lnftotval +
                               Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                               data = CPS_IAT |> filter(Type_Asian == "Second Generation Asian"), weights = ~weight, vcov = ~statefip)
)
reg1[[1]]
reg1[[2]]
reg1[[3]]

# save
write_csv(CPS_IAT, file.path(datasets,"CPS_IAT_asian_adults.csv"))
