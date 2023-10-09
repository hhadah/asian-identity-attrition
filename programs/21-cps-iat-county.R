# This a script to 
# merge the skin IAT
# and the Current
# Population Survey (CPS)
# MSA level

# Date: Aug 30th, 2022

### Open GSS with by
### state by year
### prejeduice index

Asian_IAT <- read_csv(file.path(datasets,"Asia_IAT_Clean.csv"))

### Open CPS data
### of 17 year olds
### living with their
### parents

CPS <- fread(CPS_asian)
CPS <- as.data.frame(CPS)
CPS <- CPS |> 
  mutate(Proxy = case_when(hhrespln ==lineno ~ "Self",
                           hhrespln ==lineno_mom ~ "Mother",
                           hhrespln ==lineno_mom2 ~ "Mother",
                           hhrespln ==lineno_pop  ~ "Father",
                           hhrespln ==lineno_pop2 ~ "Father",
                           TRUE ~ "Other"),
         county = as.character(county),
        year_group = case_when(year >= 2004 & year <= 2006 ~ 1,
                        year >= 2007 & year <= 2009 ~ 2,
                        year >= 2010 & year <= 2012 ~ 3,
                        year >= 2013 & year <= 2015 ~ 4,
                        year >= 2016 & year <= 2018 ~ 5,
                        year >= 2019 & year <= 2021 ~ 6))

### Merge Skin IAT and state
### information

# this .csv contains all state abbrevs, state nos., and lowercase state names
state_info <- read.csv(file.path(Implicit_Race_Harvard,"state_info.csv"))

county_info   <- read.csv(file.path(Implicit_Race_Harvard,"countyno_state_cbsa.csv")) |> 
  rename("CountyNo" = "countyno1")

# Remove people who don't report their state 
Asian_IAT <- Asian_IAT[Asian_IAT$state != "",]
Asian_IAT <- Asian_IAT[Asian_IAT$CountyNo != "",]

# merge with state info
Asian_IAT <- left_join(Asian_IAT,
                      state_info,
                      na_matches = "never",
                      by = c("state")) |> 
  rename(state_abr = state)

# merge state info with iat data
Asian_IAT <- left_join(Asian_IAT,
                      county_info,
                      na_matches = "never",
                      by = c("CountyNo")) |> 
  rename(county = CountyNo)

# county number needs to be 3 digits, 
# so leading zeroes must be added 
Asian_IAT$county <- str_pad(Asian_IAT$county, 3, pad = "0")

# now concatenation can happen
Asian_IAT <- Asian_IAT %>% 
  mutate(county.full = paste(state.no, county, sep = ""))

# calculate average skin
# iat score by state by
# year for only white
# respondents
# Clean IAT test data
# by creating 3 years
# intervals to avoid
# small samples in
# CPS data
# and only keep tests
# taken by White people

Asian_IAT  <- Asian_IAT |> 
  mutate(year_group = case_when(year >= 2004 & year <= 2006 ~ 1,
                          year >= 2007 & year <= 2009 ~ 2,
                          year >= 2010 & year <= 2012 ~ 3,
                          year >= 2013 & year <= 2015 ~ 4,
                          year >= 2016 & year <= 2018 ~ 5,
                          year >= 2019 & year <= 2021 ~ 6)) |> 
  filter(White == 1)

# calculate average skin
# iat score by state by
# year for only white
# respondents

skin_grouped_bycounty <- Asian_IAT %>% 
  # filter(White == 1) |> 
  group_by(county.full, year_group#, 
           #month
  ) %>% 
  summarise(value = mean(Implicit, na.rm = TRUE)) %>% 
  select(
    county.full,
         #month,
         year_group,
         value) |> 
  rename(county = county.full)

# merge wit CPS data
CPS_IAT <- left_join(CPS,
                     skin_grouped_bycounty,
                     na_matches = "never",
                     by = c("county", "year_group"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0)) |> 
  filter(!is.na(value))

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
# save
write_csv(CPS_IAT, file.path(datasets,"CPS_IAT_asian_county.csv"))

CrossTable(CPS_IAT$Type_Asian, CPS_IAT$Asian)