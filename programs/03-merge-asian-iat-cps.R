# This a script to 
# merge the asian IAT
# and the Current
# Population Survey (CPS)

# Date: Sep 21st, 2022

### prejeduice index

Asian_IAT <- read_csv(file.path(datasets,"Asia_IAT_Clean.csv"))

### Open CPS data
### of 17 year olds
### living with their
### parents

CPS <- fread(CPS_asian)
CPS <- as.data.frame(CPS)
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
                           TRUE ~ "Other"))

### Merge Skin IAT and state
### information

# this .csv contains all state abbrevs, state nos., and lowercase state names
state_info <- read.csv(file.path(Implicit_Race_Harvard,"state_info.csv"))

# Remove people who don't report their state 
Asian_IAT <- Asian_IAT[Asian_IAT$state != "",]

# merge state info with iat data
Asian_IAT <- left_join(Asian_IAT,
                      state_info,
                      na_matches = "never",
                      by = c("state")) |> 
  rename(state_abr = state)

# rename state name variable
Asian_IAT <- Asian_IAT %>% 
  rename(state = state.name)

# calculate average skin
# iat score by state by
# year for only white
# respondents

skin_grouped_bystate <- Asian_IAT %>% 
  # filter(White == 1) |> 
  group_by(state.no, year#, 
           #month
           ) %>% 
  summarise(value = mean(Implicit, na.rm = TRUE),
            Explicit_value = mean(Explicit, na.rm = TRUE)) %>% 
  select(state.no,
         #month,
         year,
         Explicit_value,
         value) |> 
  rename("statefip" = "state.no")

# merge wit CPS data
CPS_IAT <- left_join(CPS,
                     skin_grouped_bystate,
                     na_matches = "never",
                     by = c("statefip", "year"#, 
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
write_csv(CPS_IAT, file.path(datasets,"CPS_IAT_asian.csv"))
