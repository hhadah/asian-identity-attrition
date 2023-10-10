# This a script to 
# merge the asian IAT
# and the Current
# Population Survey (CPS)

# Date: Sep 21st, 2022

### prejeduice index

Asian_hatecrime <- read_csv(hate_crime) |> 
    rename(statefip = fstate)

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
                           TRUE ~ "Other"),
        year_group = case_when(
                        year >= 1994 & year <= 1996 ~ 1,
                        year >= 1997 & year <= 1999 ~ 2,
                        year >= 2000 & year <= 2002 ~ 3,
                        year >= 2003 & year <= 2005 ~ 4,
                        year >= 2006 & year <= 2008 ~ 5,
                        year >= 2009 & year <= 2011 ~ 6,
                        year >= 2012 & year <= 2014 ~ 7,
                        year >= 2015 & year <= 2017 ~ 8,
                        year >= 2018 & year <= 2020 ~ 9))

# merge wit CPS data
CPS_hatecrime <- left_join(CPS,
                     Asian_hatecrime,
                     na_matches = "never",
                     by = c("statefip", "year_group"#, 
                            #"month"
                            )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))


CPS_hatecrime <- CPS_hatecrime |>
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

CPS_hatecrime <- left_join(CPS_hatecrime,
                     CPS_frac,
                     na_matches = "never",
                     by = c("statefip", "year"#, 
                            #"month"
                     )) |> 
  rename(frac_asian = MeanAsian)

# save
write_csv(CPS_hatecrime, file.path(datasets,"CPS_hatecrime_asian.csv"))
