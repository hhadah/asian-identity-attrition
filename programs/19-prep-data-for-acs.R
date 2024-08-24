### Open ACS data
### of 17 year olds
### living with their
### parents
Skin_Iat <- read_csv(file.path(datasets,"ANES_GSS_Skin.csv")) |> 
  rename(mean_skin = value
         )

ACS <- fread(ACS_path)
ACS <- as.data.frame(ACS)
table(ACS$year)
table(ACS$birthyr)
table(Skin_Iat$Join_year)
ACS <- ACS |> 
  filter(age<18) |> 
  mutate(birthyr_join =  case_when(birthyr == 2005 ~ 2004,
                                birthyr == 2006 ~ 2006,
                                birthyr == 2007 | birthyr == 2008 ~ 2008,
                                birthyr == 2009 | birthyr == 2010 ~ 2010,
                                birthyr == 2011 | birthyr == 2012 ~ 2012,
                                birthyr == 2013 | birthyr == 2014 ~ 2014,
                                birthyr == 2015 | birthyr == 2016 ~ 2016,
                                birthyr == 2017 | birthyr == 2018 ~ 2018,
                                birthyr == 2019 | birthyr == 2020 ~ 2020
                                ),
        Join_year = case_when(year == 2005 ~ 2004,
                                year == 2006 ~ 2006,
                                year == 2007 | year == 2008 ~ 2008,
                                year == 2009 | year == 2010 ~ 2010,
                                year == 2011 | year == 2012 ~ 2012,
                                year == 2013 | year == 2014 ~ 2014,
                                year == 2015 | year == 2016 ~ 2016,
                                year == 2017 | year == 2018 ~ 2018,
                                year == 2019 | year == 2020 ~ 2020                        
                                ))

Skin_Iat <- Skin_Iat %>% 
  # filter(White == 1) |> 
  group_by(statefip, Join_year
  ) %>% 
  summarise(mean_skin = mean(mean_skin, na.rm = TRUE),
            Mean_Index = mean(Mean_Index, na.rm = TRUE))

# merge wit ACS data at year of survey
ACS_IAT <- left_join(ACS,
                     Skin_Iat,
                     na_matches = "never",
                     by = c("statefip", "Join_year"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))

# merge wit ACS data at year of birth
Skin_Iat <- Skin_Iat |> 
  rename(birthyr_join = Join_year,
         bpl=statefip,
         bplMean_Index = Mean_Index,
         bplmean_skin = mean_skin)

ACS_IAT <- left_join(ACS_IAT,
                     Skin_Iat,
                     na_matches = "never",
                     by = c("bpl", "birthyr_join"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))

# merge wit ACS data at year -1 of survey
Skin_Iat <- Skin_Iat |>
  select(birthyr_join, bpl, bplMean_Index, bplmean_skin) |> 
  rename(migyr = birthyr_join,
         statefip=bpl,
         migMean_Index = bplMean_Index,
         migmean_skin = bplmean_skin)

ACS_IAT <- ACS_IAT |> 
  mutate(migyr = 
                      case_when(year == 2005 ~ 2004,
                                year == 2006 ~ 2005,
                                year == 2007 | year == 2008 ~ 2006,
                                year == 2009 | year == 2010 ~ 2008,
                                year == 2011 | year == 2012 ~ 2010,
                                year == 2013 | year == 2014 ~ 2012,
                                year == 2015 | year == 2016 ~ 2014,
                                year == 2017 | year == 2018 ~ 2016,
                                year == 2019 | year == 2020 ~ 2018
                                )
  )

ACS_IAT <- left_join(ACS_IAT,
                     Skin_Iat,
                     na_matches = "never",
                     by = c("statefip", "migyr"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))

# data cleaning
ACS_IAT <- ACS_IAT |>
  filter(Type_Asian != "") |> 
  mutate(
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
         ParentType = case_when(AA == 1 ~ "Asian-Asian",
                                AW == 1 ~ "Asian-White",
                                WA == 1 ~ "White-Asian",
                                WW == 1 ~ "White-White"),
         ParentType = as.factor(ParentType),
         ParentType2 = case_when(AA_0bj == 1 ~ "Asian-Asian",
                                 AW_0bj == 1 ~ "Asian-White",
                                 WA_0bj == 1 ~ "White-Asian",
                                 WW_0bj == 1 ~ "White-White"),
         ParentType2 = as.factor(ParentType2),
         weight = case_when(!is.na(hhwt) ~ hhwt))
CrossTable(ACS_IAT$Type_Asian)
CrossTable(ACS_IAT$FirstGen_Asian)
CrossTable(ACS_IAT$SecondGen_Asian)

# Open fraction Asian data

CPS_frac <- fread(CPS_asian_mean)
CPS_frac <- as.data.frame(CPS_frac)

ACS_IAT <- left_join(ACS_IAT,
                     CPS_frac,
                     na_matches = "never",
                     by = c("statefip", "year"#, 
                            #"month"
                     )) |> 
  rename(frac_asian = MeanAsian)

CrossTable(ACS_IAT$SecondGen_Asian, ACS_IAT$AA_0bj)

CrossTable(ACS_IAT$Type_Asian, ACS_IAT$ParentType2)
CrossTable(ACS_IAT$Type_Asian, ACS_IAT$AA_0bj)

CrossTable(ACS_IAT$FirstGen_Asian)
CrossTable(ACS_IAT$SecondGen_Asian)

# save
write_csv(ACS_IAT, file.path(datasets,"ACS_IAT.csv"))
