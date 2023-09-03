# This a script to 
# merge the skin IAT
# and the Current
# Population Survey (ACS)
# State level

# Date: Oct 25th, 2022

Skin_Iat <- read_csv(file.path(datasets,"Skin_IAT_Clean.csv"))

### Open ACS data
### of 17 year olds
### living with their
### parents

ACS <- fread(ACS_path)
ACS <- as.data.frame(ACS)
ACS <- ACS |> 
  filter(age<18)

### Merge Skin IAT and state
### information

# this .csv contains all state abbrevs, state nos., and lowercase state names
state_info <- read.csv(file.path(Implicit_Race_Harvard,"state_info.csv"))
st.reg <- data.frame(state = state.abb, division = state.division)
st.reg <- rbind(st.reg , data.frame(state="DC", division="South Atlantic") )

state_info <- left_join(state_info,
                        st.reg,
                     na_matches = "never",
                     by = c("state"
                     ))

# Remove people who don't report their state 
Skin_Iat <- Skin_Iat[Skin_Iat$state != "",]

# merge state info with iat data
Skin_Iat <- left_join(Skin_Iat,
                      state_info,
                      na_matches = "never",
                      by = c("state")) |> 
  rename(state_abr = state)

# rename state name variable
Skin_Iat <- Skin_Iat %>% 
  rename(state = state.name)

# calculate average skin
# iat score by state by
# year for only white
# respondents

skin_grouped_bystate <- Skin_Iat %>% 
  # filter(White == 1) |> 
  group_by(state.no, year#, 
           #month
  ) %>% 
  summarise(value = mean(Implicit, na.rm = TRUE),
            Explicit_value = mean(Explicit, na.rm = TRUE),
            division = first(division)) %>% 
  select(state.no,
         #month,
         division,
         year,
         Explicit_value,
         value) |> 
  rename("statefip" = "state.no")

# merge wit ACS data at year of survey
ACS_IAT <- left_join(ACS,
                     skin_grouped_bystate,
                     na_matches = "never",
                     by = c("statefip", "year"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0)) |> 
  filter(!is.na(value))

# merge wit ACS data at year of birth
skin_grouped_bystate <- skin_grouped_bystate |> 
  rename(birthyr = year,
         bplregion = division,
         bpl=statefip,
         bplvalue = value,
         bplExplicit_value = Explicit_value)

ACS_IAT <- left_join(ACS_IAT,
                     skin_grouped_bystate,
                     na_matches = "never",
                     by = c("bpl", "birthyr"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))

# merge wit ACS data at year -1 of survey
skin_grouped_bystate <- skin_grouped_bystate |>
  select(birthyr, bpl, bplvalue, bplExplicit_value) |> 
  rename(migyr = birthyr,
         statefip=bpl,
         migvalue = bplvalue,
         migExplicit_value = bplExplicit_value)

ACS_IAT <- ACS_IAT |> 
  mutate(migyr = case_when(year > 2004 ~ year - 1))

ACS_IAT <- left_join(ACS_IAT,
                     skin_grouped_bystate,
                     na_matches = "never",
                     by = c("statefip", "migyr"#, 
                            #"month"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))
# data cleaning
ACS_IAT <- ACS_IAT |>
  filter(Type != "Fourth Generation+ Hispanic") |> 
  filter(Type != "") |> 
  mutate(
         Age = age,
         Age_sq = age^2,
         Age_cube = age^3,
         Age_quad = age^4,
         HH = ifelse(Hispanic_Dad == 1 & Hispanic_Mom == 1, 1, 0),
         HW = ifelse(Hispanic_Dad == 1 & Hispanic_Mom == 0, 1, 0),
         WH = ifelse(Hispanic_Dad == 0 & Hispanic_Mom == 1, 1, 0),
         WW = ifelse(Hispanic_Dad == 0 & Hispanic_Mom == 0, 1, 0),
         HH_0bj = ifelse((SpanishSpeakingPOB_Father == 1 & SpanishSpeakingPOB_Mother == 1), 1, 0),
         HW_0bj = ifelse((SpanishSpeakingPOB_Father == 1 & SpanishSpeakingPOB_Mother == 0), 1, 0),
         WH_0bj = ifelse((SpanishSpeakingPOB_Father == 0 & SpanishSpeakingPOB_Mother == 1), 1, 0),
         WW_0bj = ifelse((SpanishSpeakingPOB_Father == 0 & SpanishSpeakingPOB_Mother == 0), 1, 0),
         ParentType = case_when(HH == 1 ~ "Hispanic-Hispanic",
                                HW == 1 ~ "Hispanic-White",
                                WH == 1 ~ "White-Hispanic",
                                WW == 1 ~ "White-White"),
         ParentType = as.factor(ParentType),
         ParentType2 = case_when(HH_0bj == 1 ~ "Hispanic-Hispanic",
                                 HW_0bj == 1 ~ "Hispanic-White",
                                 WH_0bj == 1 ~ "White-Hispanic",
                                 WW_0bj == 1 ~ "White-White"),
         ParentType2 = as.factor(ParentType2),
         weight = case_when(!is.na(hhwt) ~ hhwt))
# Open fraction Hispanic data

CPS_frac <- fread(CPS_hispanic_mean)
CPS_frac <- as.data.frame(CPS_frac)

ACS_IAT <- left_join(ACS_IAT,
                     CPS_frac,
                     na_matches = "never",
                     by = c("statefip", "year"#, 
                            #"month"
                     )) |> 
  rename(frac_hispanic = MeanHispanic)

# save
write_csv(ACS_IAT, file.path(datasets,"ACS_IAT.csv"))



