# This a script to 
# merge the Harvard's
# Asian Implicit Project
# and the Current
# Population Survey (CPS)

# Date: Sep 21st, 2022

### Open Implicit

Asia_IAT <- fread(Implicit_asian_Harvard)
Asia_IAT <- Asia_IAT[session_status == "C"]
Asia_IAT <- as.data.frame(Asia_IAT)
Asia_IAT <- Asia_IAT %>%
  select(session_id, 
         D_biep.White_American_all, 
         D_biep.White_American_36,
         att_7, D_biep.White_American_47, 
         politicalid, politicalid_7, age, 
         year, num, 
         religion, countrycit, religionid,
         MSANo, CountyNo, MSAName, STATE,
         pct_300, PCT_error_3467, 
         session_status, edu_14, 
         ethnicityomb, month, ethnic, 
         raceomb, raceomb_002,
         raceombmulti, ethnicityomb, sex, birthsex) %>%
  mutate(Implicit=D_biep.White_American_all,
         Explicit=att_7,
         Error = case_when(pct_300 <= 10 & PCT_error_3467<= 30 ~ 'No',
                           pct_300 >  10 | PCT_error_3467 > 30 ~ 'Yes'),
         Female = case_when(birthsex == 2 ~ 1,
                            birthsex == 1 ~ 0),
         #White = ifelse(raceomb == 6 | raceomb_002 == 6 , 1, 0),
         White = case_when(raceomb     == 6 ~ 1,
                           raceomb_002 == 6 ~ 1,
                           ethnic      == 5 ~ 1,
                           ethnic      != 5 ~ 0,
                           raceomb     != 6 ~ 0,
                           raceomb_002 != 6 ~ 0),
         Hispanic = case_when(ethnicityomb == 1 ~ 1, 
                              ethnicityomb != 1 ~ 0,
                              ethnic == 4 ~ 1,
                              ethnic != 4 ~ 0),
         White_not_his = ifelse(White == 1 & Hispanic == 0, 1, 0)) %>% 
  filter(Error == 'No')

Asia_IAT <- Asia_IAT %>% 
  mutate(politics = ifelse(politicalid == -999, NA, politicalid),
         religiosity=religionid,
         numiats = case_when(num == '0' ~ 1,
                             num == '1' ~ 2,
                             num == '2' ~ 3,
                             num == '3' ~ 4,
                             num == '3-5' ~ 4,
                             num == '6+' ~ 5),
         Age_group = case_when(age >= 0 & age <= 20   ~ 1,
                             age >= 21 & age <= 30  ~ 2,
                             age >= 31 & age <= 40  ~ 3,
                             age >= 41 & age <= 50  ~ 4,
                             age >= 51 & age <= 60  ~ 5,
                             age >= 61 & age <= 200 ~ 6)) %>% 
  rename("state" = "STATE")

write_csv(Asia_IAT, file.path(datasets,"Asia_IAT_Clean.csv"))
