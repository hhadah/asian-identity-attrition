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

# this .csv contains all state abbrevs, state nos., and lowercase state names
state_info <- read.csv(file.path(Implicit_Race_Harvard,"state_info.csv"))

# Remove people who don't report their state 
Asia_IAT <- Asia_IAT[Asia_IAT$state != "",]

# merge state info with iat data
Asia_IAT <- left_join(Asia_IAT,
                      state_info,
                      na_matches = "never",
                      by = c("state")) |> 
  rename(state_abr = state)

# rename state name variable
Asia_IAT <- Asia_IAT |> 
  rename(state = state.name)

summary(Asia_IAT$Implicit)
# calculate average skin
# iat score by state by
# year for only white
# respondents

skin_grouped_bystate <- Asia_IAT |> 
  # filter(White == 1) |> 
  group_by(state.no, year#, 
           #month
           ) |> 
  summarise(value = mean(Implicit, na.rm = TRUE),
            Explicit_value = mean(Explicit, na.rm = TRUE)) |> 
  select(state.no,
         #month,
         year,
         Explicit_value,
         value) |> 
  rename("statefip" = "state.no")

### Open ANES data
### and rename some variables 
ANES <- read_csv(ANES_dir) |> 
    # clean_names() |> 
    rename(year             = VCF0004,
           statefip         = VCF0901a,
           state_ab         = VCF0901b,
           gender           = VCF0104,
           age              = VCF0103,
           race_det         = VCF0105a,
           race             = VCF0105b,
           race_sum         = VCF0106,
           educ             = VCF0110,
           party_identity   = VCF0303,
           race_res_1       = VCF9040,
           race_res_2       = VCF9039,
           race_res_3       = VCF9042,
           race_res_4       = VCF9041,
           feelings_hisp    = VCF0217,
           feelings_blk     = VCF0206,
           feelings_wmn     = VCF0225,
           feelings_asn     = VCF0227,
           hardworking_blk  = VCF9271,
           hardworking_hisp = VCF9272,
           hardworking_asn  = VCF9273
           ) |> 
    select(
        year, statefip, state_ab, 
        gender, age, race_det, race_det, race_sum, educ, party_identity,
        race_res_1, race_res_2, race_res_3, race_res_4, 
        feelings_hisp, feelings_blk, feelings_wmn, feelings_asn,
       hardworking_blk, hardworking_hisp, hardworking_asn      
    ) |> 
    glimpse()

table(ANES$state_ab)
table(ANES$feelings_hisp)
table(ANES$feelings_blk)
table(ANES$feelings_wmn)
table(ANES$feelings_asn)
table(ANES$hardworking_blk)
table(ANES$hardworking_hisp)
table(ANES$hardworking_asn)

# Create racial animosity var
ANES <- ANES |>
  mutate(race_res_1_rec = case_when(race_res_1 == 1   ~ 4/4,
                                    race_res_1 == 2   ~ 3/4,
                                    race_res_1 == 3   ~ 2/4,
                                    race_res_1 == 4   ~ 1/4,
                                    race_res_1 == 5   ~ 0,
                                    TRUE ~ NA_real_),
         race_res_2_rec = case_when(race_res_2 == 1   ~ 0,
                                    race_res_2 == 2   ~ 1/4,
                                    race_res_2 == 3   ~ 2/4,
                                    race_res_2 == 4   ~ 3/4,
                                    race_res_2 == 5   ~ 4/4,
                                    TRUE ~ NA_real_),
         race_res_3_rec = case_when(race_res_3 == 1   ~ 0,
                                    race_res_3 == 2   ~ 1/4,
                                    race_res_3 == 3   ~ 2/4,
                                    race_res_3 == 4   ~ 3/4,
                                    race_res_3 == 5   ~ 4/4,
                                    TRUE ~ NA_real_),
         race_res_4_rec = case_when(race_res_4 == 1   ~ 4/4,
                                    race_res_4 == 2   ~ 3/4,
                                    race_res_4 == 3   ~ 2/4,
                                    race_res_4 == 4   ~ 1/4,
                                    race_res_4 == 5   ~ 0,
                                    TRUE ~ NA_real_),
         feelings_hisp = case_when(feelings_hisp <= 97 ~ feelings_hisp/100,
                                  feelings_hisp == 98 ~ 0,
                                  feelings_hisp == 99 ~ 0,
                                  TRUE ~ NA_real_),
         feelings_blk = case_when(feelings_blk <= 97 ~ feelings_blk/100,
                                       feelings_blk == 98 ~ 0,
                                       feelings_blk == 99 ~ 0,
                                       TRUE ~ NA_real_),
         feelings_wmn = case_when(feelings_wmn <= 97 ~ feelings_wmn/100,
                                 feelings_wmn == 98 ~ 0,
                                 feelings_wmn == 99 ~ 0,
                                 TRUE ~ NA_real_),
         feelings_asn = case_when(feelings_asn <= 97 ~ feelings_asn/100,
                                       feelings_asn == 98 ~ 0,
                                       feelings_asn == 99 ~ 0,
                                       TRUE ~ NA_real_),
         hardworking_blk = case_when(hardworking_blk <= 7 ~ hardworking_blk/7,
                                 hardworking_blk == -8 ~ 0,
                                 hardworking_blk == -9 ~ 0,
                                 TRUE ~ NA_real_),
         hardworking_hisp = case_when(hardworking_hisp <= 7 ~ hardworking_hisp/7,
                             hardworking_hisp == -8 ~ 0,
                             hardworking_hisp == -9 ~ 0,
                             TRUE ~ NA_real_),
         hardworking_asn = case_when(hardworking_asn <= 7 ~ hardworking_asn/7,
                             hardworking_asn == -8 ~ 0,
                             hardworking_asn == -9 ~ 0,
                             TRUE ~ NA_real_)

        )
ANES <- ANES |>
  mutate(race_anim = rowMeans(ANES[c("race_res_1_rec", "race_res_2_rec", "race_res_3_rec", "race_res_4_rec",
                                     "feelings_asn"#, 
                                    #  "feelings_blk", 
                                    #  "feelings_wmn", 
                                    #  "feelings_asn",
                                    #  "hardworking_blk", 
                                    #  "hardworking_hisp"#, 
                                     #"hardworking_asn"
                                     )], na.rm = T))
ANES <- ANES |>
       mutate(scaled_index = scale(race_anim, center = T, scale = T))
# Create a dataframe with the mean racial animosity by state and year
ANES_grouped_bystate_year <- ANES |> 
  group_by(statefip, year) |>
  summarise(Mean_Index = mean(scaled_index, na.rm = TRUE)) |> 
  select(statefip, 
         Mean_Index,
         year) |> 
  mutate(state = fips(statefip, to = "Name"))

# create a dataframe with the state names in lowercase
# and remove the years with NA raceanon values
# and create a join_year variable to join with CPS data
ANES_grouped_bystate_year <- ANES_grouped_bystate_year |> 
  mutate(state = tolower(state),
         Join_year = year) |> 
  # rename(year_anes = year) |> 
  filter(!is.na(Mean_Index))

ANES_grouped_bystate_year  <- ANES_grouped_bystate_year |> 
  mutate(Trump_2015 = case_when(year <= 2015 ~ 0,
                                year >  2015 ~ 1,
                                ))
feols(Mean_Index ~ Trump_2015 | statefip, data = ANES_grouped_bystate_year, vcov = ~statefip)
feols(Mean_Index ~ i(year) | statefip, data = ANES_grouped_bystate_year, vcov = ~statefip)

summary(ANES_grouped_bystate_year$Mean_Index)

ANES_grouped_bystate_year |> 
       group_by(year) |>
       summarise(mean(Mean_Index, na.rm = T)) |> 
       print(n=Inf)
table(ANES_grouped_bystate_year$year)
### Open GSS with by
### state by year
### prejeduice index
 
GSS_bystate <- read_csv(GSS_bystate_new_path)
table(GSS_bystate$YEAR)

# # Some maps
GSS_bystate |> 
  group_by(fipsstat, YEAR) |>
  summarise(Mean_AggregateIndex = mean(Mean_Hispanic_Index, na.rm = TRUE))
GSS_grouped_bystate_year <- GSS_bystate |> 
  group_by(fipsstat, YEAR) |>
  summarise(Mean_AggregateIndex = mean(Mean_Hispanic_Index, na.rm = TRUE)) |> 
  select(fipsstat, 
         Mean_AggregateIndex,
         YEAR) |> 
  mutate(state = fips(fipsstat, to = "Name"))

GSS_grouped_bystate_year <- GSS_grouped_bystate_year |> 
  mutate(state = tolower(state))

table(GSS_grouped_bystate_year$YEAR)
table(ANES_grouped_bystate_year$year)
table(Asia_IAT$year)

### Open Hate Crime Data
Asian_hatecrime <- read_csv(hate_crime) |> 
    rename(statefip = fstate)
Asian_hatecrime |> glimpse()

Asian_hatecrime <- Asian_hatecrime |> 
  filter(biasmo1 == 14) |> 
  group_by(year, statefip) |> 
  summarize(hate_crimes_per_100000 = sum(hate_crimes_per_100000),
            hate_crimes = sum(hate_crimes))
table(Asian_hatecrime$year)
#-------------------------------
# Merge ANES, GSS, and Skin IAT
# by creating a year variable
# starting 1994
#-------------------------------
GSS_grouped_bystate_year <- GSS_grouped_bystate_year |> 
  mutate(Join_year = case_when(YEAR == 1994 ~ 1994,
                               YEAR == 1996 ~ 1996,
                               YEAR == 1998 ~ 1998,
                               YEAR == 2000 ~ 2000,
                               YEAR == 2002 ~ 2002,
                               YEAR == 2004 ~ 2004,
                               YEAR == 2006 ~ 2006,
                               YEAR == 2008 ~ 2008,
                               YEAR == 2010 ~ 2010,
                               YEAR == 2012 ~ 2012,
                               YEAR == 2014 ~ 2014,
                               YEAR == 2016 ~ 2016,
                               YEAR == 2018 ~ 2018,
                               YEAR == 2020 | YEAR == 2021 ~ 2020,
                               TRUE ~ NA_real_))

ANES_grouped_bystate_year <- ANES_grouped_bystate_year |>
  mutate(Join_year = case_when(year == 1994 ~ 1994,
                               year == 1996 ~ 1996,
                               year == 1998 ~ 1998,
                               year == 2000 ~ 2000,
                               year == 2002 ~ 2002,
                               year == 2004 ~ 2004,
                               year == 2008 ~ 2008,
                               year == 2012 ~ 2012,
                               year == 2016 ~ 2016,
                               year == 2020 ~ 2020,
                               TRUE ~ NA_real_))

skin_grouped_bystate <- skin_grouped_bystate |>
  mutate(Join_year = case_when(year == 2004 | year == 2005 ~ 2004,
                               year == 2006 | year == 2007 ~ 2006,
                               year == 2008 | year == 2009 ~ 2008,
                               year == 2010 | year == 2011 ~ 2010,
                               year == 2012 | year == 2013 ~ 2012,
                               year == 2014 | year == 2015 ~ 2014,
                               year == 2016 | year == 2017 ~ 2016,
                               year == 2018 | year == 2019 ~ 2018,
                               year == 2020 | year == 2021 ~ 2020,
                               TRUE ~ NA_real_))
Asian_hatecrime <- Asian_hatecrime |>
  mutate(Join_year = case_when(
                               year == 1994 | year == 1995 ~ 1994,
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
                               TRUE ~ NA_real_))

# combined_data <- combined_data |>
#   mutate(Join_year = case_when(year == 2004 | year == 2005  ~ 2004,
#                                year == 2006 | year == 2007 ~ 2006,
#                                year == 2008 | year == 2009 ~ 2008,
#                                year == 2010 | year == 2011 ~ 2010,
#                                year == 2012 | year == 2013 ~ 2012,
#                                year == 2014 | year == 2015 ~ 2014,
#                                year == 2016 | year == 2017 ~ 2016,
#                                year == 2018 | year == 2019 ~ 2018,
#                                year == 2020 | year == 2021 ~ 2020,
#                                TRUE ~ NA_real_),
#          statefip = fips(state, to = "FIPS"))

skin_grouped_bystate <- skin_grouped_bystate |> 
  group_by(statefip, Join_year#, 
           #month
           ) |> 
  summarise(value = mean(value, na.rm = TRUE),
            Explicit_value = mean(Explicit_value, na.rm = TRUE))
Asian_hatecrime <- Asian_hatecrime |> 
  group_by(statefip, Join_year#, 
           #month
           ) |> 
  summarise(hate_crimes_per_100000 = sum(hate_crimes_per_100000, na.rm = TRUE))
# combined_data <- combined_data |> 
#   group_by(statefip, Join_year#, 
#            #month
#            ) |> 
#   summarise(racially_charged_search_rate = mean(racially_charged_search_rate, na.rm = TRUE))


table(ANES_grouped_bystate_year$Join_year)
table(GSS_grouped_bystate_year$Join_year)
table(skin_grouped_bystate$Join_year)
# table(combined_data$Join_year)
table(ANES_grouped_bystate_year$statefip)
table(GSS_grouped_bystate_year$fipsstat)
table(skin_grouped_bystate$statefip)

GSS_grouped_bystate_year <- GSS_grouped_bystate_year |> 
  filter(!is.na(Join_year))
table(GSS_grouped_bystate_year$YEAR)
# GSS_grouped_bystate_year <- GSS_grouped_bystate_year |> 
#   rename("statefip" = "fipsstat",
#          "state_gss" = "state",
#          "year_gss" = "YEAR"
#   )
ANES_grouped_bystate_year <- ANES_grouped_bystate_year |> 
  filter(!is.na(Join_year)) |> 
  rename("state_anes" = "state",
         "year_anes" = "year"
  )
table(ANES_grouped_bystate_year$year_anes)
table(ANES_grouped_bystate_year$Join_year)

# Merge ANES and GSS
# ANES_GSS <- full_join(ANES_grouped_bystate_year,
#                       GSS_grouped_bystate_year,
#                       by = c("statefip", "Join_year"),
#                       na_matches = "never")
# table(ANES_GSS$Join_year)
# Merge ANES, GSS, and Skin IAT
ANES_GSS_Skin <- full_join(ANES_grouped_bystate_year,
                           skin_grouped_bystate,
                           by = c("statefip", "Join_year"),
                           na_matches = "never")
ANES_GSS_Skin <- full_join(ANES_GSS_Skin,
                           Asian_hatecrime,
                           by = c("statefip", "Join_year"),
                           na_matches = "never")
names(ANES_GSS_Skin)
summary(ANES_GSS_Skin$Mean_Index)
summary(ANES_GSS_Skin$value)
# summary(ANES_GSS_Skin$Mean_AggregateIndex)

table(ANES_GSS_Skin$Join_year)

feols(Mean_Index ~ value, data = ANES_GSS_Skin)

# # Merge ANES, Google, and Skin IAT
# ANES_GSS_Skin <- full_join(ANES_GSS_Skin,
#                            combined_data,
#                            by = c("statefip", "Join_year"),
#                            na_matches = "never")
names(ANES_GSS_Skin)
summary(ANES_GSS_Skin$Mean_Index)
summary(ANES_GSS_Skin$value)
# summary(ANES_GSS_Skin$racially_charged_search_rate)
# summary(ANES_GSS_Skin$Mean_AggregateIndex)

table(ANES_GSS_Skin$Join_year)

feols(Mean_Index ~ value, data = ANES_GSS_Skin)

# feols(Mean_AggregateIndex ~ value, data = ANES_GSS_Skin)

#-------------------------------
# Create index
#-------------------------------
ANES_GSS_Skin <- ANES_GSS_Skin |>
  mutate(
    Mean_Index = as.numeric(Mean_Index),
    hate_crimes_per_100000 = ifelse(is.na(hate_crimes_per_100000), 0, hate_crimes_per_100000),
    # racially_charged_search_rate,
    value = as.numeric(value)
  ) |>
  rowwise() |>
  mutate(Value = sum(c(Mean_Index,
                       hate_crimes_per_100000, 
                       value), na.rm = T)) |>
  ungroup()
ANES_GSS_Skin |> filter(Join_year == 1994)

feols(Value ~ Mean_Index + value, data = ANES_GSS_Skin)

# extract state name from statefip
ANES_GSS_Skin <- ANES_GSS_Skin |> 
  mutate(state = fips(statefip, to = "Name"))
# save the data
write_csv(ANES_GSS_Skin, file.path(datasets,"ANES_GSS_Skin.csv"))

#-------------------------------
# Maps
#-------------------------------

### Get lat & long info -----

# this "states" dataframe has the lat & long info needed for mapping.
states <- st_as_sf(map('state', plot = TRUE, fill = TRUE))
# states <- map_data("state")
states <- states %>% 
  rename(state = ID)

# change state to lowercase
ANES_GSS_Skin <- ANES_GSS_Skin |> 
  mutate(state = tolower(state))
# join IAT + lowercase names to df that has lat & long
ANES_GSS_Skin <- left_join(ANES_GSS_Skin, 
                                   states, 
                                   by = c("state")) 
table(ANES_GSS_Skin$Join_year)
ANES_GSS_Skin <- st_as_sf(ANES_GSS_Skin)
library(tidyverse)
library(tigris)

sts <- states() |> 
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP'))

DIVISION <- sts %>%
  group_by(DIVISION) %>% 
  summarize()

# List of years to plot
years_to_plot <- c(1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)

# Loop to plot all maps
for (year_map in years_to_plot) {
  data_year <- ANES_GSS_Skin |> filter(Join_year == year_map)
  
  map <- ggplot() + 
    geom_sf(data = data_year, aes(fill = Value), color = "white") +
    geom_sf(data = ANES_GSS_Skin, color = 'white', fill = NA, size = 0.01) +
    geom_sf(data = DIVISION, color = 'black', fill = NA, lwd = 1.0) +
    scale_fill_distiller(palette = "Spectral", guide = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 20, barheight = 1)) +
    labs(fill = "Bias") +
    theme_customs_map() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, hjust = 0.5))
  
  ggsave(path = figures_wd, filename = paste0(year_map, "new_index.png"))
  ggsave(path = "~/Documents/GiT/Attitudes-and-Identity/my_paper/figure", 
         filename = paste0(year_map, "new_index.png.png"), width = 8, height = 5, 
         units = "in")
}

# by_state <- ANES_GSS_Skin |> 
#   group_by(statefip) |> 
#   summarise(Value = mean(Value, na.rm = TRUE))

# map <- ggplot() + geom_sf(data = ANES_GSS_Skin, 
#                           aes(fill = Value), 
#                           color = "white")+
#   geom_sf(data = ANES_GSS_Skin, 
#           color = 'white', 
#           fill = NA,
#           size = 0.01) +
#   geom_sf(data = DIVISION, 
#           color = 'red', 
#           fill = NA,
#           lwd = 1.5) +
#   scale_fill_viridis_c(option = "D", direction = -1, name = "Bias"#,
#                        #breaks = seq(-0.4,0.4,0.2)
#   ) +
#   theme_customs_map() +
#   # annotation_scale(location = "bl", width_hint = 0.4) +
#   # annotation_north_arrow(location = "bl", which_north = "true", 
#   #                        pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
#   #                        style = north_arrow_fancy_orienteering) +
#   theme(legend.position = "bottom") +  scale_size_area()
# map
# ggsave(path = figures_wd, filename = paste0("ALL","year_map.png"))
# ggsave(path = "~/Documents/GiT/Attitudes-and-Identity/my_paper/figure", 
#        filename = paste0("ALL","year_map.png"), width = 8, height = 5, 
#        units = c("in"))
