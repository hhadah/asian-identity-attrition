library(ggspatial) # scale bars and north arrows
library(ggmap)

# Import data with geographic variables from Github
Skin_IAT <- read.csv(file.path(datasets,"Skin_IAT_Clean.csv")) |> 
  mutate(Implicit=(Implicit-mean(Implicit, na.rm = T))/SD(Implicit, na.rm = T))

# this .csv contains all state abbrevs, state nos., and lowercase state names
state_info <- read.csv(file.path(datasets,"state_info.csv"))

# Remove people who don't report their state 
Skin_IAT <- Skin_IAT[Skin_IAT$state != "",]

# merge state info with iat data
Skin_IAT <- merge(Skin_IAT, state_info, 
                  by = "state", 
                  all = TRUE) %>% 
  rename(state_abr = state)

Skin_IAT <- Skin_IAT %>% 
  rename(state = state.name)

skin_grouped_bystate <- Skin_IAT %>% 
  group_by(state, year) %>% 
  summarise(value = mean(Implicit, na.rm = TRUE)) %>% 
  select(state,
         year,
         value)

### Get lat & long info -----

# this "states" dataframe has the lat & long info needed for mapping.
states <- st_as_sf(map('state', plot = TRUE, fill = TRUE))
# states <- map_data("state")
states <- states %>% 
  rename(state = ID)

# join IAT + lowercase names to df that has lat & long
skin_grouped_bystate <- inner_join(skin_grouped_bystate, 
                                   states, 
                                   by = "state") 
skin_grouped_bystate <- st_as_sf(skin_grouped_bystate)
library(tidyverse)
library(tigris)

sts <- states() |> 
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP'))

DIVISION <- sts %>%
  group_by(DIVISION) %>% 
  summarize()

# use for loop to plot all maps

for (year_map in seq(2004,2021)) {
  map <- ggplot() + geom_sf(data = skin_grouped_bystate |> filter(year == year_map), 
                            aes(fill = value), 
                            color = "white")+
    geom_sf(data = skin_grouped_bystate, 
            color = 'white', 
            fill = NA,
            size = 0.01) +
    geom_sf(data = DIVISION, 
            color = 'red', 
            fill = NA,
            size = 0.9) +
    scale_fill_viridis_c(option = "D", direction = -1, name = "Bias"#,
                         #breaks = seq(-0.4,0.4,0.2)
                         ) +
    theme_customs_map()  +
    # annotation_scale(location = "bl", width_hint = 0.4) +
    # annotation_north_arrow(location = "bl", which_north = "true", 
    #                        pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  map
  ggsave(path = figures_wd, filename = paste0(year_map,"skinmap.png"))
  ggsave(path = "~/Documents/GiT/Attitudes-and-Identity/my_paper/figure", 
         filename = paste0(year_map,"skinmap.png"), width = 8, height = 5, 
         units = c("in"))
  
}

map <- ggplot() + geom_sf(data = skin_grouped_bystate, 
                          aes(fill = value), 
                          color = "white")+
  geom_sf(data = skin_grouped_bystate, 
          color = 'white', 
          fill = NA,
          size = 0.01) +
  geom_sf(data = DIVISION, 
          color = 'red', 
          fill = NA,
          lwd = 1.5) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "Bias"#,
                       #breaks = seq(-0.4,0.4,0.2)
  ) +
  theme_customs_map() +
  # annotation_scale(location = "bl", width_hint = 0.4) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  theme(legend.position = "bottom") +  scale_size_area()
map
ggsave(path = figures_wd, filename = paste0(year_map,"skinmap.png"))
ggsave(path = "~/Documents/GiT/Attitudes-and-Identity/my_paper/figure", 
       filename = paste0(year_map,"skinmap.png"), width = 8, height = 5, 
       units = c("in"))

map_facet <- ggplot() + geom_sf(data = skin_grouped_bystate |> filter(year %in% seq(2004,2021,4)), 
                                aes(fill = value), 
                                color = "white")+
  facet_wrap(~year) +
  geom_sf(data = DIVISION, 
          color = 'red', 
          fill = NA,
          size = 10) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  theme_customs_map() +
  theme(legend.position = "bottom") +
  labs(title = paste0("Implicit Skin Tone Prejeduice 
       Scores: by State ", year_map))
map_facet
ggsave(path = figures_wd, filename = "skinmap_facet.png")

# animation
library(gganimate)
library(transformr)
library(tweenr)
skin_grouped_bystate <- skin_grouped_bystate |> 
  mutate(group = 1)
map <- ggplot() + geom_sf(data = skin_grouped_bystate, 
                          aes(fill = value, 
                              group=group)) +
  scale_fill_viridis_c(option = "D") 
map
num_years <- max(skin_grouped_bystate$year) - min(skin_grouped_bystate$year) + 1

map_with_animation <- map +
  # transition_time(year) +
  transition_states(year)
# +
#   labs(title = "Implicit Skin Tone Prejeduice 
#        Scores: by State {frame_time}")
# transition_time(year) +
# transition_states(year,
#                   transition_length = 2,
#                   state_length = 2) +
# ease_aes('linear')

map_with_animation_gif <- animate(map_with_animation, nframes = num_years)

map_with_animation_gif

anim_save(file.path(figures_wd,"maps_year.gif"), map_with_animation_gif)

## # ggplot with state labels -----

# Use state_info to add state abbreviations to be used for labelling
state_info$region <- state_info$state.name
race_grouped_bystate <- merge(race_grouped_bystate, state_info, by = "region")

# Create dataframe of labels
statelabels <- aggregate(cbind(long, lat) ~ state, data = race_grouped_bystate, FUN = function(x) mean(range(x)))

# Some state labels aren't in good positions; can change here 
# View(statelabels)
statelabels[12, c(2:3)] <- c(-114.5, 43.5)  # alter idaho's coordinates
statelabels[17, c(2:3)] <- c(-92.5, 31.75)  # alter louisiana's coordinates
statelabels[21, c(2:3)] <- c(-84.5, 42.75)  # alter michigan's coordinates
statelabels[9, c(2, 3)] <- c(-81.5, 28.75)  # alter florida's angle and coordinates
statelabels[44, c(2, 3)] <- c(-79, 37)      # alter virginia's angle and coordinates
statelabels[45, c(2, 3)] <- c(-72.87, 45.7) # vermont
statelabels[29, c(2, 3)] <- c(-71.64, 43.5) # nh
statelabels[18, c(2, 3)] <- c(-70, 42.5)    # ma
statelabels[32, c(2, 3)] <- c(-116.5, 40)   # nevada 
statelabels[25, c(2, 3)] <- c(-109, 47)     # montana
statelabels[35, c(2, 3)] <- c(-97.5, 35.5)  # oklahoma
statelabels[22, c(2, 3)] <- c(-94, 47)      # minnesota
statelabels[38, c(2, 3)] <- c(-71, 41.3)    # ri
statelabels[30, c(2, 3)] <- c(-73.22, 40.15)    # nj
statelabels[8, c(2, 3)] <- c(-74.42, 38.9)    # de
statelabels[19, c(2, 3)] <- c(-74.2, 38)    # md
statelabels[7, c(2, 3)] <- c(-74.2, 37)    # dc
statelabels[26, c(2, 3)] <- c(-79.9, 35.6)    # nc
statelabels[44, c(2, 3)] <- c(-79, 37.5)    # va
statelabels[16, c(2, 3)] <- c(-85.68584, 37.5)    # ky
statelabels[4, c(2, 3)] <- c(-120.25830, 37.27950)    # ca

ggplot() + geom_polygon(data = race_grouped_bystate, 
                        aes(x = long, y = lat, group = group, fill = value), 
                        color = "white") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1.0)) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
  geom_text(data = statelabels, aes(long, lat, label = state), size = 4.0) 

## # ggplot with value labels -----

# Add IAT values to labels coordinates.
race_grouped_bystate2 <- Race_IAT %>% 
  group_by(state.name) %>% 
  summarize(value = mean(Implicit, na.rm = TRUE)) %>% 
  select(region = state.name, 
         value)

valuelabels_bystate <- merge(state_info, race_grouped_bystate2, 
                             by = "region", 
                             all = TRUE)
valuelabels_bystate <- valuelabels_bystate %>% select(state, state.name, value)
valuelabels_bystate <- merge(valuelabels_bystate, statelabels, 
                             by = "state",
                             all = TRUE)

valuelabels_bystate$value <- round(valuelabels_bystate$value, 2) # round for labelling map
valuelabels_bystate <- na.omit(valuelabels_bystate)

# remove leading 0s from values to take up less space on map
library(weights)
valuelabels_bystate$value <- rd(valuelabels_bystate$value) 

# change variable name to one I'd prefer to print on ggplot image
valuelabels_bystate$`IAT score` <- valuelabels_bystate$value

ggplot() + geom_polygon(data = race_grouped_bystate, 
                        aes(x = long, y = lat, group = group, fill = value), 
                        color = "white") +
  labs(fill='IAT score') +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1.0)) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
  geom_text(data = valuelabels_bystate, 
            aes(long, lat, label = value), 
            size = 4.0) 

ggsave("raceiat_bystate_valuelabels.png", width = 8, height = 5, units = c("in"), dpi = 300)

# Use this to keep latitude & longtitude lines but remove axis titles and text  
#        axis.text.x = element_blank(),
#        axis.text.y = element_blank(),
#        axis.ticks = element_blank(),
#        axis.title.y = element_blank(),
#       axis.title.x = element_blank())


### References -----

# labelling chloropleths in ggplot from https://trinkerrstuff.wordpress.com/2013/07/05/ggplot2-chloropleth-of-supreme-court-decisions-an-tutorial/
