library(ggspatial) # scale bars and north arrows
library(ggmap)

# Import data with geographic variables from Github
Skin_IAT <- read.csv(file.path(datasets,"Asia_IAT_Clean.csv")) |> 
  mutate(Implicit=(Implicit-mean(Implicit, na.rm = T))/SD(Implicit, na.rm = T))

# this .csv contains all state abbrevs, state nos., and lowercase state names
state_info <- read.csv(file.path(Implicit_Race_Harvard,"state_info.csv"))

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
map <- ggplot() + geom_sf(data = skin_grouped_bystate |> filter(year == 2004), 
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
  ggsave(path = thesis_plots, 
         filename = paste0(year_map,"skinmap.png"), width = 8, height = 5, 
         units = c("in"))
  
}

