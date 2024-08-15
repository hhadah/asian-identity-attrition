library(ggspatial) # scale bars and north arrows
library(ggmap)

# Import data with geographic variables from Github
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))

# Remove people who don't report their state 
CPS_IAT <- CPS_IAT[CPS_IAT$state != "",]

skin_grouped_bystate <- CPS_IAT %>% 
  group_by(state, year) %>% 
  summarise(value = mean(lw_index, na.rm = TRUE)) %>% 
  select(state,
         year,
         value)

skin_grouped_bystate |>
  group_by(state) |> 
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(state == "Texas")
### Get lat & long info -----

# this "states" dataframe has the lat & long info needed for mapping.
states <- st_as_sf(map('state', plot = TRUE, fill = TRUE))
# states <- map_data("state")
states <- states %>% 
  rename(state = ID)

# change state to title case
states$state  <- str_to_title(states$state)
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

for (year_map in seq(2004,2020, 4)) {
  map <- ggplot() + geom_sf(data = skin_grouped_bystate |> filter(year == year_map), 
                            aes(fill = value), 
                            color = "white")+
    geom_sf(data = skin_grouped_bystate, 
            color = 'white', 
            fill = NA,
            size = 0.01) +
    geom_sf(data = DIVISION, 
            color = 'black', 
            fill = NA,
            lwd = 1.0) +
    scale_fill_distiller(palette = "Spectral", guide = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 20, barheight = 1)) +
    labs(fill = "Bias (Low to High)") +
    theme_customs_map()  +
    # annotation_scale(location = "bl", width_hint = 0.4) +
    # annotation_north_arrow(location = "bl", which_north = "true", 
    #                        pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12))
  map
  ggsave(path = figures_wd, filename = paste0(year_map,"skinmap.png"))
  ggsave(path = thesis_plots, 
         filename = paste0(year_map,"skinmap.png"), width = 10, height = 5, 
         units = c("in"))
  
}
# Create a single map with the averaged data
map <- ggplot() + 
  geom_sf(data = skin_grouped_bystate, aes(fill = value), color = "white") +
  geom_sf(data = skin_grouped_bystate, color = 'white', fill = NA, size = 0.01) +
  geom_sf(data = DIVISION, color = 'black', fill = NA, lwd = 1.0) +
  scale_fill_distiller(palette = "Spectral", guide = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 20, barheight = 1)) +
    labs(fill = "Bias (Low to High)") +
  theme_customs_map() +
  theme(legend.position = "bottom", legend.text = element_text(size = 12))

# Save the map
ggsave(path = figures_wd, filename = "Average_Skinmap.png")
ggsave(path = thesis_plots, filename = "Average_Skinmap.png", width = 10, height = 5, units = "in")
