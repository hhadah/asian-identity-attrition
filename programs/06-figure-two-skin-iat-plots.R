# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# table with different FE

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))

# This a script to
# run a regression
# with cps and 
# project implicit

CPS_mean_state <- CPS_IAT %>% 
  group_by(statefip, Join_year) %>% 
  filter(!is.na(hwtfinl))  %>% 
  summarise( 
    MeanIndex = weighted.mean(lw_index, w =hwtfinl, na.rm = T)) |> 
  mutate(state_name = fips(statefip, to = 'Name'))

P9 = unname(createPalette(9,  c("#ff0000", "#00ff00", "#0000ff")))
P56 = unname(createPalette(56,  c("#ff0000", "#00ff00", "#0000ff")))

# Filter the data for the year 2020
data_2020 <- CPS_mean_state %>%
  filter(Join_year == 2016)

# Calculate mean bias for each state in 2020
state_means_2020 <- CPS_mean_state %>%
  group_by(state_name) %>%
  summarize(mean_bias = mean(MeanIndex, na.rm = TRUE))

# Identify the top two and bottom two states based on mean bias in 2020
top_two_states <- state_means_2020 %>%
  top_n(2, mean_bias) %>%
  pull(state_name)

bottom_two_states <- state_means_2020 %>%
  top_n(-2, mean_bias) %>%
  pull(state_name)

selected_states <- c(top_two_states, bottom_two_states)

# Filter the original data to include only the selected states
filtered_data <- CPS_mean_state %>%
  filter(state_name %in% selected_states)

# Get the minimum and maximum values for the x and y axes
min_year <- min(filtered_data$Join_year, na.rm = TRUE)
max_year <- 2021
min_y <- round(min(filtered_data$MeanIndex, na.rm = TRUE), 2)
max_y <- round(max(filtered_data$MeanIndex, na.rm = TRUE), 2)

# Plot the data
p5 <- ggplot(filtered_data) +
  stat_summary(aes(x = Join_year, y = MeanIndex, color = factor(state_name)), geom = "line") +
  geom_point(aes(x = Join_year, y = MeanIndex, shape = factor(state_name))) + 
  labs(x = "Year", y = "Aggregate Bias") +
  # ggtitle("Aggregate Implicit Skin Tone Bias Over Time") +
  theme_customs() +
  scale_color_continuous(labels = c('High Program', 'Low Program')) +
  # gghighlight(state_name == "North Dakota" | state_name == "District of Columbia") + 
  scale_color_manual(values = P56, name = "State") +
  scale_shape_manual(values = c(0:4), name = "State") +
  scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, 4)) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = seq(min_y, max_y, 0.2)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.line = element_line(colour = "black")) 

p5

ggsave(paste0(figures_wd,"/Bias_twostates.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/Bias_twostates.png"), width = 10, height = 4, units = "in")

CPS_mean_state_his <- CPS_IAT %>% 
  group_by(statefip, Join_year) %>% 
  filter(!is.na(hwtfinl))  %>% 
  summarise( 
    MeanAsian = weighted.mean(Asian, w =hwtfinl, na.rm = T)) |> 
  mutate(state_name = fips(statefip, to = 'Name'))

CPS_mean_state_his |> 
  print(n = Inf)
min_y <- round(min(CPS_mean_state_his$MeanAsian, na.rm = TRUE), 2)
max_y <- round(max(CPS_mean_state_his$MeanAsian, na.rm = TRUE), 2)

p6 <- ggplot(CPS_mean_state_his |> filter(state_name == "North Dakota" | state_name == "District of Columbia" |
                                          state_name == "South Dakota"| state_name == "Vermont"), aes(Join_year, MeanAsian)) +
  stat_summary(geom = "line", aes(Join_year, MeanAsian, color= factor(state_name))) +
  geom_point(aes(Join_year, MeanAsian, shape= factor(state_name))) +
  labs(x = "Year", y = "Self-reported Asian Identity") +
  # ggtitle("Self-reported Asian Identity Over Time") +
  theme_customs() +
  scale_color_continuous(labels=c('High Program', 'Low Program')) +
  scale_color_manual(values = P56,
                     name = "State") +
  scale_shape_manual(values = c(0:4),
                     name = "State") +
  scale_x_continuous(limits = c(2004,2021), breaks = seq(2004,2021, 4)) +
  scale_y_continuous(limits = c(0,max_y), breaks = seq(0,max_y, 0.2)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x= element_text(size = 20),
        axis.title.y= element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.line = element_line(colour = "black")) 
p6
ggsave(paste0(figures_wd,"/Bias_twostates-asian.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/Bias_twostates-asian.png"), width = 10, height = 4, units = "in")
