# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# table with different FE

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))

CPS_mean <- CPS_IAT %>% 
  group_by(region, year) %>% 
  filter(!is.na(hwtfinl))  %>% 
  summarise( 
    MeanIndex = weighted.mean(value, w =hwtfinl, na.rm = T))

CPS_mean_state <- CPS_IAT %>% 
  group_by(statefip, year) %>% 
  filter(!is.na(hwtfinl))  %>% 
  summarise( 
    MeanIndex = weighted.mean(value, w =hwtfinl, na.rm = T)) |> 
  mutate(state_name = fips(statefip, to = 'Name'))

CPS_mean_all <- CPS_IAT %>% 
  group_by(year) %>% 
  filter(!is.na(hwtfinl))  %>% 
  summarise( 
    MeanIndex = weighted.mean(value, w =hwtfinl, na.rm = T))


# top and bottom states
result <- CPS_mean_state %>%
  group_by(state_name) %>%
  summarize(avg_bias = mean(MeanIndex, na.rm = TRUE)) %>%
  arrange(avg_bias)

top_2_states <- slice_head(result, n = 2)
bottom_2_states <- slice_tail(result, n = 2)

# Extracting names of top and bottom states
top_state_names <- pull(top_2_states, state_name)
bottom_state_names <- pull(bottom_2_states, state_name)

# Print the state names
print(top_state_names)
print(bottom_state_names)

P9 = unname(createPalette(9,  c("#ff0000", "#00ff00", "#0000ff")))
P56 = unname(createPalette(56,  c("#ff0000", "#00ff00", "#0000ff")))

# by region
p1 <- ggplot(CPS_mean, aes(year, MeanIndex)) +
  geom_point(aes(color = factor(region))) +
  stat_summary(geom = "line", aes(color = factor(region))) +
  labs(x = "Year", y = "Aggregate Bias") +
  ggtitle("Aggregate Implicit Skin Tone Bias Over Time") +
  theme_customs() +
  scale_color_manual(values = P9,
                     labels = c("New England", "M. Atlantic", "E. N. Central",
                                "W. N. Central", "S. Atlantic", "E. S. Central",
                                "W. S. Central", "Mountain", "Pacific"),
                     name = "Region") +
  scale_x_continuous(limits = c(2004,2021))
#scale_y_continuous(limits = c(-0.65,0.65), breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
#theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
p1
ggsave(paste0(figures_wd,"/Bias_byyearbyreagion.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/Bias_byyearbyreagion.png"), width = 10, height = 4, units = "in")

# by state
p2 <- ggplot(CPS_mean_state, aes(year, MeanIndex)) +
  geom_point(aes(color = factor(state_name))) +
  stat_summary(geom = "line", aes(color = factor(state_name))) +
  labs(x = "Year", y = "Aggregate Bias") +
  ggtitle("Aggregate Implicit Skin Tone Bias Over Time") +
  theme_customs() +
  scale_color_manual(values = P56,
                     name = "State") +
  scale_x_continuous(limits = c(2004,2021))
p2

p3 <- ggplot(CPS_mean_all, aes(year, MeanIndex)) +
  geom_point() +
  stat_summary(geom = "line", aes(year, MeanIndex)) +
  labs(x = "Year", y = "Aggregate Bias") +
  ggtitle("Aggregate Implicit Skin Tone Bias Over Time") +
  theme_customs() +
  scale_color_manual(values = P56,
                     name = "State") +
  scale_x_continuous(limits = c(2004,2021))
#scale_y_continuous(limits = c(-0.65,0.65), breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
#theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
p3

p4 <- ggplot(CPS_mean_state, aes(year, MeanIndex)) +
  stat_summary(geom = "line", aes(year, MeanIndex, color= factor(state_name))) +
  stat_summary(geom = "line") +
  labs(x = "Year", y = "Aggregate Bias") +
  ggtitle("Aggregate Implicit Skin Tone Bias Over Time") +
  theme_customs() +
  scale_color_continuous(labels=c('High Program', 'Low Program')) +
  gghighlight(state_name %in% bottom_state_names | state_name %in% top_state_names) + 
  scale_color_manual(values = P56,
                     name = "State") +
  scale_x_continuous(limits = c(2004,2021))
p4

p5 <- ggplot(CPS_mean_state |> filter(state_name %in% bottom_state_names | state_name %in% top_state_names)) +
  stat_summary(geom = "line", aes(year, MeanIndex, color= factor(state_name))) +
  geom_point(aes(year, MeanIndex, shape= factor(state_name))) + 
  labs(x = "Year", y = "Aggregate Bias") +
  # ggtitle("Aggregate Implicit Skin Tone Bias Over Time") +
  theme_customs() +
  scale_color_continuous(labels=c('High Program', 'Low Program')) +
  # gghighlight(state_name == "North Dakota" | state_name == "District of Columbia") + 
  scale_color_manual(values = P56,
                     name = "State") +
  scale_shape_manual(values = c(0:4),name = "State") +
  scale_x_continuous(limits = c(2004,2021), breaks = seq(2004,2021, 4)) +
  scale_y_continuous(limits = c(-0.26,0.43), breaks = seq(-0.26,0.43, 0.06)) +
  theme(axis.text = element_text(size = 12),
        axis.title= element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
p5
ggsave(paste0(figures_wd,"/Bias_twostates.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/Bias_twostates.png"), width = 10, height = 4, units = "in")

CPS_mean_state_his <- CPS_IAT %>% 
  group_by(statefip, year) %>% 
  filter(!is.na(hwtfinl))  %>% 
  summarise( 
    MeanAsian = weighted.mean(Asian, w =hwtfinl, na.rm = T)) |> 
  mutate(state_name = fips(statefip, to = 'Name'))

p6 <- ggplot(CPS_mean_state_his |> filter(state_name %in% bottom_state_names | state_name %in% top_state_names), aes(year, MeanAsian)) +
  stat_summary(geom = "line", aes(year, MeanAsian, color= factor(state_name))) +
  geom_point(aes(year, MeanAsian, shape= factor(state_name))) +
  labs(x = "Year", y = "Self-reported Asian Identity") +
  # ggtitle("Self-reported Asian Identity Over Time") +
  theme_customs() +
  scale_color_continuous(labels=c('High Program', 'Low Program')) +
  scale_color_manual(values = P56,
                     name = "State") +
  scale_shape_manual(values = c(0:4),
                     name = "State") +
  scale_x_continuous(limits = c(2004,2021), breaks = seq(2004,2021, 4)) +
  scale_y_continuous(limits = c(0.3,1), breaks = seq(0.3,1, 0.1)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x= element_text(size = 20),
        axis.title.y= element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
p6
ggsave(paste0(figures_wd,"/Bias_twostates-asian.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/Bias_twostates-asian.png"), width = 10, height = 4, units = "in")

# Bubble Graph
CPS_mean_reg <- CPS_IAT |> 
  filter(!is.na(hwtfinl))  %>% 
  group_by(statefip, year) %>% 
  summarise( 
    MeanAsian = weighted.mean(Asian, w =hwtfinl, na.rm = T),
    MeanIndex = weighted.mean(value, w =hwtfinl, na.rm = T)) |> 
  mutate(state_name = fips(statefip, to = 'Name'))

CPS_mean_reg <- CPS_mean_reg |> 
  filter(state_name %in% bottom_state_names | state_name %in% top_state_names)

ggplot(CPS_mean_reg, aes(year, MeanAsian)) +
  geom_point(aes(color = factor(state_name), size = MeanIndex)) +
  # geom_smooth(aes(color = factor(region)))+
  scale_color_manual(values = P56, labels = c("California", "Hawaii", "Mississippi", "West Virginia"),
                     name = "State and Districts") +
  stat_summary(geom = 'line', aes(color = factor(state_name))) +
  labs(x = "Year", y = "Percent Self-reported Asian Identity") +
  scale_size(range = c(1, 10), name="Bias") +
  theme_customs() +
  ggtitle("Self-Reported Asian Identity in the US \nOver Time: By Least and Most Biased Regions") +
  scale_x_continuous(limits = c(2004, 2021), breaks = seq(2004, 2021, 5))
ggsave(paste0(figures_wd,"/Asians-bubble.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/Asians-bubble.png"), width = 10, height = 4, units = "in")

rm(CPS_index)
