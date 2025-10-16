# This a script to 
# merge the skin IAT
# and the Current
# Population Survey (CPS)

# Date: Sep 23rd, 2022

### Open iat
### state by year
### prejeduice index


CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  group_by(statefip, year) |>
  summarise(MeanAsian = mean(frac_asian, na.rm = T),
            lw_index  = mean(lw_index, na.rm = T)) |>
  rename(value = lw_index)

# Plot Asians

ggplot(CPS_IAT |> filter(year>=2015), aes(value, MeanAsian, frame = year)) +
  geom_point() +
  geom_label_repel(
    data = CPS_IAT |> filter(year== 2021 & statefip %in% c(35, 48, 6, 33, 46, 25, 4, 24, 15, 12, 55, 11, 44)),
    aes(value, MeanAsian, label = fips(statefip, to = "Abbreviation")),
    nudge_x = 0.005, nudge_y = 0.005,
    #check_overlap = T
  ) +
  labs(#title = "Scatter Plot of Percent Asian in a State on Bias: Year \u2265 2015",
       x = "Bias",
       y = "Percent Asian") +
  theme_customs()# +
  # transition_time(year) +
  # ease_aes('linear')
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
ggsave(file.path(figures_wd,"scatter-plot-bias-Asian-great2015.png"), width = 10, height = 6, units = "in")
ggsave(file.path(thesis_plots,"scatter-plot-bias-Asian-great2015.png"), width = 10, height = 6, units = "in")


# less than 2015

ggplot(CPS_IAT |> filter(year<2015), aes(value, MeanAsian, frame = year)) +
  geom_point() +
  geom_label_repel(
    data = CPS_IAT |> filter(year== 2008 &  statefip %in% c(35, 48, 6, 33, 46, 25, 4, 24, 15, 12, 55, 11, 44)),
    aes(value, MeanAsian, label = fips(statefip, to = "Abbreviation")),
    nudge_x = 0.005, nudge_y = 0.005,
    #check_overlap = T
  ) +
  labs(#title = "Scatter Plot of Percent Asian in a State on Bias: Year \u003c 2015",
       x = "Bias",
       y = "Percent cAsian") +
  theme_customs()# +
# transition_time(year) +
# ease_aes('linear')
# scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
ggsave(file.path(figures_wd,"scatter-plot-bias-Asian-less2015.png"), width = 10, height = 6, units = "in")
ggsave(file.path(thesis_plots,"scatter-plot-bias-Asian-less2015.png"), width = 10, height = 6, units = "in")

