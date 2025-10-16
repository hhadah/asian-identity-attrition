# Plot of what Asians
# identify as in terms
# of race

# date: September 7th, 2025

CPS <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))

CPS |> 
    glimpse()

#----------------------------------------------
# histogram of identities
#----------------------------------------------
# Function to categorize race codes with Asian focus
categorize_race_asian_focus <- function(race_code) {
  case_when(
    race_code %in% c(651) ~ "Asian Only",
    race_code %in% c(650, 652) ~ "Asian/Pacific Islander", # Merge broader Asian categories
    race_code %in% c(803, 804, 809, 813) ~ "Asian + White/Pacific Islander", # Common Asian multiracial
    race_code %in% c(806, 808, 811, 812, 814, 815, 817, 818, 819) ~ "Asian + Other Races", # Less common combinations
    race_code %in% c(100) ~ "White",
    race_code %in% c(200, 300, 700, 801, 802, 805, 807, 810, 816, 820, 830, 999) ~ "Other/Non-Asian Multiracial",
    TRUE ~ "Other"
  )
}

# Create race code labels mapping
race_labels <- c(
  "100" = "White",
  "200" = "Black", 
  "300" = "American Indian/Aleut/Eskimo",
  "650" = "Asian or Pacific Islander",
  "651" = "Asian only",
  "652" = "Hawaiian/Pacific Islander only",
  "700" = "Other (single) race, n.e.c.",
  "801" = "White-Black",
  "802" = "White-American Indian", 
  "803" = "White-Asian",
  "804" = "White-Hawaiian/Pacific Islander",
  "805" = "Black-American Indian",
  "806" = "Black-Asian",
  "807" = "Black-Hawaiian/Pacific Islander", 
  "808" = "American Indian-Asian",
  "809" = "Asian-Hawaiian/Pacific Islander",
  "810" = "White-Black-American Indian",
  "811" = "White-Black-Asian",
  "812" = "White-American Indian-Asian",
  "813" = "White-Asian-Hawaiian/Pacific Islander", 
  "814" = "White-Black-American Indian-Asian",
  "815" = "American Indian-Hawaiian/Pacific Islander",
  "816" = "White-Black-American Indian-Hawaiian/Pacific Islander",
  "817" = "White-American Indian-Asian-Hawaiian/Pacific Islander",
  "818" = "Black-American Indian-Asian",
  "819" = "White-American Indian-Asian-Hawaiian/Pacific Islander",
  "820" = "Two or three races, unspecified",
  "830" = "Four or five races, unspecified", 
  "999" = "Blank"
)

#----------------------------------------------
# histogram of identities: All Asian Americans
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_aggregated.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_aggregated.png"), width = 10, height = 6, units = "in")

# Print the aggregated summary
print(asian_american_race)

#----------------------------------------------
# histogram of identities: First generation only
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(FirstGen_Asian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_firstgen.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_firstgen.png"), width = 10, height = 6, units = "in")

# Print the aggregated summary
print(asian_american_race)

#----------------------------------------------
# histogram of identities: Second generation only
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(SecondGen_Asian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_secondgen.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_secondgen.png"), width = 10, height = 6, units = "in")

# Print the aggregated summary
print(asian_american_race)

#----------------------------------------------
# histogram of identities: Second generation only
# Asian Fathers and Mothers
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(SecondGen_Asian == 1 & AA_0bj == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_secondgen_AA.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_secondgen_AA.png"), width = 10, height = 6, units = "in")

#----------------------------------------------
# histogram of identities: Second generation only
# Asian Fahers and White Mothers
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(SecondGen_Asian == 1 & AW_0bj == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_secondgen_AW.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_secondgen_AW.png"), width = 10, height = 6, units = "in")

#----------------------------------------------
# histogram of identities: Second generation only
# White Fathers and Asian Mothers
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(SecondGen_Asian == 1 & WA_0bj == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_secondgen_WA.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_secondgen_WA.png"), width = 10, height = 6, units = "in")

#----------------------------------------------
# histogram of identities: Third generation only
#----------------------------------------------

# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(ThirdGen_Asian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_thirdgen.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_thirdgen.png"), width = 10, height = 6, units = "in")

#----------------------------------------------
# histogram of identities: Third generation only
# by number of Asian grandparents
#----------------------------------------------
CPS <- CPS |> 
  mutate(OneAsian = case_when(Grandparent_Type == "AWWW" ~ 1,
                                 Grandparent_Type == "WAWW" ~ 1,
                                 Grandparent_Type == "WWAW" ~ 1,
                                 Grandparent_Type == "WWWA" ~ 1,
                                 TRUE ~ 0),
         TwoAsian = case_when(Grandparent_Type == "AAWW" ~ 1,
                                 Grandparent_Type == "AWAW" ~ 1,
                                 Grandparent_Type == "AWWA" ~ 1,
                                 Grandparent_Type == "WAAW" ~ 1,
                                 Grandparent_Type == "WWAA" ~ 1,
                                 Grandparent_Type == "WAWA" ~ 1,
                                 TRUE ~ 0),
         ThreeAsian = case_when(Grandparent_Type == "AAAW" ~ 1,
                                   Grandparent_Type == "AAWA" ~ 1,
                                   Grandparent_Type == "AWAA" ~ 1,
                                   Grandparent_Type == "WAAA" ~ 1,
                                 TRUE ~ 0),
         FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1,
                                 TRUE ~ 0))
 #----------------------------------------------
# histogram of identities: Third generation only
# One Asian grandparents
#----------------------------------------------
# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(ThirdGen_Asian == 1 & OneAsian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_thirdgen_oneasiangran.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_thirdgen_oneasiangran.png"), width = 10, height = 6, units = "in")

 #----------------------------------------------
# histogram of identities: Third generation only
# Two Asian grandparents
#----------------------------------------------
# Filter for Asian Americans and create race distribution - AGGREGATED BY BROADER CATEGORIES
asian_american_race <- CPS |>
  filter(ThirdGen_Asian == 1 & TwoAsian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_thirdgen_twoasiangran.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_thirdgen_twoasiangran.png"), width = 10, height = 6, units = "in")

#----------------------------------------------
# histogram of identities: Third generation only
# Three Asian grandparents
#----------------------------------------------
asian_american_race <- CPS |>
  filter(ThirdGen_Asian == 1 & ThreeAsian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_thirdgen_threeasiangran.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_thirdgen_threeasiangran.png"), width = 10, height = 6, units = "in")

#----------------------------------------------
# histogram of identities: Third generation only
# Four Asian grandparents
#----------------------------------------------
asian_american_race <- CPS |>
  filter(ThirdGen_Asian == 1 & FourAsian == 1) |>
  count(race) |>
  mutate(
    race_label = race_labels[as.character(race)],
    race_category = categorize_race_asian_focus(race)  # Using the Asian-focused function
  ) |>
  # Group by the broader categories and sum the counts
  group_by(race_category) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    percentage = n / sum(n) * 100
  ) |>
  arrange(desc(n))

# Create the simplified plot with just the broader categories
ggplot(asian_american_race, aes(x = reorder(race_category, n), y = percentage)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    x = "Race Category",
    y = "Percentage"
  ) +
  theme_customs() +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(paste0(figures_wd,"/histogram_asian_american_race_thirdgen_fourasiangran.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/histogram_asian_american_race_thirdgen_fourasiangran.png"), width = 10, height = 6, units = "in")
