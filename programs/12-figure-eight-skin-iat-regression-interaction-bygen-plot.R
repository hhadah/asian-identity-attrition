# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: Aug 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index)

# fixed effects regression

CPS_IAT_secondgen <- CPS_IAT |> 
  filter(SecondGen_Asian == 1)

CPS_IAT_thirddgen <- CPS_IAT |> 
  filter(ThirdGen_Asian == 1)

reg1 <- list(
  "Second Generation" = feols(Asian ~ value*ParentType2 + Female 
                              + MomGradCollege + DadGradCollege + frac_asian +
                                Age| region:year, 
                              data = CPS_IAT_secondgen |> filter(ParentType2 != "White-White"), weights = ~weight, vcov = "HC1"),
  
  "Third Generation" = feols(Asian ~ value*Grandparent_Type + Female 
                             + MomGradCollege + DadGradCollege + frac_asian +
                               Age| region:year, 
                             data = CPS_IAT_thirddgen, weights = ~weight, vcov = "HC1")
)

# plots

#################
# WW grandparents
#################

cm <- c(
  "value:Grandparent_TypeWWAA"      = "Bias x WWAA",
  "value:Grandparent_TypeWWAW"      = "Bias x WWAW",
  "value:Grandparent_TypeWWWA"      = "Bias x WWWA"
) 

text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeWWAA",
                                                 "value:Grandparent_TypeWWAW",
                                                 "value:Grandparent_TypeWWWA"))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x WWAA"
row.names(text_data)[2] = "Bias x WWAW"
row.names(text_data)[3] = "Bias x WWWA"

P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "darkgreen") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  labs(title = "White Paternal Grandfather and White Paternal Grandmother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  # labs(title = "White Paternal Grandfather and White Paternal Grandmother") +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-WW.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-WW.png"), width = 10, height = 4, units = "in")

#################
# WH grandparents
#################

cm <- c(
  "value:Grandparent_TypeWAAA"      = "Bias x WAAA",
  "value:Grandparent_TypeWAAW"      = "Bias x WAAW",
  "value:Grandparent_TypeWAWA"      = "Bias x WAWA",
  "value:Grandparent_TypeWAWW"      = "Bias x WAWW"
) 
text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeWAAA",
                                                 "value:Grandparent_TypeWAAW",
                                                 "value:Grandparent_TypeWAWA",
                                                 "value:Grandparent_TypeWAWW"
))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x WAAA"
row.names(text_data)[2] = "Bias x WAAW"
row.names(text_data)[3] = "Bias x WAWA"
row.names(text_data)[4] = "Bias x WAWW"

P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "darkblue") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  labs(title = "White Paternal Grandfather and Asian Paternal Grandmother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  ) +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-WH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-WH.png"), width = 10, height = 4, units = "in")

#################
# HW grandparents
#################

cm <- c(
  "value:Grandparent_TypeAWAA"      = "Bias x AWAA",
  "value:Grandparent_TypeAWAW"      = "Bias x AWAW",
  "value:Grandparent_TypeAWWA"      = "Bias x AWWA",
  "value:Grandparent_TypeAWWW"      = "Bias x AWWW"
) 
text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeAWAA",
                                                 "value:Grandparent_TypeAWAW",
                                                 "value:Grandparent_TypeAWWA",
                                                 "value:Grandparent_TypeAWWW"
))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x AWAA"
row.names(text_data)[2] = "Bias x AWAW"
row.names(text_data)[3] = "Bias x AWWA"
row.names(text_data)[4] = "Bias x AWWW"
P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "red") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  # facet_grid(~model) +
  labs(title = "Asian Paternal Grandfather and White Paternal GrandMother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  ) +
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  # labs(title = "Asian Paternal Grandfather and White Paternal Grandmother") +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-HW.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-HW.png"), width = 10, height = 4, units = "in")

#################
# HH grandparents
#################

cm <- c(
  "value:Grandparent_TypeAAAW"      = "Bias x AAAW",
  "value:Grandparent_TypeAAWA"      = "Bias x AAWA",
  "value:Grandparent_TypeAAWW"      = "Bias x AAWW"
) 
text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeAAAW",
                                                 "value:Grandparent_TypeAAWA",
                                                 "value:Grandparent_TypeAAWW"))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x AAAW"
row.names(text_data)[2] = "Bias x AAWA"
row.names(text_data)[3] = "Bias x AAWW"
P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "darkred") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  labs(title = "Asian Paternal Grandfather and Asian Paternal GrandMother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  ) +
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  # labs(title = "Asian Paternal Grandfather and Asian Paternal Grandmother") +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-HH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-HH.png"), width = 10, height = 4, units = "in")

