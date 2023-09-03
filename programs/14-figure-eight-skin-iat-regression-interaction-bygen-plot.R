# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: Aug 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT.csv"))

# fixed effects regression

CPS_IAT_secondgen <- CPS_IAT |> 
  filter(SecondGen == 1)

CPS_IAT_thirddgen <- CPS_IAT |> 
  filter(ThirdGen == 1)

reg1 <- list(
  "Second Generation" = feols(Hispanic ~ value*ParentType2 + Female 
                              + MomGradCollege + DadGradCollege + frac_hispanic +
                                Age| region:year, 
                              data = CPS_IAT_secondgen |> filter(ParentType2 != "White-White"), weights = ~weight, vcov = "HC1"),
  
  "Third Generation" = feols(Hispanic ~ value*Grandparent_Type + Female 
                             + MomGradCollege + DadGradCollege + frac_hispanic +
                               Age| region:year, 
                             data = CPS_IAT_thirddgen, weights = ~weight, vcov = "HC1")
)

# plots

#################
# WW grandparents
#################

cm <- c(
  "value:Grandparent_TypeWWHH"      = "Bias x WWHH",
  "value:Grandparent_TypeWWHW"      = "Bias x WWHW",
  "value:Grandparent_TypeWWWH"      = "Bias x WWWH"
) 

text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeWWHH",
                                                 "value:Grandparent_TypeWWHW",
                                                 "value:Grandparent_TypeWWWH"))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x WWHH"
row.names(text_data)[2] = "Bias x WWHW"
row.names(text_data)[3] = "Bias x WWWH"

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
ggsave(paste0(pres_plots,"/skin-iat-regression-interaction-bygen-plot-WW.png"), width = 12, height = 6, units = "in")

#################
# WH grandparents
#################

cm <- c(
  "value:Grandparent_TypeWHHH"      = "Bias x WHHH",
  "value:Grandparent_TypeWHHW"      = "Bias x WHHW",
  "value:Grandparent_TypeWHWH"      = "Bias x WHWH",
  "value:Grandparent_TypeWHWW"      = "Bias x WHWW"
) 
text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeWHHH",
                                                 "value:Grandparent_TypeWHHW",
                                                 "value:Grandparent_TypeWHWH",
                                                 "value:Grandparent_TypeWHWW"
))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x WHHH"
row.names(text_data)[2] = "Bias x WHHW"
row.names(text_data)[3] = "Bias x WHWH"
row.names(text_data)[4] = "Bias x WHWW"

P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "darkblue") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  labs(title = "White Paternal Grandfather and Hispanic Paternal Grandmother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  ) +
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  # labs(title = "White Paternal Grandfather and Hispanic Paternal Grandmother") +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-WH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-WH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-interaction-bygen-plot-WH.png"), width = 12, height = 6, units = "in")

#################
# HW grandparents
#################

cm <- c(
  "value:Grandparent_TypeHWHH"      = "Bias x HWHH",
  "value:Grandparent_TypeHWHW"      = "Bias x HWHW",
  "value:Grandparent_TypeHWWH"      = "Bias x HWWH",
  "value:Grandparent_TypeHWWW"      = "Bias x HWWW"
) 
text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeHWHH",
                                                 "value:Grandparent_TypeHWHW",
                                                 "value:Grandparent_TypeHWWH",
                                                 "value:Grandparent_TypeHWWW"
))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x HWHH"
row.names(text_data)[2] = "Bias x HWHW"
row.names(text_data)[3] = "Bias x HWWH"
row.names(text_data)[4] = "Bias x HWWW"
P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "red") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  # facet_grid(~model) +
  labs(title = "Hispanic Paternal Grandfather and White Paternal GrandMother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  ) +
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  # labs(title = "Hispanic Paternal Grandfather and White Paternal Grandmother") +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-HW.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-HW.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-interaction-bygen-plot-HW.png"), width = 12, height = 6, units = "in")

#################
# HH grandparents
#################

cm <- c(
  "value:Grandparent_TypeHHHW"      = "Bias x HHHW",
  "value:Grandparent_TypeHHWH"      = "Bias x HHWH",
  "value:Grandparent_TypeHHWW"      = "Bias x HHWW"
) 
text_data <- subset(tidy(reg1[[2]]), term %in% c("value:Grandparent_TypeHHHW",
                                                 "value:Grandparent_TypeHHWH",
                                                 "value:Grandparent_TypeHHWW"))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias x HHHW"
row.names(text_data)[2] = "Bias x HHWH"
row.names(text_data)[3] = "Bias x HHWW"
P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

modelplot(reg1[[2]],
          coef_map = cm, color = "darkred") +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  labs(title = "Hispanic Paternal Grandfather and Hispanic Paternal GrandMother") +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  ) +
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  # labs(title = "Hispanic Paternal Grandfather and Hispanic Paternal Grandmother") +
  theme(axis.text = element_text(size = 20)) 
ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-HH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-HH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-interaction-bygen-plot-HH.png"), width = 12, height = 6, units = "in")

