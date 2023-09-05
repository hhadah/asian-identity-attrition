# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: Aug 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv"))

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
cm <- c("value:ParentType2Asian-White" = "Bias x Asian-White Parent",
        "value:ParentType2White-Asian" = "Bias x White-Asian Parent",
        "value:Grandparent_TypeAAAW"      = "Bias x AAAW",
        "value:Grandparent_TypeAAWA"      = "Bias x AAWA",
        "value:Grandparent_TypeAAWW"      = "Bias x AAWW",
        "value:Grandparent_TypeAWAA"      = "Bias x AWAA",
        "value:Grandparent_TypeAWAW"      = "Bias x AWAW",
        "value:Grandparent_TypeAWWA"      = "Bias x AWWA",
        "value:Grandparent_TypeAWWW"      = "Bias x AWWW",
        "value:Grandparent_TypeWAAA"      = "Bias x WAAA",
        "value:Grandparent_TypeWAAW"      = "Bias x WAAW",
        "value:Grandparent_TypeWAWA"      = "Bias x WAWA",
        "value:Grandparent_TypeWAWW"      = "Bias x WAWW",
        "value:Grandparent_TypeWWAA"      = "Bias x WWAA",
        "value:Grandparent_TypeWWAW"      = "Bias x WWAW",
        "value:Grandparent_TypeWWWA"      = "Bias x WWWA",
        "value" = "Bias",
        "ParentType2Asian-White" = "Asian-White Parent",
        "ParentType2White-Asian" = "White-Asian Parent"
) 

P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

# cols <- c("Second Generation" = "red", "Third Generation" = "darkgreen")
text_data <- subset(tidy(reg1[[1]]), term %in% c("value:ParentType2Asian-White",
                                                 "value:ParentType2White-Asian" ,
                                                 "value",
                                                 "ParentType2Asian-White",
                                                 "ParentType2White-Asian" ))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Asian-White Parent"
row.names(text_data)[3] = "White-Asian Parent"
row.names(text_data)[4] = "Bias x Asian-White Parent"
row.names(text_data)[5] = "Bias x White-Asian Parent"

modelplot(reg1[[1]],
             coef_map = cm, color = 'red',
             title = "Subjective Asian identity and Skin Tone Implicit Bias: Second Generation") +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15) 
  )

  ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-second.png"), width = 10, height = 4, units = "in")
  ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-second.png"), width = 10, height = 4, units = "in")

  
  modelplot(reg1[[2]],
            coef_map = cm, color = "darkgreen",
            title = "Subjective Asian identity and Skin Tone Implicit Bias: Third Generation") +
    # facet_grid(~model) +
    geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
    theme_customs() +
    theme(
      strip.background = element_rect(
        color="black", fill="white", size=1.5
      )
    ) +
    # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
    labs(title = "Subjective Asian identity and Skin Tone Implicit Bias: Third Generation") +
    theme(axis.text = element_text(size = 20))   
  ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-third.png"), width = 10, height = 4, units = "in")
  ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-third.png"), width = 10, height = 4, units = "in")
