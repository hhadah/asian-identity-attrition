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
# f-tests

# linearHypothesis(reg1[[2]], test = "F", c("value:Grandparent_TypeWWWH  = 0", 
#                                           "value:Grandparent_TypeWWHW  = 0",
#                                           "value:Grandparent_TypeWWHH  = 0"))
# linearHypothesis(reg1[[2]], test = "F", c("value:Grandparent_TypeHWWH  = 0", 
#                                          "value:Grandparent_TypeHWHW  = 0",
#                                          "value:Grandparent_TypeHWHW  = 0",
#                                          "value:Grandparent_TypeHWHH = 0"))
# linearHypothesis(reg1[[2]], test = "F", "value:Grandparent_TypeHWWW + 
#                                          value:Grandparent_TypeHWWH +  
#                                          value:Grandparent_TypeHWHW +  
#                                          value:Grandparent_TypeHWHH = 0")
# linearHypothesis(reg1[[2]], test = "F", "value:Grandparent_TypeWHWW + 
#                                          value:Grandparent_TypeWHWH +  
#                                          value:Grandparent_TypeWHHW +  
#                                          value:Grandparent_TypeWHHH = 0")
# linearHypothesis(reg1[[2]], test = "F", "value:Grandparent_TypeWHWW + 
#                                          value:Grandparent_TypeWHWH +  
#                                          value:Grandparent_TypeWHHW +  
#                                          value:Grandparent_TypeWHHH =
#                                          value:Grandparent_TypeWWWH +
#                                          value:Grandparent_TypeWWHW +
#                                          value:Grandparent_TypeWWHH  ")

# plots
cm <- c("value:ParentType2Hispanic-White" = "Bias x Hispanic-White Parent",
        "value:ParentType2White-Hispanic" = "Bias x White-Hispanic Parent",
        "value:Grandparent_TypeHHHW"      = "Bias x HHHW",
        "value:Grandparent_TypeHHWH"      = "Bias x HHWH",
        "value:Grandparent_TypeHHWW"      = "Bias x HHWW",
        "value:Grandparent_TypeHWHH"      = "Bias x HWHH",
        "value:Grandparent_TypeHWHW"      = "Bias x HWHW",
        "value:Grandparent_TypeHWWH"      = "Bias x HWWH",
        "value:Grandparent_TypeHWWW"      = "Bias x HWWW",
        "value:Grandparent_TypeWHHH"      = "Bias x WHHH",
        "value:Grandparent_TypeWHHW"      = "Bias x WHHW",
        "value:Grandparent_TypeWHWH"      = "Bias x WHWH",
        "value:Grandparent_TypeWHWW"      = "Bias x WHWW",
        "value:Grandparent_TypeWWHH"      = "Bias x WWHH",
        "value:Grandparent_TypeWWHW"      = "Bias x WWHW",
        "value:Grandparent_TypeWWWH"      = "Bias x WWWH",
        "value" = "Bias",
        "ParentType2Hispanic-White" = "Hispanic-White Parent",
        "ParentType2White-Hispanic" = "White-Hispanic Parent"#,
        # "Female" = "Female",
        # "MomGradCollege" = "College Graduate: Mother",
        # "DadGradCollege" = "College Graduate: Father",
        # "lnftotval_mom" = "Log Total Family Income"
        #"age" = "Age",
        #"HH" = "Both parents Hispanic",
        # "FirstGen" = "First Gen",
        # "SecondGen" = "Second Gen",
        # "ThirdGen" = "Third Generation"
) 

P2 = unname(createPalette(2,  c("#ff0000", "#00ff00", "#0000ff")))

# cols <- c("Second Generation" = "red", "Third Generation" = "darkgreen")
text_data <- subset(tidy(reg1[[1]]), term %in% c("value:ParentType2Hispanic-White",
                                                 "value:ParentType2White-Hispanic" ,
                                                 "value",
                                                 "ParentType2Hispanic-White",
                                                 "ParentType2White-Hispanic" ))
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Hispanic-White Parent"
row.names(text_data)[3] = "White-Hispanic Parent"
row.names(text_data)[4] = "Bias x Hispanic-White Parent"
row.names(text_data)[5] = "Bias x White-Hispanic Parent"

modelplot(reg1[[1]],
             coef_map = cm, color = 'red',
             title = "Subjective Hispanic identity and Skin Tone Implicit Bias: Second Generation") +
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
  ggsave(paste0(pres_plots,"/skin-iat-regression-interaction-bygen-plot-second.png"), width = 12, height = 6, units = "in")

  # scale_color_manual(values = wes_palette('Darjeeling1'))
  # scale_color_paletteer_d("colorblindr::OkabeIto_black")
  # scale_color_manual(values = P2)
  
  modelplot(reg1[[2]],
            coef_map = cm, color = "darkgreen",
            title = "Subjective Hispanic identity and Skin Tone Implicit Bias: Third Generation") +
    # facet_grid(~model) +
    geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
    theme_customs() +
    theme(
      strip.background = element_rect(
        color="black", fill="white", size=1.5
      )
    ) +
    # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
    labs(title = "Subjective Hispanic identity and Skin Tone Implicit Bias: Third Generation") +
    theme(axis.text = element_text(size = 20))   
  ggsave(paste0(figures_wd,"/skin-iat-regression-interaction-bygen-plot-third.png"), width = 10, height = 4, units = "in")
  ggsave(paste0(thesis_plots,"/skin-iat-regression-interaction-bygen-plot-third.png"), width = 10, height = 4, units = "in")
  ggsave(paste0(pres_plots,"/skin-iat-regression-interaction-bygen-plot-third.png"), width = 12, height = 6, units = "in")