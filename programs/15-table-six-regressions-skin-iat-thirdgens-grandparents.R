# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT.csv")) |> 
  mutate(OneHispanic = case_when(Grandparent_Type == "HWWW" ~ 1,
                                 Grandparent_Type == "WHWW" ~ 1,
                                 Grandparent_Type == "WWHW" ~ 1,
                                 Grandparent_Type == "WWWH" ~ 1,
                                 TRUE ~ 0),
         TwoHispanic = case_when(Grandparent_Type == "HHWW" ~ 1,
                                 Grandparent_Type == "HWHW" ~ 1,
                                 Grandparent_Type == "HWWH" ~ 1,
                                 Grandparent_Type == "WHHW" ~ 1,
                                 Grandparent_Type == "WWHH" ~ 1,
                                 Grandparent_Type == "WHWH" ~ 1,
                                 TRUE ~ 0),
         ThreeHispanic = case_when(Grandparent_Type == "HHHW" ~ 1,
                                   Grandparent_Type == "HHWH" ~ 1,
                                   Grandparent_Type == "HWHH" ~ 1,
                                   Grandparent_Type == "WHHH" ~ 1,
                                 TRUE ~ 0),
         FourHispanic = case_when(Grandparent_Type == "HHHH" ~ 1,
                                 TRUE ~ 0))


# fixed effects regression

reg1 <- list(
  "\\specialcell{(1) \\\\ One}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(ThirdGen == 1 & OneHispanic == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ Two}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad| region:year, 
                     data = CPS_IAT |> filter(ThirdGen == 1 & TwoHispanic == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ Three}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(ThirdGen == 1 & ThreeHispanic == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ Four}" = feols(Hispanic ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_hispanic +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(ThirdGen == 1 & FourHispanic == 1), weights = ~weight, vcov = ~statefip)
)


cm <- c("value" = "Bias",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom" = "Log Total Family Income"
        #"age" = "Age",
        #"HH" = "Both parents Hispanic",
        # "FirstGen" = "First Gen",
        # "SecondGen" = "Second Gen",
        # "ThirdGen" = "Third Generation"
) 

f1 <- function(x) format(round(x, 2), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: region", clean = "Region FE", fmt = 0),
  list(raw = "FE: year", clean = "Year FE", fmt = 0),
  list(raw = "FE: region:year", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

options(modelsummary_format_numeric_latex = "plain")
regression_tab <- modelsummary(reg1, fmt = f1,  
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Relationship Between Bias and Self-Reported Hispanic identity Among Third-Generation Hispanic Immigrants: By Grandparental Type \\label{regtab-bygrandparents}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{Each column is an estimation of equation (\\\\ref{eq:identity_reg_bias}) restricted to third-generation Hispanic immigrants by 
                      number of Hispanic grandparents with region × year fixed effects. 
                      I include controls for sex, quartic age, fraction of Hispanics in a state, and parental education.
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{The samples include third-generation Hispanic children ages 17 and below who live in intact families. 
                      Native-born third-generation Hispanic 
                      immigrant children with at least one grandparent born in a Spanish-speaking 
                      country.}",
                      "\\\\footnotesize{Data source is the 2004-2021 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T
  ) |> 
  add_header_above(c(" " = 1, "Number of Hispanic Grandparents" = 4
  ))
regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab45-iat_regression_tab_bygrandparent_type.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab45-iat_regression_tab_bygrandparent_type.tex"))

# dissertation table
regression_tab <- modelsummary(reg1, fmt = 2,  
                               output = "latex", 
                               coef_map = cm,
                               gof_map = gm,
                               escape = F,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "HOLD_position")
  ) %>%
  add_header_above(c(" " = 1, "Number of Hispanic Grandparents" = 4
  ))
regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(dissertation_wd,"tables/tab45-iat_regression_tab_bygrandparent_type.tex"))


### Plots
cm <- c("value" = "Bias") 
# One Hispanic 
modelplot(reg1[[1]],
          conf_level = 0.9,
          coef_map = cm, color = 'red',
          title = "One Hispanic Grandparent") +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15) 
  )+ 
  labs(title = "One Hispanic Grandparent") + theme(plot.title = element_text(size = 20))

ggsave(paste0(figures_wd,"/skin-iat-regression-thirdgen-oneH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-thirdgen-oneH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-thirdgen-oneH.png"), width = 12, height = 6, units = "in")

# Two Hispanic 
modelplot(reg1[[2]],
          conf_level = 0.9,
          coef_map = cm, color = 'red',
          title = "Two Hispanic Grandparents") +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15) 
  )+ 
  labs(title = "Two Hispanic Grandparents") + theme(plot.title = element_text(size = 20))

ggsave(paste0(figures_wd,"/skin-iat-regression-thirdgen-twoH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-thirdgen-twoH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-thirdgen-twoH.png"), width = 12, height = 6, units = "in")

# Three Hispanic 
modelplot(reg1[[3]],
          conf_level = 0.9,
          coef_map = cm, color = 'red',
          title = "Three Hispanic Grandparents") +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15) 
  )+ 
  labs(title = "Three Hispanic Grandparents") + theme(plot.title = element_text(size = 20))

ggsave(paste0(figures_wd,"/skin-iat-regression-thirdgen-threeH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-thirdgen-threeH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-thirdgen-threeH.png"), width = 12, height = 6, units = "in")

# Four Hispanic 
modelplot(reg1[[4]],
          conf_level = 0.9,
          coef_map = cm, color = 'red',
          title = "Four Hispanic Grandparents") +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15) 
  )+ 
  labs(title = "Four Hispanic Grandparents") + theme(plot.title = element_text(size = 20))

ggsave(paste0(figures_wd,"/skin-iat-regression-thirdgen-fourH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-thirdgen-fourH.png"), width = 10, height = 4, units = "in")
ggsave(paste0(pres_plots,"/skin-iat-regression-thirdgen-fourH.png"), width = 12, height = 6, units = "in")
