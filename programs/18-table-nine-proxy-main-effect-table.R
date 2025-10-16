# This a script to
# create a table of
# identity by proxy
# MAIN EFFECTS

# Date: Oct 11th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  filter(SecondGen_Asian == 1) |> 
  rename(value = lw_index)
# mean Asian for 
# mother as proxy

mother_proxy_mean <- CPS_IAT |> 
  filter(Proxy == "Mother") |> 
  summarise(mean(Asian, na.rm = T))

mother_proxy_mean_by <- CPS_IAT |> 
  filter(Proxy == "Mother") |> 
  group_by(ParentType2) |> 
  summarise(mean(Asian, na.rm = T))

# mean Asian for 
# father as proxy

father_proxy_mean <- CPS_IAT |> 
  filter(Proxy == "Father") |> 
  summarise(mean(Asian, na.rm = T))

father_proxy_mean_by <- CPS_IAT |> 
  filter(Proxy == "Father") |> 
  group_by(ParentType2) |> 
  summarise(mean(Asian, na.rm = T))

# mean Asian for 
# self

self_mean <- CPS_IAT |> 
  filter(Proxy == "Self") |> 
  summarise(mean(Asian, na.rm = T))

self_mean_by <- CPS_IAT |> 
  filter(Proxy == "Self") |> 
  group_by(ParentType2) |> 
  summarise(mean(Asian, na.rm = T))

# mean Asian for 
# other proxy

others_proxy_mean <- CPS_IAT |> 
  filter(Proxy == "Other") |> 
  summarise(mean(Asian, na.rm = T))

others_proxy_mean_by <- CPS_IAT |> 
  filter(Proxy == "Other") |> 
  group_by(ParentType2) |> 
  summarise(mean(Asian, na.rm = T))

### Table

first_col <- c("Proxy:",
               "Mother",  
               "Father", 
               "Self", 
               "Others"
)

second_col <- c("",
                round(mother_proxy_mean[[1]], digits = 2),
                round(father_proxy_mean[[1]], digits = 2),
                round(self_mean[[1]], digits = 2),
                round(others_proxy_mean[[1]], digits = 2))

third_col <- c("",
  round(mother_proxy_mean_by[[1,2]], digits = 2),
  round(father_proxy_mean_by[[1,2]], digits = 2),
  round(self_mean_by[[1,2]], digits = 2),
  round(others_proxy_mean_by[[1,2]], digits = 2))

fourth_col <- c("",
  round(mother_proxy_mean_by[[2,2]], digits = 2),
  round(father_proxy_mean_by[[2,2]], digits = 2),
  round(self_mean_by[[2,2]], digits = 2),
  round(others_proxy_mean_by[[2,2]], digits = 2))

fifth_col <- c("",
  round(mother_proxy_mean_by[[3,2]], digits = 2),
  round(father_proxy_mean_by[[3,2]], digits = 2),
  round(self_mean_by[[3,2]], digits = 2),
  round(others_proxy_mean_by[[3,2]], digits = 2))



Table_cols <- cbind(first_col, second_col, third_col, fourth_col, fifth_col)

colnames(Table_cols) <- c()
colnames(Table_cols) <- c("Parents Type",
                          "All",      
                          "Asian-Asian",
                          "Asian-White",
                          "White-Asian")

knitr::kable(Table_cols, "latex", valign = 'c',
             booktabs = T,
             caption = "Main Effect of Proxy on Second-Generation's Asian Self-identification \\label{tab:hispbyproxy}") %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), latex_options = c("HOLD_position"), full_width = FALSE,  font_size = 12) |>
  add_indent(c(2:5)) |> 
  save_kable(file.path(tables_wd,"maineffect-proxy.tex")) %>% 
  save_kable(file.path(thesis_tabs,"maineffect-proxy.tex"))




# fixed effects regression: White Mother proxy

CPS_index_WM <- CPS_IAT |> 
  filter(Proxy == "Mother" & White_Mom == 1)
CPS_index_HM <- CPS_IAT |> 
  filter(Proxy == "Mother" & Asian_Mom == 1)

CPS_index_WF <- CPS_IAT |> 
  filter(Proxy == "Father" & White_Dad == 1)
CPS_index_HF <- CPS_IAT |> 
  filter(Proxy == "Father" & Asian_Dad == 1)
CPS_index_self <- CPS_IAT |> 
  filter(Proxy == "Self")
CPS_index_other <- CPS_IAT |> 
  filter(Proxy == "Other")
reg1 <- list(
  "\\specialcell{(1) \\\\ $A_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad+ FirstGen + SecondGen| region*year, 
                     data = CPS_index_WM, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $A_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad+ FirstGen + SecondGen| region*year, 
                     data = CPS_index_HM, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $A_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad+ FirstGen + SecondGen| region*year, 
                     data = CPS_index_WF, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $A_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad+ FirstGen + SecondGen| region*year, 
                     data = CPS_index_HF, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(5) \\\\ $A_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad+ FirstGen + SecondGen| region*year, 
                     data = CPS_index_self, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(6) \\\\ $A_{ist}$}" = feols(Asian ~ 1 + value + Female 
                     + MomGradCollege + DadGradCollege + frac_asian +
                       Age + Age_sq + Age_cube + Age_quad+ FirstGen + SecondGen| region*year, 
                     data = CPS_index_other, weights = ~weight, vcov = ~statefip)
  
)

reg1[[1]]
reg1[[2]]
reg1[[3]]
reg1[[4]]
reg1[[5]]
reg1[[6]]

cm <- c("value" = "Prejudice Measure",
        "Female" = "Female",
        "MomGradCollege" = "College Graduate: Mother",
        "DadGradCollege" = "College Graduate: Father",
        "lnftotval_mom" = "Log Total Family Income",
        #"age" = "Age",
        #"HH" = "Both parents Asian",
        "FirstGen" = "First Gen",
        "SecondGen" = "Second Gen"#,
        #"ThirdGen" = "Third Generation"
) 

f1 <- function(x) format(round(x, 2), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE: region:year", clean = "Year $\\times$ Region FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

options(modelsummary_format_numeric_latex = "plain")

regression_tab <- modelsummary(reg1, fmt = f1,
                               output = "latex", 
                               coef_map = cm,
                               escape = F,
                               gof_map = gm,
                               #gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors',
                               stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
                               title = "Relationship Between Bias and Self-Reported Asian Identity: By Proxy Respondent\\label{regtab-proxy-01}") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                latex_options = c("scale_down", "hold_position")
  ) %>%
  footnote(number = c("\\\\footnotesize{Each column is an estimation of a heterogeneous effect of regression (\\\\ref{eq:identity_reg_bias}) by 
                      the proxy household respondent with region × year fixed effects. 
                      I include controls for sex, quartic age, fraction of Asians in a state, parental education.
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{The samples include children ages 17 and below who live in intact families. 
                      First-generation Asian immigrant children that were born in a 
                      Spanish-speaking county. Native-born second-generation Asian 
                      immigrant children with at least one parent born in an Asian 
                      country. Finally, native-born third-generation Asian immigrant children 
                      with native-born parents and at least one grandparent born in a Spanish 
                      speaking country.}",
                      "\\\\footnotesize{Data source is the 2004-2021 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T
  ) |> 
  add_header_above(c(" " = 1, 
                     "White Mother" = 1, "Asian Mother" = 1, 
                     "White Father" = 1, "Asian Father" = 1,
                     "Self" = 1, "Other" = 1)) |> 
  add_header_above(c(" " = 1,"Proxy Respondent" = 6))


regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab06-regression_tab_byproxy_skin.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab06-regression_tab_byproxy_skin.tex"))

# regression_tab %>%
#   #save_kable(file = "Objective_Subjective.html", self_contained = T)
#   save_kable(file.path(Identity_paper_tab,"tab06-regression_tab_byproxy_skin.tex"))

