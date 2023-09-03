# Summary statisitcs

# date: June 15th, 2022

# open data

Skin_Iat <- read_csv(file.path(datasets,"Skin_IAT_Clean.csv"))

Skin_Iat <- Skin_Iat |> 
  mutate(Education_iat = factor(Education_iat, ordered = TRUE))

# summary stat tables by
# generation type
sumstat1 <-   tbl_summary(data = Skin_Iat, 
                          include = c(age,
                                      Female,
                                      White,
                                      White_not_his,
                                      Hispanic,
                                      Education_iat,
                                      D_biep.LightSkin_Good_all
                          ),
                          statistic = list(all_continuous() ~ "{mean} \n({sd})",
                                           all_categorical() ~ "{p}%"),
                          label = list(
                            age~ "Age",
                            White ~ "White",
                            White_not_his ~ "Non-Hispanic White",
                            Hispanic ~ "Hispanic",
                            Education_iat ~ "Education Levels",
                            D_biep.LightSkin_Good_all ~ "Bias"
                          ),
                          missing = "no") %>%
  bold_labels()


# save overall table
sumstat1 %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 caption = "Skin Implicit Association Test (IAT) Scores Sample \\label{tab:sumstat-iat}") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), latex_options = c("HOLD_position"), full_width = FALSE) %>%
  footnote(alphabet = c("Data source is the 2004-2020 Harvard's Project Implicit Implicit Association Test scores."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab10-Skin-Iat-SumStat.tex"))
sumstat1 %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 caption = "Skin Implicit Association Test (IAT) Scores Sample \\label{tab:sumstat-iat}") %>% 
  footnote(alphabet = c("Data source is the 2004-2021 Harvard's Project Implicit Implicit Association Test scores."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), latex_options = c("HOLD_position"), full_width = FALSE) %>% 
  save_kable(file.path(thesis_tabs,"tab10-Skin-Iat-SumStat.tex"))

sumstat1 %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T
                 ) %>% 
  footnote(alphabet = c("Data source is the 2004-2020 Harvard's Project Implicit Implicit Association Test scores."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  kable_styling(bootstrap_options = c("condensed"), latex_options = c("HOLD_position"), full_width = FALSE, font_size = 10) %>% 
  save_kable(file.path(pres_tabs,"tab02-SumStat.tex"))

sumstat1 %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T
                 ) %>% 
  footnote(alphabet = c("Data source is the 2004-2020 Harvard's Project Implicit Implicit Association Test scores."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  kable_styling(bootstrap_options = c("condensed"), latex_options = c("HOLD_position"), full_width = FALSE, font_size = 10) %>% 
  save_kable(file.path(dissertation_wd,"tables/tab02-SumStat.tex"))





