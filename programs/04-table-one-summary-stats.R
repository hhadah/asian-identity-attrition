# Summary statisitcs

# date: June 15th, 2022

# open data

CPS_index <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  mutate(ftotval_def = cpi99 * ftotval_mom)

# summary stat tables by
# generation type
sumstat1 <-   tbl_summary(data = CPS_index, by = "Type_Asian",
                          include = c(Female, Asian,
                                      age,
                                      DadGradCollege,
                                      MomGradCollege,
                                      ftotval_def#, value#,
                                      # HH,
                                      # HW,
                                      # WH,
                                      # WW
                                      ),
                          statistic = list(all_continuous() ~ "{mean} \n({sd})",
                                           all_categorical() ~ "{p}"),
                          digits = all_categorical() ~ function(x) style_number(x, digits = 2),
                          label = list(
                            Female~ "Female",
                            Asian ~ "Asian",
                            age ~ "Age",
                            DadGradCollege ~ "College Graduate: \n \t Father", 
                            MomGradCollege ~ "College Graduate: \n \t Mother",
                            ftotval_def ~ "Total Family Income \n \t (1999 dollars)"#,
                            #value ~ "Bias",
                            # HH ~ "Asian Father & Asian Mother",
                            # HW ~ "Asian Father & White Mother",
                            # WH ~ "White Father & Asian Mother",
                            # WW ~ "White Father & White Mother"
                            ),
                          missing = "no") |> 
  modify_header(stat_1 = "**First** \nN=40,033",
                stat_2 = "**Second** \nN=199,294",
                stat_3 = "**Third** \nN=79,077") |> 
  modify_footnote(update = everything() ~ NA)

# summary stat tables of 
# overall sample

sumstat_all <-   tbl_summary(data = CPS_index,
                             include = c(Female, Asian,
                                         age,
                                         DadGradCollege,
                                         MomGradCollege,
                                         ftotval_def#, value#,
                                         # HH,
                                         # HW,
                                         # WH,
                                         # WW
                                         ),
                             statistic = list(all_continuous() ~ "{mean} \n({sd})",
                                              all_categorical() ~ "{p}"),
                             digits = all_categorical() ~ function(x) style_number(x, digits = 2),
                             label = list(
                               Female~ "Female",
                               Asian ~ "Asian",
                               age ~ "Age",
                               DadGradCollege ~ "College Graduate: \n \t Father", 
                               MomGradCollege ~ "College Graduate: \n \t Mother",
                               ftotval_def ~ "Total Family Income \n \t (1999 dollars)"#,
                               #value ~ "Bias"#,
                               # HH ~ "Asian Father & Asian Mother",
                               # HW ~ "Asian Father & White Mother",
                               # WH ~ "White Father & Asian Mother",
                               # WW ~ "White Father & White Mother"
                               ),
                             missing = "no") |> 
  modify_header(stat_0 = "**All Sample** \nN = 318,404") |> 
  modify_footnote(update = everything() ~ NA)

sumstat <- tbl_merge(tbls = list(sumstat_all, sumstat1),
                     tab_spanner = c("**Overall**", "**By Generation**"))

# sumstat_all %>% 
#   as_gt() %>% 
#   gt::gtsave(filename = file.path(tables_wd,"SumStatOverAll.tex"))

# save overall table
sumstat_all %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 caption = "Current Population Survey (CPS) Summary Statistics \\label{tab:sumstat1}") %>% 
  footnote(number = c("The samples include children ages 17 and below who live in intact families. First-generation Asian immigrant children that were born in a Asian country. Native-born second-generation Asian immigrant children with at least one parent born in a Asian country. Finally, native-born third generation Asian immigrant children with native-born parents and at least one grand parent born in a Asian country.",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab01a-SumStatOverAll.tex"))
sumstat_all %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 caption = "CPS Summary Statistics \\label{tab:sumstat1}") %>%
  footnote(number = c("The sample includes children ages 17 and below who live in intact families. First-generation Asian immigrant children that were born in a Asian country. Native-born second-generation Asian immigrant children with at least one parent born in a Asian country. Finally, native-born third-generation Asian immigrant children with native-born parents and at least one grand parent born in a Asian country.",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), 
                latex_options = "scale_down", full_width = FALSE) %>% 
  save_kable(file.path(thesis_tabs,"tab01a-SumStatOverAll.tex"))
# save table by type

sumstat %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 caption = "CPS Summary Statistics with Skin IAT Scores \\label{tab:sumstat1}") %>% 
  footnote(number = c("The samples include children ages 17 and below who live in intact families. First-generation Asian immigrant children that were born in a Asian country. Native-born second-generation Asian immigrant children with at least one parent born in a Asian country. Finally, native-born third generation Asian immigrant children with native-born parents and at least one grand parent born in a Asian country.",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), 
                latex_options = "scale_down", full_width = FALSE) %>% 
  save_kable(file.path(tables_wd,"tab01-SumStat.tex"))

sumstat %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = F,
                 caption = "CPS Summary Statistics with Skin IAT Scores \\label{tab:sumstat1}") %>% 
  footnote(number = c("The samples include children ages 17 and below who live in intact families. First-generation Asian immigrant children that were born in a Asian country. Native-born second-generation Asian immigrant children with at least one parent born in a Asian country. Finally, native-born third generation Asian immigrant children with native-born parents and at least one grand parent born in a Asian country.",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), latex_options = c("scale_down", "HOLD_position"), full_width = FALSE) %>% 
  # landscape() %>% 
  save_kable(file.path(thesis_tabs,"tab01-SumStat.tex"))