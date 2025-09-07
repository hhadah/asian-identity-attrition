# Summary statisitcs

# date: June 15th, 2022

# open data

CPS_index <- read_csv(file.path(datasets,"CPS_IAT_asian_adults.csv")) |> 
  mutate(ftotval_def = cpi99 * ftotval)

# summary stat tables by
# generation type
sumstat1 <-   tbl_summary(data = CPS_index, by = "Type_Asian",
                          include = c(Female, Asian,
                                      age,
                                      EducYears,
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
                            EducYears ~ "Years of Education",
                            ftotval_def ~ "Total Family Income \n \t (1999 dollars)"#,
                            #value ~ "Bias",
                            # HH ~ "Asian Father & Asian Mother",
                            # HW ~ "Asian Father & White Mother",
                            # WH ~ "White Father & Asian Mother",
                            # WW ~ "White Father & White Mother"
                            ),
                          missing = "no") |> 
  modify_header(stat_1 = "**First** \nN=35,728",
                stat_2 = "**Second** \nN=12,425") |> 
  modify_footnote(update = everything() ~ NA)

# summary stat tables of 
# overall sample

sumstat_all <-   tbl_summary(data = CPS_index,
                             include = c(Female, Asian,
                                         age,
                                         EducYears,
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
                               EducYears ~ "Years of Education",
                               ftotval_def ~ "Total Family Income \n \t (1999 dollars)"#,
                               #value ~ "Bias"#,
                               # HH ~ "Asian Father & Asian Mother",
                               # HW ~ "Asian Father & White Mother",
                               # WH ~ "White Father & Asian Mother",
                               # WW ~ "White Father & White Mother"
                               ),
                             missing = "no") |> 
  modify_header(stat_0 = "**All Sample** \nN = 48,153") |> 
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
                 caption = "Current Population Survey (CPS) Summary Statistics \\label{tab:sumstat-adults}") %>% 
  footnote(number = c("The samples include people of Asian ancestry ages 18 and above. First-generation Asian immigrants were born in a Asian country. Native-born second-generation Asian immigrants have at least one parent born in a Asian country. ",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab53-SumStatOverAll-adults.tex"))
sumstat_all %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 ccaption = "Current Population Survey (CPS) Summary Statistics \\label{tab:sumstat-adults}") %>% 
  footnote(number = c("The samples include people of Asian ancestry ages 18 and above. First-generation Asian immigrants were born in a Asian country. Native-born second-generation Asian immigrants have at least one parent born in a Asian country. ",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab53-SumStatOverAll-adults.tex"))

# save table by type

sumstat %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = T,
                 caption = "Current Population Survey (CPS) Summary Statistics \\label{tab:sumstat-adults}") %>% 
  footnote(number = c("The samples include people of Asian ancestry ages 18 and above. First-generation Asian immigrants were born in a Asian country. Native-born second-generation Asian immigrants have at least one parent born in a Asian country. ",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab53-SumStatOverAll-adults.tex"))

sumstat %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = "",
                 escape = F,
                 caption = "Current Population Survey (CPS) Summary Statistics \\label{tab:sumstat-adults}") %>% 
  footnote(number = c("The samples include people of Asian ancestry ages 18 and above. First-generation Asian immigrants were born in a Asian country. Native-born second-generation Asian immigrants have at least one parent born in a Asian country. ",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab53-SumStatOverAll-adults.tex"))
