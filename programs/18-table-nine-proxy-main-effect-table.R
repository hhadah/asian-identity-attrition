# This a script to
# create a table of
# identity by proxy
# MAIN EFFECTS

# Date: Oct 11th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  filter(SecondGen_Asian == 1)

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

