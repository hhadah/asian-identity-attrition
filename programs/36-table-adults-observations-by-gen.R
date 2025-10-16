# Table of observation number
# of different generations


# date: Aug 26th, 2022

CPS <- read_csv(file.path(datasets,"CPS_IAT_asian_adults.csv"))

### Generate generations vairables and table

#### First Generation Calculations

Asians_FirstGen <- CPS %>%
  filter(FirstGen_Asian == 1 & Asian == 1) %>%
  summarise(n())
NonAsians_FirstGen <- CPS %>%
  filter(FirstGen_Asian == 1 & Asian == 0) %>%
  summarise(n())
FirstGen <- CPS %>%
  filter(FirstGen_Asian == 1) %>%
  summarise(n())

# Calculate number of First generation Asians
# By group

### Father and Mother are Asians (Both Sides)
Asians_FirstGen_BothSides <- CPS %>%
  filter(FirstGen_Asian == 1 & Asian == 1 & 
           ((Asian_Dad == 1 & Asian_Mom == 1))) %>%
  summarise(n())

FirstGen_BothSides <- CPS %>%
  filter(FirstGen_Asian == 1 & 
           ((Asian_Dad == 1 & Asian_Mom == 1))) %>%
  summarise(n())

### Father or Mother are Asians (One Sides)
Asians_FirstGen_OneSides <- CPS %>%
  filter(FirstGen_Asian == 1 & Asian == 1 & 
           ((Asian_Dad == 1 & Asian_Mom == 0) | (Asian_Dad == 0 & Asian_Mom == 1))) %>%
  summarise(n())
FirstGen_OneSides <- CPS %>%
  filter(FirstGen_Asian == 1 & 
           ((Asian_Dad == 1 & Asian_Mom == 0) | (Asian_Dad == 0 & Asian_Mom == 1))) %>%
  summarise(n())

### Father is Asians (One Sides)
Asians_FirstGen_FatherSides <- CPS %>%
  filter(FirstGen_Asian == 1 & Asian == 1 & 
           (Asian_Dad == 1)) |> 
  summarise(n())
FirstGen_FatherSides <- CPS %>%
  filter(FirstGen_Asian == 1 & 
           (Asian_Dad == 1)) %>%
  summarise(n())

### Mother is Asians (One Sides)
Asians_FirstGen_MotherSides <- CPS %>%
  filter(FirstGen_Asian == 1 & Asian == 1 & 
           (Asian_Mom == 1)) %>%
  summarise(n())
FirstGen_MotherSides <- CPS %>%
  filter(FirstGen_Asian == 1 & 
           (Asian_Mom == 1)) %>%
  summarise(n())

#### Second Generation Calculations

# Calculate number of Second generation Asians

# All Asian Second Generations
Asians_SecondGen <- CPS %>%
  filter(SecondGen_Asian == 1 & Asian == 1) %>%
  summarise(n())

# Asians, Father and Mother are Hisapnics (Both Sides)
Asians_SecondGen_BothSides <- CPS %>%
  filter(SecondGen_Asian == 1 & Asian == 1 & 
           ((AsianPOB_Father == 1 & AsianPOB_Mother == 1))) %>%
  summarise(n())

# Asians, Father or Mother are Asians (One Side)
Asians_SecondGen_OneSides <- CPS %>%
  filter(SecondGen_Asian == 1 & Asian == 1 & 
           ((AsianPOB_Father == 1 & AsianPOB_Mother == 0) | (AsianPOB_Father == 0 & AsianPOB_Mother == 1))) %>%
  summarise(n())

# Asians, Mother is Asians (One Side)
Asians_SecondGen_MotherSides <- CPS %>%
  filter(SecondGen_Asian == 1 & Asian == 1 & 
           (AsianPOB_Mother == 1)) %>%
  summarise(n())

# Asians, Father is Asians (One Side)
Asians_SecondGen_FatherSides <- CPS %>%
  filter(SecondGen_Asian == 1 & Asian == 1 & 
           (AsianPOB_Father == 1)) %>%
  summarise(n())

# Second Generation
SecondGen <- CPS %>%
  filter(SecondGen_Asian == 1) %>%
  summarise(n())

# Second Generation Both Sides
SecondGen_BothSides <- CPS %>%
  filter(SecondGen_Asian == 1 & 
           ((AsianPOB_Father == 1 & AsianPOB_Mother == 1))) %>%
  summarise(n())

# Second Generation One Sides
SecondGen_OneSides <- CPS %>%
  filter(SecondGen_Asian == 1 & 
           ((AsianPOB_Father == 1 & AsianPOB_Mother == 0) | (AsianPOB_Father == 0 & AsianPOB_Mother == 1))) %>%
  summarise(n())

# Second Generation Mother Sides
SecondGen_MotherSides <- CPS %>%
  filter(SecondGen_Asian == 1 & 
           (AsianPOB_Mother == 1)) %>%
  summarise(n())

# Second Generation Father Sides
SecondGen_FatherSides <- CPS %>%
  filter(SecondGen_Asian == 1 & 
           (AsianPOB_Father == 1)) %>%
  summarise(n())


#### Table
first_col <- c("1st Gen.", 
               "2nd Gen.",  "Asian on:", "Both Sides", "One Side"               
               )

second_col <- c(format(Asians_FirstGen[[1]], big.mark = ",", scientific = FALSE),
                format(Asians_SecondGen[[1]], big.mark = ",", scientific = FALSE), " ", 
                format(Asians_SecondGen_BothSides[[1]], big.mark = ",", scientific = FALSE),
                format(Asians_SecondGen_OneSides[[1]], big.mark = ",", scientific = FALSE)
)

third_col <- c(format(FirstGen[[1]] - Asians_FirstGen[[1]], big.mark = ",", scientific = FALSE),
               format(SecondGen[[1]] - Asians_SecondGen[[1]], big.mark = ",", scientific = FALSE),  " ",  
               format(SecondGen_BothSides[[1]] - Asians_SecondGen_BothSides[[1]], big.mark = ",", scientific = FALSE),
               format(SecondGen_OneSides[[1]] - Asians_SecondGen_OneSides[[1]], big.mark = ",", scientific = FALSE))

fourth_col <- c(format(round(Asians_FirstGen[[1]]/FirstGen[[1]], digits = 2), big.mark = ",", scientific = FALSE),
                format(round(Asians_SecondGen[[1]]/SecondGen[[1]], digits = 2), big.mark = ",", scientific = FALSE),  " ",
                format(round(Asians_SecondGen_BothSides[[1]]/SecondGen_BothSides[[1]], digits = 2), big.mark = ",", scientific = FALSE),
                format(round(Asians_SecondGen_OneSides[[1]]/SecondGen_OneSides[[1]], digits = 2), big.mark = ",", scientific = FALSE))

fifth_col <- c( round(1-Asians_FirstGen[[1]]/FirstGen[[1]], digits = 2), 
                round(1-Asians_SecondGen[[1]]/SecondGen[[1]], digits = 2),  " ", 
                round(1-Asians_SecondGen_BothSides[[1]]/SecondGen_BothSides[[1]], digits = 2),   
                round(1-Asians_SecondGen_OneSides[[1]]/SecondGen_OneSides[[1]], digits = 2)
)

Table_cols <- cbind(first_col, second_col,  third_col, fourth_col, fifth_col)

colnames(Table_cols) <- c()
colnames(Table_cols) <- c(' ','\\specialcell{Self-identify \\\\ as Asian}', '\\specialcell{Self-identify as \\\\ non-Asian}', '\\specialcell{\\% Self-identify \\\\ as Asian}', '\\specialcell{\\% Self-identify \\\\ as non-Asian}')

knitr::kable(Table_cols, "latex", align = "lcccc",
             booktabs = T,
             escape = F,
             caption = "Asian Self-identification by Generation \\label{tab:hispbygen-adults}") %>%
  column_spec(1, bold = T) %>%
  kable_classic(full_width = F) %>%
  add_indent(c(3:5)) %>% 
  add_indent(c(4:5)) %>% 
  #add_indent(c(10:11)) %>%
  kable_styling(#bootstrap_options = c("hover", "condensed"), 
                latex_options = c(#"scale_down", 
                "HOLD_position")) |> 
  footnote(number = c("The samples include people of Asian ancestry ages 18 and above. First-generation Asian immigrants were born in a Asian country. Native-born second-generation Asian immigrants have at least one parent born in a Asian country. ",
                      "Data source is the 2004-2021 Current Population Survey."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab54-observations-by-gen-adults.tex")) %>% 
  save_kable(file.path(thesis_tabs,"tab54-observations-by-gen-adults.tex"))
