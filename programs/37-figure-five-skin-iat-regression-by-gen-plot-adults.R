# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# table with different FE

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian_adults.csv")) |> 
  rename(value = lw_index)

# By generation
reg1 <- list(
  "\\specialcell{(1) \\\\ $A_i$}" = feols(Asian ~ 1 + value + Female 
                                + EducYears + frac_asian + lnftotval +
                                Age + Age_sq + Age_cube + Age_quad  + AA_0bj  + FirstGen_Asian + SecondGen_Asian| region:year, 
                                data = CPS_IAT, weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $A^1_i$}" = feols(Asian ~ 1 + value + Female 
                               + EducYears + frac_asian +  lnftotval +
                               Age + Age_sq + Age_cube + Age_quad| region:year, 
                               data = CPS_IAT |> filter(FirstGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $A^2_i$}" = feols(Asian ~ 1 + value + Female 
                               + EducYears + frac_asian +  lnftotval +
                               Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                               data = CPS_IAT |> filter(SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip)
  
)

cm <- c("value" = "Bias",
        "Female" = "Female",
        "EducYears" = "Years of Education",
        "lnftotval" = "IHS Total Family Income"#,
        #"lnftotval_mom" = "Log Total Family Income"#,
        #"age" = "Age",
        #"HH" = "Both parents Asian",
        # "FirstGen" = "First Gen",
        # "SecondGen" = "Second Gen",
        # "ThirdGen" = "Third Generation"
) 

# All generations
text_data <- subset(tidy(reg1[[1]]), term %in% c("value", "Female", 
                                                "EducYears", 
                                                "lnftotval"))
text_data <- as.data.frame(text_data)
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Female"
row.names(text_data)[3] = "Years of Education"
row.names(text_data)[4] = "IHS Total Family Income"

p1 <- modelplot(reg1[[1]],
          coef_map = cm, color = "#E69F00",
          conf_level = 0.95) +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', linewidth = 1) +
  theme_customs() +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                label = round(estimate, digits = 2)), data = text_data,
                size = 6) +
  theme(
    strip.background = element_rect(
      color="black", fill="white", linewidth=1.5
    ),
    axis.title.x = element_blank()
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 15)) 
p1
ggsave(paste0(figures_wd,"/skin-iat-regression-all-gens-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-all-gens-adults.png"), width = 10, height = 4, units = "in")

# first-generation
text_data <- subset(tidy(reg1[[2]]), term %in% c("value", "Female", 
                                                "EducYears", 
                                                "lnftotval"))
text_data <- as.data.frame(text_data)
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Female"
row.names(text_data)[3] = "Years of Education"
row.names(text_data)[4] = "IHS Total Family Income"

p2 <-modelplot(reg1[[2]],
          coef_map = cm, color = "#CC79A7",
          conf_level = 0.95) +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', linewidth = 1) +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", linewidth=1.5
    ),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(axis.text = element_text(size = 15)) 
p2
ggsave(paste0(figures_wd,"/skin-iat-regression-first-gen-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-first-gen-adults.png"), width = 10, height = 4, units = "in")

# second-generation
text_data <- subset(tidy(reg1[[3]]), term %in% c("value", "Female", 
                                                "EducYears", 
                                                "lnftotval"))
text_data <- as.data.frame(text_data)
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Female"
row.names(text_data)[3] = "Years of Education"
row.names(text_data)[4] = "IHS Total Family Income"

p3 <-modelplot(reg1[[3]],
          coef_map = cm, color = "#009E73",
          conf_level = 0.95) +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', linewidth = 1) +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  labs(x = "Coefficient estimates and 95% CI") +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", linewidth=1.5
    )
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20)) 
p3
ggsave(paste0(figures_wd,"/skin-iat-regression-second-gen-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/skin-iat-regression-second-gen-adults.png"), width = 10, height = 4, units = "in")
