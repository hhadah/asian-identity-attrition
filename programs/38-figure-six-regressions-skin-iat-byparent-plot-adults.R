# This a script to
# run a regression
# with cps and 
# project implicit
# data

# Date: July 30th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian_adults.csv")) |> 
  rename(value = lw_index)

# fixed effects regression

reg1 <- list(
  "\\specialcell{(1) \\\\ $A^2$}" = feols(Asian ~ 1 + value + Female 
                     + EducYears + frac_asian + lnftotval +
                       Age + Age_sq + Age_cube + Age_quad + AA_0bj| region:year, 
                     data = CPS_IAT |> filter(SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(2) \\\\ $A^2$}" = feols(Asian ~ 1 + value + Female 
                     + EducYears + frac_asian + lnftotval +
                       Age + Age_sq + Age_cube + Age_quad| region:year, 
                     data = CPS_IAT |> filter(AA_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(3) \\\\ $A^2$}" = feols(Asian ~ 1 + value + Female 
                     + EducYears + frac_asian + lnftotval +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(AW_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip),
  "\\specialcell{(4) \\\\ $A^2$}" = feols(Asian ~ 1 + value + Female 
                     + EducYears + frac_asian + lnftotval +
                       Age + Age_sq + Age_cube + Age_quad | region:year, 
                     data = CPS_IAT |> filter(WA_0bj == 1 & SecondGen_Asian == 1), weights = ~weight, vcov = ~statefip)
)

cm <- c("value" = "Bias",
        "Female" = "Female",
        "EducYears" = "Years of Education",
        "lnftotval" = "IHS Total Family Income"#,
        #"age" = "Age",
        #"HH" = "Both parents Asian",
        # "FirstGen" = "First Gen",
        # "SecondGen" = "Second Gen",
        # "ThirdGen" = "Third Generation"
) 

# Second-generation: all parents
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
                conf_level = 0.9) +
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
    axis.title.x = element_blank()
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 15)) 

ggsave(paste0(figures_wd,"/by-parents-regs-all-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/by-parents-regs-all-adults.png"), width = 10, height = 4, units = "in")

# HH parents
text_data <- subset(tidy(reg1[[2]]), term %in% c("value", "Female", 
                                                "EducYears", 
                                                "lnftotval"))
text_data <- as.data.frame(text_data)
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Female"
row.names(text_data)[3] = "Years of Education"
row.names(text_data)[4] = "IHS Total Family Income"

p2 <- modelplot(reg1[[2]],
                coef_map = cm, color = "#CC79A7",
                conf_level = 0.9) +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(axis.text = element_text(size = 15)) 
p2
ggsave(paste0(figures_wd,"/by-parents-regs-hh-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/by-parents-regs-hh-adults.png"), width = 10, height = 4, units = "in")

# HW parents
text_data <- subset(tidy(reg1[[3]]), term %in% c("value", "Female", 
                                                "EducYears", 
                                                "lnftotval"))
text_data <- as.data.frame(text_data)
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Female"
row.names(text_data)[3] = "Years of Education"
row.names(text_data)[4] = "IHS Total Family Income"

p3 <- modelplot(reg1[[3]],
                coef_map = cm, color = "#009E73",
                conf_level = 0.9) +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    )
  )+
  labs(x = "Coefficient estimates and 90% CI") +
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20)) 
p3
ggsave(paste0(figures_wd,"/by-parents-regs-hw-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/by-parents-regs-hw-adults.png"), width = 10, height = 4, units = "in")

# WH parents
text_data <- subset(tidy(reg1[[4]]), term %in% c("value", "Female", 
                                                "EducYears", 
                                                "lnftotval"))
text_data <- as.data.frame(text_data)
row.names(text_data) = text_data$term
row.names(text_data)[1] = "Bias"
row.names(text_data)[2] = "Female"
row.names(text_data)[3] = "Years of Education"
row.names(text_data)[4] = "IHS Total Family Income"

p4 <- modelplot(reg1[[4]],
                coef_map = cm, color = "#CC79A7",
                conf_level = 0.9) +
  geom_text_repel(aes(x = estimate, y = row.names(text_data),
                      label = round(estimate, digits = 2)), data = text_data,
                  size = 6) +
  # facet_grid(~model) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted', size = 1) +
  theme_customs() +
  labs(x = "Coefficient estimates and 90% CI") +
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=1.5
    ),
    axis.text.y = element_blank()
  )+
  # scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))
  theme(
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 20)) 
p4
ggsave(paste0(figures_wd,"/by-parents-regs-wh-adults.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/by-parents-regs-wh-adults.png"), width = 10, height = 4, units = "in")
