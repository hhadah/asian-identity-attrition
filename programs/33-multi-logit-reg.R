# This a script for
# a multi logit reg-
# ression where the 
# LHS variable is
# parents types
# and RHS is bias

# Date: Oct 12th, 2022

# open data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT.csv")) |> 
  filter(SecondGen == 1) |> 
  mutate(ParentType2 = as.ordered(ParentType2),
         ParentType_Ordered = as.ordered(case_when(ParentType2 == "Hispanic-Hispanic" ~ 0,
                                                   ParentType2 == "Hispanic-White" ~ 1,
                                                   ParentType2 == "White-Hispanic" ~ 2))) |> 
  rename("Bias" = "value")
reg <- list(
  "\\specialcell{(1) \\\\ Probit}" = polr(as.ordered(ParentType2) ~ Bias + 
                                            MomGradCollege + DadGradCollege + age_mom + age_pop +
                                            yrimmig_mom + yrimmig_pop, 
                                          data = CPS_IAT, Hess=TRUE, 
                                          method = "probit"),
  "\\specialcell{(2) \\\\ Logistic}" = polr(as.ordered(ParentType2) ~ Bias +
                                              MomGradCollege + DadGradCollege + age_mom + age_pop +
                                              yrimmig_mom + yrimmig_pop,
                                            data = CPS_IAT, Hess=TRUE,
                                            method = "logistic")
)

margins_res <- list(
  "\\specialcell{(1) \\\\ Probit Marginal Effect \\\\ at Mean}" = margins(reg[[1]], variables = c('Bias', "MomGradCollege",
                                                                                                  "DadGradCollege", "age_mom",
                                                                                                  "age_pop", "yrimmig_mom", "yrimmig_pop")),
  "\\specialcell{(2) \\\\ Logistic Marginal Effect \\\\ at Mean}" = margins(reg[[2]], variables = c('Bias', "MomGradCollege",
                                                                                                    "DadGradCollege", "age_mom",
                                                                                                    "age_pop", "yrimmig_mom", "yrimmig_pop"))
)

png(file=file.path(thesis_plots,"probit-regression.png"))
plot(margins_res[[1]], 
     main = "Marginal Effect of Bias on Interethnic Marriages: Probit",
     cex.lab=1.2, cex.axis=0.8, cex.main=1.3,
     pch = 16)
dev.off()
png(file=file.path(figures_wd,"probit-regression.png"))
plot(margins_res[[1]], 
     main = "Marginal Effect of Bias on Interethnic Marriages: Probit",
     cex.lab=1.2, cex.axis=0.8, cex.main=1.3,
     pch = 16)
dev.off()

png(file=file.path(thesis_plots,"logistic-regression.png"))
plot(margins_res[[2]], 
     main = "Marginal Effect of Bias on Interethnic Marriages: Logistic",
     cex.lab=1.2, cex.axis=0.8, cex.main=1.3,
     pch = 16)
dev.off()
png(file=file.path(figures_wd,"logistic-regression.png"))
plot(margins_res[[2]], 
     main = "Marginal Effect of Bias on Interethnic Marriages: Logistic",
     cex.lab=1.2, cex.axis=0.8, cex.main=1.3,
     pch = 16)
dev.off()


tidy_custom.polr <- function(x, ...) {
  s <- lmtest::coeftest(x)
  out <- data.frame(
    term = row.names(s),
    p.value = s[, "Pr(>|t|)"])
  out
}

cm <- c(
  "Bias"                             = "Bias",
  "MomGradCollege"                   = "College Indicator: Wife",
  "DadGradCollege"                   = "College Indicator: Husband"
) 
modelsummary(margins_res, 
  fmt = 2,
  exponentiate = c(F, F),
  coef_map = cm,
  gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
  stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1)) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), 
                latex_options = "scale_down"
  )

regression_tab <- modelsummary(margins_res, 
                               exponentiate = c(F, F), 
  fmt = 2,
  output = "latex", 
  coef_map = cm,
  escape = F,
  gof_omit = 'DF|Deviance|R2|AIC|BIC|Log.Lik.|F|Std.Errors|RMSE',
  stars= c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  title = "Relationship Between Bias and Interethnic Marriages: Marginal Effect of Ordered Logit and Probit Regressions \\label{regtab-logit-01}")  |> 
  kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),
                latex_options = c("HOLD_position")
  ) |>
  footnote(number = c("\\\\footnotesize{This is the result to estimating (\\\\ref{eq:inter-hw}) as a
                      multinomial probit and logit regression.}",
                      "\\\\footnotesize{The results are of regressions where the left hand side variable is 
                      an interethnic marriage ordinal variable where a value of: 1) zero is an endogamous marriage with
                      objectively Hispanic-Husband-Hispanic-Wife; 2) one is an interethnic marriage with
                      objectively Hispanic-Husband-White-Wife; 3) two is an interethnic marriage with
                      objectively White-Husband-Hispanic-Wife. I include controls for partners' sex, age, education, 
                      and years since immigrating to the United States.
                      Standard errors are clustered on the state level.}",
                      "\\\\footnotesize{Data source is the 2004-2020 Current Population Survey Data.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(tables_wd,"tab41-multi-logit.tex"))

regression_tab %>%
  #save_kable(file = "Objective_Subjective.html", self_contained = T)
  save_kable(file.path(thesis_tabs,"tab41-multi-logit.tex"))

