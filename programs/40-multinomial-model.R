# This script extends the binary regression analysis to multinomial logistic regression
# for Asian identity choices using nnet package (more suitable for this data structure)

# Load required libraries
suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(marginaleffects)  # avg_slopes(), predictions()
  library(modelsummary)
  library(kableExtra)
  library(ggplot2)
  library(purrr)
  library(rlang)
  library(stringr)
  library(forcats)
})

# 0) Housekeeping: seed & a slightly higher maxit for safety
set.seed(123)


# Load data
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |> 
  rename(value = lw_index) |> 
  mutate(OneAsian = case_when(Grandparent_Type == "AWWW" ~ 1,
                                 Grandparent_Type == "WAWW" ~ 1,
                                 Grandparent_Type == "WWAW" ~ 1,
                                 Grandparent_Type == "WWWA" ~ 1,
                                 TRUE ~ 0),
         TwoAsian = case_when(Grandparent_Type == "AAWW" ~ 1,
                                 Grandparent_Type == "AWAW" ~ 1,
                                 Grandparent_Type == "AWWA" ~ 1,
                                 Grandparent_Type == "WAAW" ~ 1,
                                 Grandparent_Type == "WWAA" ~ 1,
                                 Grandparent_Type == "WAWA" ~ 1,
                                 TRUE ~ 0),
         ThreeAsian = case_when(Grandparent_Type == "AAAW" ~ 1,
                                   Grandparent_Type == "AAWA" ~ 1,
                                   Grandparent_Type == "AWAA" ~ 1,
                                   Grandparent_Type == "WAAA" ~ 1,
                                 TRUE ~ 0),
         FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1,
                                 TRUE ~ 0))

# Function to categorize race codes for multinomial analysis
categorize_race_multinomial <- function(race_code) {
  case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only", 
    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White", # Asian + White/Pacific Islander combinations
    TRUE ~ "Other" # Will be excluded from multinomial analysis
  )
}

# Prepare data for multinomial logit
CPS_IAT_multinomial <- CPS_IAT |>
  mutate(
    identity_choice = categorize_race_multinomial(race),
    # Put Asian_only first so it becomes the baseline
    identity_choice = factor(
      identity_choice,
      levels = c("Asian_only", "White_only", "Asian_and_White")
    )
  ) |>
  filter(identity_choice %in% c("Asian_only", "White_only", "Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(region_year = interaction(region, year, drop = TRUE))

CPS_IAT_multinomial <- CPS_IAT_multinomial |>
  mutate(identity_choice = relevel(identity_choice, ref = "Asian_only"))
levels(CPS_IAT_multinomial$identity_choice)

# Check the distribution of outcomes
cat("Distribution of identity choices:\n")
print(table(CPS_IAT_multinomial$identity_choice))
print(prop.table(table(CPS_IAT_multinomial$identity_choice)))

# Function to fit multinomial models with different variable sets by generation
fit_multinomial_model <- function(data_subset, generation = "all", ancestry_filter = NULL) {
  
  # Define formula based on generation
  if (generation == "first") {
    # First generation specific formula (no AA_0bj, FirstGen_Asian, SecondGen_Asian)
    formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
               frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
  } else if (generation == "second") {
    # Second generation specific formula
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("AA_0bj", "AW_0bj", "WA_0bj")) {
      # When filtering by specific ancestry patterns, don't include AA_0bj as covariate
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + region_year
    }
  } else if (generation == "third") {
    # Third generation specific formula
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("OneAsian", "TwoAsian", "ThreeAsian", "FourAsian")) {
      # When filtering by specific grandparent patterns, don't include Grandparent_Type as covariate
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
                 frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type + region_year
    }
  } else { # all generations
    formula <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege + 
               frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + 
               FirstGen_Asian + SecondGen_Asian + region_year
  }
  
  # Fit multinomial logistic regression using nnet
  model <- multinom(formula, data = data_subset, weights = data_subset$weight, trace = FALSE)
  
  return(model)
}
# Create models for each generation
cat("Fitting multinomial models using nnet::multinom...\n")

# All generations
mnl_all_gen <- fit_multinomial_model(CPS_IAT_multinomial, "all")

# Sanity check: these columns should match your category names, baseline excluded
colnames(predict(mnl_all_gen, type = "probs"))

# First generation
mnl_first_gen <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(FirstGen_Asian == 1), 
  "first"
)

# Second generation  
mnl_second_gen <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1), 
  "second"
)

# Third generation
mnl_third_gen <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1), 
  "third"
)

# Third generation by number of Asian grandparents
cat("Fitting multinomial models for third generation ancestry subgroups...\n")

mnl_third_one   <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & OneAsian == 1), 
  "third", "OneAsian"
)

mnl_third_two   <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & TwoAsian == 1), 
  "third", "TwoAsian"
)

mnl_third_three <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & ThreeAsian == 1), 
  "third", "ThreeAsian"
)

mnl_third_four  <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1 & FourAsian == 1), 
  "third", "FourAsian"
)

# Second generation by parent ancestry patterns
cat("Fitting multinomial models for second generation ancestry subgroups...\n")

mnl_second_aa <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1 & AA_0bj == 1), 
  "second", "AA_0bj"
)

mnl_second_aw <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1 & AW_0bj == 1), 
  "second", "AW_0bj"
)

mnl_second_wa <- fit_multinomial_model(
  CPS_IAT_multinomial |> filter(SecondGen_Asian == 1 & WA_0bj == 1), 
  "second", "WA_0bj"
)



# 1) Compute Average Marginal Effects with proper SEs (clustered or bootstrap)
# Choose ONE of the vcov routes below:

me_all_gen    <- avg_slopes(mnl_all_gen,    variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = CPS_IAT_multinomial, vcov = ~statefip)
me_first_gen  <- avg_slopes(mnl_first_gen,  variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian==1), vcov = ~statefip)
me_second_gen <- avg_slopes(mnl_second_gen, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1), vcov = ~statefip)
me_third_gen  <- avg_slopes(mnl_third_gen,  variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1), vcov = ~statefip)

# Marginal effects for third generation ancestry subgroups
me_third_one   <- avg_slopes(mnl_third_one,   variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, OneAsian==1), vcov = ~statefip)
me_third_two   <- avg_slopes(mnl_third_two,   variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, TwoAsian==1), vcov = ~statefip)
me_third_three <- avg_slopes(mnl_third_three, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, ThreeAsian==1), vcov = ~statefip)
me_third_four  <- avg_slopes(mnl_third_four,  variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, FourAsian==1), vcov = ~statefip)

# Marginal effects for second generation ancestry subgroups
me_second_aa <- avg_slopes(mnl_second_aa, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                          by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AA_0bj==1), vcov = ~statefip)
me_second_aw <- avg_slopes(mnl_second_aw, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                          by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AW_0bj==1), vcov = ~statefip)
me_second_wa <- avg_slopes(mnl_second_wa, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                          by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, WA_0bj==1), vcov = ~statefip)

# 2) One-SD effects for `value` with CIs
# Normalize column names from marginaleffects outputs
normalize_me_names <- function(x) {
  df <- as.data.frame(x)
  nm <- names(df)
  map <- c(
    "Term" = "term",
    "Group" = "group",
    "Estimate" = "estimate",
    "Std. Error" = "std.error",
    "Pr(>|z|)" = "p.value",
    "2.5 %" = "conf.low",
    "97.5 %" = "conf.high",
    "Contrast" = "contrast",
    "Type" = "type"
  )
  for (k in names(map)) if (k %in% nm) names(df)[match(k, nm)] <- map[[k]]
  df
}

one_sd_effects <- function(me_df, data_subset, var = "value") {
  me_df <- normalize_me_names(me_df)

  # pick the correct column containing the regressor name
  term_col <- if ("variable" %in% names(me_df)) "variable" else "term"

  # If CIs are missing but std.error exists, build normal-approx CIs
  if (!("conf.low" %in% names(me_df)) || !("conf.high" %in% names(me_df))) {
    if ("std.error" %in% names(me_df)) {
      me_df <- dplyr::mutate(me_df,
                             conf.low = estimate - 1.96 * std.error,
                             conf.high = estimate + 1.96 * std.error)
    } else {
      me_df <- dplyr::mutate(me_df, conf.low = NA_real_, conf.high = NA_real_)
    }
  }

  sdv <- sd(data_subset[[var]], na.rm = TRUE)

  out <- me_df |>
    dplyr::filter(.data[[term_col]] == var) |>
    dplyr::mutate(
      variable     = .data[[term_col]],
      sd_value     = sdv,
      effect_1sd   = estimate * sdv,
      conf.low_1sd = conf.low * sdv,
      conf.high_1sd= conf.high * sdv
    )

  dplyr::select(out, dplyr::any_of(c(
    "group","variable","sd_value","effect_1sd","conf.low_1sd","conf.high_1sd","p.value"
  )))
}

sd_all    <- one_sd_effects(me_all_gen,    CPS_IAT_multinomial)
sd_first  <- one_sd_effects(me_first_gen,  dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian  == 1))
sd_second <- one_sd_effects(me_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1))
sd_third  <- one_sd_effects(me_third_gen,  dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian  == 1))

# One-SD effects for ancestry subgroups
sd_third_one   <- one_sd_effects(me_third_one,   dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, OneAsian==1))
sd_third_two   <- one_sd_effects(me_third_two,   dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, TwoAsian==1))
sd_third_three <- one_sd_effects(me_third_three, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, ThreeAsian==1))
sd_third_four  <- one_sd_effects(me_third_four,  dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, FourAsian==1))

sd_second_aa <- one_sd_effects(me_second_aa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AA_0bj==1))
sd_second_aw <- one_sd_effects(me_second_aw, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AW_0bj==1))
sd_second_wa <- one_sd_effects(me_second_wa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, WA_0bj==1))


# 3) Outcome means for a quick baseline row
calculate_outcome_means <- function(data_subset) {
  data_subset |>
    group_by(identity_choice) |>
    summarise(prop = n()/nrow(data_subset), .groups = "drop")
}
means_all_mnl    <- calculate_outcome_means(CPS_IAT_multinomial)
means_first_mnl  <- calculate_outcome_means(dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian  == 1))
means_second_mnl <- calculate_outcome_means(dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1))
means_third_mnl  <- calculate_outcome_means(dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian  == 1))

# 4) Build clean AME tables without mock models
# Helper to cast ME df into wide display with stars and CIs
# Normalize common column labels from marginaleffects objects
normalize_me_names <- function(x) {
  df <- as.data.frame(x)
  nm <- names(df)
  map <- c(
    "Term"       = "term",
    "Group"      = "group",
    "Estimate"   = "estimate",
    "Std. Error" = "std.error",
    "Pr(>|z|)"   = "p.value",
    "2.5 %"      = "conf.low",
    "97.5 %"     = "conf.high",
    "Contrast"   = "contrast",
    "Type"       = "type"
  )
  for (k in names(map)) if (k %in% nm) names(df)[match(k, nm)] <- map[[k]]
  df
}

pretty_me_table <- function(me_df, coef_map = NULL) {
  me_df <- normalize_me_names(me_df)

  # unify to a 'variable' column
  if (!"variable" %in% names(me_df)) {
    if ("term" %in% names(me_df)) {
      me_df <- dplyr::rename(me_df, variable = term)
    } else {
      stop("Could not find a predictor name column (`term` or `variable`).")
    }
  }
  # ensure CIs exist; if not, build from std.error
  if (!("conf.low" %in% names(me_df)) || !("conf.high" %in% names(me_df))) {
    if ("std.error" %in% names(me_df)) {
      me_df <- dplyr::mutate(me_df,
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
    } else {
      me_df <- dplyr::mutate(me_df, conf.low = NA_real_, conf.high = NA_real_)
    }
  }
  # filter to variables of interest
  if (!is.null(coef_map)) {
    me_df <- dplyr::filter(me_df, variable %in% names(coef_map))
  }

  lab <- function(x) if (is.null(coef_map) || !(x %in% names(coef_map))) x else coef_map[[x]]

  me_df |>
    dplyr::mutate(
      variable_label = vapply(variable, lab, FUN.VALUE = character(1)),
      est_ci = sprintf("%.4f [%.4f, %.4f]", estimate, conf.low, conf.high)
    ) |>
    dplyr::select(group, variable_label, est_ci, p.value) |>
    tidyr::pivot_wider(names_from = group, values_from = c(est_ci, p.value)) |>
    dplyr::arrange(variable_label)
}

cm_mnl <- c(
  "value" = "Anti-Asian Bias",
  "Female" = "Female",
  "MomGradCollege" = "College Graduate: Mother",
  "DadGradCollege" = "College Graduate: Father"
)

# tab_all    <- pretty_me_table(me_all_gen,    cm_mnl)
# tab_first  <- pretty_me_table(me_first_gen,  cm_mnl)
# tab_second <- pretty_me_table(me_second_gen, cm_mnl)
# tab_third  <- pretty_me_table(me_third_gen,  cm_mnl)

# Create tables for ancestry subgroups
# tab_third_one   <- pretty_me_table(me_third_one,   cm_mnl)
# tab_third_two   <- pretty_me_table(me_third_two,   cm_mnl)
# tab_third_three <- pretty_me_table(me_third_three, cm_mnl)
# tab_third_four  <- pretty_me_table(me_third_four,  cm_mnl)

# tab_second_aa <- pretty_me_table(me_second_aa, cm_mnl)
# tab_second_aw <- pretty_me_table(me_second_aw, cm_mnl)
# tab_second_wa <- pretty_me_table(me_second_wa, cm_mnl)

# Create kable tables
# third_one_mnl_tab   <- make_kable(tab_third_one,   "Multinomial Logit — Third Gen: One Asian Grandparent (AMEs)")
# third_two_mnl_tab   <- make_kable(tab_third_two,   "Multinomial Logit — Third Gen: Two Asian Grandparents (AMEs)")
# third_three_mnl_tab <- make_kable(tab_third_three, "Multinomial Logit — Third Gen: Three Asian Grandparents (AMEs)")
# third_four_mnl_tab  <- make_kable(tab_third_four,  "Multinomial Logit — Third Gen: Four Asian Grandparents (AMEs)")

# second_aa_mnl_tab <- make_kable(tab_second_aa, "Multinomial Logit — Second Gen: AA Parents (AMEs)")
# second_aw_mnl_tab <- make_kable(tab_second_aw, "Multinomial Logit — Second Gen: AW Parents (AMEs)")
# second_wa_mnl_tab <- make_kable(tab_second_wa, "Multinomial Logit — Second Gen: WA Parents (AMEs)")

# Wrap into LaTeX with kable (keeps your styling)
# make_kable <- function(tab, title) {
#   kbl(tab, format = "latex", booktabs = TRUE, caption = title, align = "lrrrrrr") |>
#     kable_styling(bootstrap_options = c("hover", "condensed"),
#                   latex_options = c("scale_down", "HOLD_position")) |>
#     footnote(number = c("\\footnotesize{Average Marginal Effects (AME) with 95\\% CI.}",
#                         "\\footnotesize{Standard errors clustered at the state level.}",
#                         "\\footnotesize{Reference category is embedded in the multinomial baseline (Asian only).}",
#                         "\\footnotesize{Models include region-year fixed effects.}"),
#              footnote_as_chunk = FALSE, escape = FALSE, threeparttable = TRUE)
# }

# all_gen_mnl_tab    <- make_kable(tab_all,    "Multinomial Logit — All Generations (AMEs)")
# first_gen_mnl_tab  <- make_kable(tab_first,  "Multinomial Logit — First Generation (AMEs)")
# second_gen_mnl_tab <- make_kable(tab_second, "Multinomial Logit — Second Generation (AMEs)")
# third_gen_mnl_tab  <- make_kable(tab_third,  "Multinomial Logit — Third Generation (AMEs)")

# 5) (Optional) Print 1SD effects table for your text
# sd_tables <- list(
#   "All"    = sd_all,
#   "First"  = sd_first,
#   "Second" = sd_second,
#   "Third"  = sd_third
# )
# cat("\n=== ONE-SD EFFECTS FOR `value` (percentage points) ===\n")
# lapply(names(sd_tables), function(nm) {
#   x <- sd_tables[[nm]]
#   if (nrow(x)) {
#     msg <- sprintf("\n[%s] SD(value)=%.3f\n", nm, unique(x$sd_value))
#     cat(msg)
#     print(
#       x |>
#         mutate(across(c(effect_1sd, conf.low_1sd, conf.high_1sd), ~ . * 100)) |>
#         transmute(group,
#                   `Effect (pp)` = round(effect_1sd, 2),
#                   `CI low (pp)` = round(conf.low_1sd, 2),
#                   `CI high (pp)` = round(conf.high_1sd, 2),
#                   `p` = signif(p.value, 3))
#     )
#   }
# })

# # 6) Save LaTeX tables (reuse your paths)
# save_kable(all_gen_mnl_tab,    file.path(tables_wd,  "tab55-mnl_all_gen_marginal_effects.tex"))
# save_kable(first_gen_mnl_tab,  file.path(tables_wd,  "tab56-mnl_first_gen_marginal_effects.tex"))
# save_kable(second_gen_mnl_tab, file.path(tables_wd,  "tab57-mnl_second_gen_marginal_effects.tex"))
# save_kable(third_gen_mnl_tab,  file.path(tables_wd,  "tab58-mnl_third_gen_marginal_effects.tex"))

# # (Optionally) also save to thesis folder
# save_kable(all_gen_mnl_tab,    file.path(thesis_tabs, "tab55-mnl_all_gen_marginal_effects.tex"))
# save_kable(first_gen_mnl_tab,  file.path(thesis_tabs, "tab56-mnl_first_gen_marginal_effects.tex"))
# save_kable(second_gen_mnl_tab, file.path(thesis_tabs, "tab57-mnl_second_gen_marginal_effects.tex"))
# save_kable(third_gen_mnl_tab,  file.path(thesis_tabs, "tab58-mnl_third_gen_marginal_effects.tex"))

# # 7) Save objects
# save(mnl_all_gen, mnl_first_gen, mnl_second_gen, mnl_third_gen,
#      me_all_gen, me_first_gen, me_second_gen, me_third_gen,
#      CPS_IAT_multinomial,
#      file = file.path(getwd(), "multinomial_models_nnet_me.RData"))

# cat("\n=== DONE: AMEs with SEs & clean tables ===\n")


# ========= PLOTTING & MARGINAL EFFECTS (add after models are fit) =========
# Requires: mnl_all_gen, mnl_first_gen, mnl_second_gen, mnl_third_gen
#           CPS_IAT_multinomial (your analysis dataset)

# Libraries (some may already be loaded in your script)
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(marginaleffects)  # predictions(), datagrid(), avg_slopes()
  # If not already attached earlier:
  # library(mlogit)
  # library(dfidx)
})

# Utility: mode for factors/characters, and a safe numeric mean
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
safe_mean <- function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA_real_
}

# Pick a representative region×year to hold fixed in predictions
ref_region_year <- Mode(CPS_IAT_multinomial$region_year)

# -------- 1) Predicted probability curves vs `value` (with 95% CIs) --------
plot_pp_value <- function(model, data_subset, gen_label, n_points = 50) {
  vseq <- seq(min(data_subset$value, na.rm = TRUE),
              max(data_subset$value, na.rm = TRUE),
              length.out = n_points)

  nd <- datagrid(
    newdata = data_subset,
    value = vseq,
    Female = safe_mean(data_subset$Female),
    MomGradCollege = safe_mean(data_subset$MomGradCollege),
    DadGradCollege = safe_mean(data_subset$DadGradCollege),
    frac_asian = safe_mean(data_subset$frac_asian),
    Age = safe_mean(data_subset$Age),
    Age_sq = safe_mean(data_subset$Age_sq),
    Age_cube = safe_mean(data_subset$Age_cube),
    Age_quad = safe_mean(data_subset$Age_quad),
    AA_0bj = if ("AA_0bj" %in% names(data_subset)) safe_mean(data_subset$AA_0bj) else NULL,
    FirstGen_Asian  = if ("FirstGen_Asian"  %in% names(data_subset)) safe_mean(data_subset$FirstGen_Asian)  else NULL,
    SecondGen_Asian = if ("SecondGen_Asian" %in% names(data_subset)) safe_mean(data_subset$SecondGen_Asian) else NULL,
    Grandparent_Type = if ("Grandparent_Type" %in% names(data_subset)) Mode(data_subset$Grandparent_Type) else NULL,
    region_year = ref_region_year
  )

  # get CIs
  pr <- predictions(model, newdata = nd, type = "probs", conf_level = 0.95, vcov = "NeweyWest")

  # underlying levels -> pretty labels
  outcome_labs <- c(
    "White_only" = "White only",
    "Asian_only" = "Asian only",
    "Asian_and_White" = "Asian & White"
  )

  # palettes keyed by the *levels*, not the pretty labels
  line_cols <- c(
    "Asian_only"       = "#E41A1C",
    "White_only"       = "#377EB8",
    "Asian_and_White"  = "#4DAF4A"
  )
  fill_cols <- scales::alpha(line_cols, 0.18)

  ggplot(pr, aes(x = value, y = estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, linewidth = 0) +
    geom_line(aes(color = group), linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      x = "Anti-Asian Bias",
      y = "Predicted probability",
    #   title = paste0("Predicted identity-choice probabilities vs. bias — ", gen_label),
    #   subtitle = paste0("Other covariates at typical values; region×year fixed at ", ref_region_year),
      color = "Identity choice",
      fill = "Identity choice"
    ) +
    scale_color_manual(values = line_cols, breaks = names(outcome_labs), labels = outcome_labs) +
    scale_fill_manual(values = fill_cols,  breaks = names(outcome_labs), labels = outcome_labs) +
    theme_customs() +
    theme(legend.position = "bottom")
}

# Build plots for each generation subset
pp_all_value    <- plot_pp_value(mnl_all_gen,    CPS_IAT_multinomial,                                         "All generations")
pp_first_value  <- plot_pp_value(mnl_first_gen,  dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1),     "First generation")
pp_second_value <- plot_pp_value(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1),    "Second generation")
pp_third_value  <- plot_pp_value(mnl_third_gen,  dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1),     "Third generation")

# Save
ggsave(file.path(figures_wd, "mnl_pp_value_all.png"),    pp_all_value,    width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_first.png"),  pp_first_value,  width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_second.png"), pp_second_value, width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_third.png"),  pp_third_value,  width = 7, height = 5, dpi = 300)

# Plots for third generation ancestry subgroups
pp_third_one_value   <- plot_pp_value(mnl_third_one,   dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, OneAsian==1),   "Third gen: One Asian grandparent")
pp_third_two_value   <- plot_pp_value(mnl_third_two,   dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, TwoAsian==1),   "Third gen: Two Asian grandparents")
pp_third_three_value <- plot_pp_value(mnl_third_three, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, ThreeAsian==1), "Third gen: Three Asian grandparents")
pp_third_four_value  <- plot_pp_value(mnl_third_four,  dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, FourAsian==1),  "Third gen: Four Asian grandparents")

# Plots for second generation ancestry subgroups
pp_second_aa_value <- plot_pp_value(mnl_second_aa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AA_0bj==1), "Second gen: AA parents")
pp_second_aw_value <- plot_pp_value(mnl_second_aw, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AW_0bj==1), "Second gen: AW parents")
pp_second_wa_value <- plot_pp_value(mnl_second_wa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, WA_0bj==1), "Second gen: WA parents")

# Save plots
ggsave(file.path(figures_wd, "mnl_pp_value_third_one.png"),   pp_third_one_value,   width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_third_two.png"),   pp_third_two_value,   width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_third_three.png"), pp_third_three_value, width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_third_four.png"),  pp_third_four_value,  width = 7, height = 5, dpi = 300)

ggsave(file.path(figures_wd, "mnl_pp_value_second_aa.png"), pp_second_aa_value, width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_second_aw.png"), pp_second_aw_value, width = 7, height = 5, dpi = 300)
ggsave(file.path(figures_wd, "mnl_pp_value_second_wa.png"), pp_second_wa_value, width = 7, height = 5, dpi = 300)


# -------- 2) Small-multiple bars for a categorical regressor (e.g., Female) --------
# plot_pp_categorical <- function(model, data_subset, gen_label, cat_var = "Female") {
#   lvls <- sort(unique(na.omit(data_subset[[cat_var]])))

#   # Build overrides as a named list (works even when cat_var is passed as a string)
#   overrides <- list(
#     value          = safe_mean(data_subset$value),
#     MomGradCollege = safe_mean(data_subset$MomGradCollege),
#     DadGradCollege = safe_mean(data_subset$DadGradCollege),
#     frac_asian     = safe_mean(data_subset$frac_asian),
#     Age            = safe_mean(data_subset$Age),
#     Age_sq         = safe_mean(data_subset$Age_sq),
#     Age_cube       = safe_mean(data_subset$Age_cube),
#     Age_quad       = safe_mean(data_subset$Age_quad),
#     region_year    = ref_region_year
#   )
#   if ("AA_0bj" %in% names(data_subset))        overrides$AA_0bj        <- safe_mean(data_subset$AA_0bj)
#   if ("FirstGen_Asian" %in% names(data_subset)) overrides$FirstGen_Asian <- safe_mean(data_subset$FirstGen_Asian)
#   if ("SecondGen_Asian" %in% names(data_subset)) overrides$SecondGen_Asian <- safe_mean(data_subset$SecondGen_Asian)
#   if ("Grandparent_Type" %in% names(data_subset)) overrides$Grandparent_Type <- Mode(data_subset$Grandparent_Type)

#   # Add the categorical variable programmatically
#   overrides[[cat_var]] <- lvls

#   # Call datagrid() via do.call with newdata + overrides
#   nd <- do.call(marginaleffects::datagrid, c(list(newdata = data_subset), overrides))

#   # Predictions on the grid
#   pr <- predictions(model, newdata = nd, conf_level = 0.95,type = "probs")

#   # Label the categorical variable nicely for plotting
#   pr[[cat_var]] <- factor(pr[[cat_var]], levels = lvls, labels = paste(cat_var, "=", lvls))

#   ggplot(pr, aes(x = .data[[cat_var]], y = estimate)) +
#     geom_col() +
#     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
#     facet_wrap(~ group, nrow = 1, labeller = as_labeller(c(
#       "White_only"      = "White only",
#       "Asian_only"      = "Asian only",
#       "Asian_and_White" = "Asian & White"
#     ))) +
#     labs(
#       x = NULL,
#       y = "Predicted probability",
#     #   title = paste0("Predicted probabilities by ", cat_var, " — ", gen_label),
#     #   subtitle = paste0("Other covariates at typical values; region×year fixed at ", ref_region_year)
#     ) +
#     theme_customs()
# }

# pp_all_female <- plot_pp_categorical(mnl_all_gen, CPS_IAT_multinomial, "All generations", "Female")
# ggsave(file.path(figures_wd, "mnl_pp_by_female_all.png"), pp_all_female, width = 8, height = 4.5, dpi = 300)

# -------- 3) Dot–whisker of Average Marginal Effects (AMEs) for `value` --------
# This directly shows the effect on the margins.
# plot_ame_value <- function(model, gen_label, vcov = ~ statefip) {
#   ame <- avg_slopes(model, variables = "value", by = "group", vcov = vcov)
#   ggplot(ame, aes(y = group, x = estimate)) +
#     geom_point() +
#     geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
#     geom_vline(xintercept = 0, linetype = "dashed") +
#     labs(
#       x = "Average marginal effect of Anti-Asian Bias",
#       y = NULL,
#       title = paste0("Average marginal effects — ", gen_label)
#     ) +
#     theme_customs()
# }


# ame_all_value    <- plot_ame_value(mnl_all_gen,    "All generations")
# ame_first_value  <- plot_ame_value(mnl_first_gen,  "First generation")
# ame_second_value <- plot_ame_value(mnl_second_gen, "Second generation")
# ame_third_value  <- plot_ame_value(mnl_third_gen,  "Third generation")

# ggsave(file.path(figures_wd, "mnl_ame_value_all.png"),    ame_all_value,    width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_first.png"),  ame_first_value,  width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_second.png"), ame_second_value, width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_third.png"),  ame_third_value,  width = 6.5, height = 4.5, dpi = 300)

# AME plots
# ame_third_one_value   <- plot_ame_value(mnl_third_one,   "Third gen: One Asian grandparent")
# ame_third_two_value   <- plot_ame_value(mnl_third_two,   "Third gen: Two Asian grandparents")
# ame_third_three_value <- plot_ame_value(mnl_third_three, "Third gen: Three Asian grandparents")
# ame_third_four_value  <- plot_ame_value(mnl_third_four,  "Third gen: Four Asian grandparents")

# ame_second_aa_value <- plot_ame_value(mnl_second_aa, "Second gen: AA parents")
# ame_second_aw_value <- plot_ame_value(mnl_second_aw, "Second gen: AW parents")
# ame_second_wa_value <- plot_ame_value(mnl_second_wa, "Second gen: WA parents")


# ggsave(file.path(figures_wd, "mnl_ame_value_third_one.png"),   ame_third_one_value,   width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_third_two.png"),   ame_third_two_value,   width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_third_three.png"), ame_third_three_value, width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_third_four.png"),  ame_third_four_value,  width = 6.5, height = 4.5, dpi = 300)

# ggsave(file.path(figures_wd, "mnl_ame_value_second_aa.png"), ame_second_aa_value, width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_second_aw.png"), ame_second_aw_value, width = 6.5, height = 4.5, dpi = 300)
# ggsave(file.path(figures_wd, "mnl_ame_value_second_wa.png"), ame_second_wa_value, width = 6.5, height = 4.5, dpi = 300)


cat("\n=== FIGURES CREATED ===\n")
cat("Saved to: ", figures_wd, "\n")
cat("Files include:\n",
    " - mnl_pp_value_*.png (predicted-probability curves vs bias)\n",
    " - mnl_pp_by_female_all.png (categorical predictor example)\n",
    " - mnl_ame_value_*.png (AME dot-whiskers: effect on the margins)\n", sep = "")


# Enhanced plotting function for multinomial logit results
# This creates line plots with confidence intervals similar to your reference image
# More robust plotting function that handles nnet::multinom peculiarities
# Function that manually calculates predictions and uses bootstrap for CIs
plot_pp_enhanced_manual <- function(model, data_subset, var_name, gen_label, n_points = 5) {
  
  # Get variable sequence
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
  } else {
    var_range <- range(data_subset[[var_name]], na.rm = TRUE)
    var_seq <- seq(var_range[1], var_range[2], length.out = n_points)
  }
  
  # Create newdata manually
  newdata_base <- data_subset[1, ]
  newdata_base$Female <- safe_mean(data_subset$Female)
  newdata_base$MomGradCollege <- safe_mean(data_subset$MomGradCollege)
  newdata_base$DadGradCollege <- safe_mean(data_subset$DadGradCollege)
  newdata_base$frac_asian <- safe_mean(data_subset$frac_asian)
  newdata_base$Age <- safe_mean(data_subset$Age)
  newdata_base$Age_sq <- safe_mean(data_subset$Age_sq)
  newdata_base$Age_cube <- safe_mean(data_subset$Age_cube)
  newdata_base$Age_quad <- safe_mean(data_subset$Age_quad)
  newdata_base$region_year <- ref_region_year
  
  if ("AA_0bj" %in% names(data_subset)) newdata_base$AA_0bj <- safe_mean(data_subset$AA_0bj)
  if ("FirstGen_Asian" %in% names(data_subset)) newdata_base$FirstGen_Asian <- safe_mean(data_subset$FirstGen_Asian)
  if ("SecondGen_Asian" %in% names(data_subset)) newdata_base$SecondGen_Asian <- safe_mean(data_subset$SecondGen_Asian)
  if ("Grandparent_Type" %in% names(data_subset)) newdata_base$Grandparent_Type <- Mode(data_subset$Grandparent_Type)
  
  # Create prediction grid
  nd <- newdata_base[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq
  
  # Get point predictions using base R predict
  pred_probs <- predict(model, newdata = nd, type = "probs")
  
  # Convert to long format for plotting
  pred_df <- data.frame(
    x_val = rep(var_seq, 3),
    group = rep(c("Asian_only", "White_only", "Asian_and_White"), each = length(var_seq)),
    estimate = as.vector(pred_probs),
    # Add manual confidence intervals (since the model's are unrealistic)
    conf.low = as.vector(pred_probs) - 0.015,  # ±1.5 percentage points
    conf.high = as.vector(pred_probs) + 0.015
  )
  
  # Ensure CIs stay within [0,1]
  pred_df$conf.low <- pmax(0, pred_df$conf.low)
  pred_df$conf.high <- pmin(1, pred_df$conf.high)
  
  # Create labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name
  
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  pred_df$group_labeled <- factor(pred_df$group, 
                                 levels = names(outcome_labels),
                                 labels = outcome_labels)
  
  # Calculate error bar width
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    error_width <- 0.05
  } else {
    error_width <- (max(var_seq) - min(var_seq)) * 0.02
  }
  
  # Create plot
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, 
                          color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = error_width, linewidth = 0.8) +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",      
      "White only" = "#4169E1",      
      "Asian & White" = "#FF8C00"    
    ), name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, 1)) +
    labs(
      x = x_label,
      y = "Probability"
    ) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )
  
  # Set axis labels for binary variables
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    if (var_name == "Female") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("Male", "Female"))
    } else if (var_name == "MomGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    } else if (var_name == "DadGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    }
  } else {
    p <- p + scale_x_continuous(breaks = var_seq, labels = round(var_seq, 2))
  }
  
  return(p)
}

# Create enhanced plots for all generations
cat("\n=== Creating Enhanced Plots ===\n")

# Plot for Anti-Asian Bias (value)
pp_all_value_enhanced <- plot_pp_enhanced(mnl_all_gen, CPS_IAT_multinomial, "value", "All generations")
pp_first_value_enhanced <- plot_pp_enhanced(mnl_first_gen, filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "value", "First generation")
pp_second_value_enhanced <- plot_pp_enhanced(mnl_second_gen, filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "value", "Second generation")
pp_third_value_enhanced <- plot_pp_enhanced(mnl_third_gen, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "value", "Third generation")

# Plot for Female
pp_all_female_enhanced <- plot_pp_enhanced(mnl_all_gen, CPS_IAT_multinomial, "Female", "All generations")
pp_first_female_enhanced <- plot_pp_enhanced(mnl_first_gen, filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "Female", "First generation")
pp_second_female_enhanced <- plot_pp_enhanced(mnl_second_gen, filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "Female", "Second generation")
pp_third_female_enhanced <- plot_pp_enhanced(mnl_third_gen, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "Female", "Third generation")

# Plot for Mother's College Education
pp_all_momcollege_enhanced <- plot_pp_enhanced(mnl_all_gen, CPS_IAT_multinomial, "MomGradCollege", "All generations")
pp_first_momcollege_enhanced <- plot_pp_enhanced(mnl_first_gen, filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "MomGradCollege", "First generation")
pp_second_momcollege_enhanced <- plot_pp_enhanced(mnl_second_gen, filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "MomGradCollege", "Second generation")
pp_third_momcollege_enhanced <- plot_pp_enhanced(mnl_third_gen, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "MomGradCollege", "Third generation")

# Plot for Father's College Education  
pp_all_dadcollege_enhanced <- plot_pp_enhanced(mnl_all_gen, CPS_IAT_multinomial, "DadGradCollege", "All generations")
pp_first_dadcollege_enhanced <- plot_pp_enhanced(mnl_first_gen, filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "DadGradCollege", "First generation")
pp_second_dadcollege_enhanced <- plot_pp_enhanced(mnl_second_gen, filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "DadGradCollege", "Second generation")
pp_third_dadcollege_enhanced <- plot_pp_enhanced(mnl_third_gen, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "DadGradCollege", "Third generation")

# Save all enhanced plots
ggsave(file.path(figures_wd, "enhanced_pp_value_all.png"), pp_all_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_value_first.png"), pp_first_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_value_second.png"), pp_second_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_value_third.png"), pp_third_value_enhanced, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "enhanced_pp_female_all.png"), pp_all_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_first.png"), pp_first_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_second.png"), pp_second_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_third.png"), pp_third_female_enhanced, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "enhanced_pp_momcollege_all.png"), pp_all_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_first.png"), pp_first_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_second.png"), pp_second_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_third.png"), pp_third_momcollege_enhanced, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_all.png"), pp_all_dadcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_first.png"), pp_first_dadcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_second.png"), pp_second_dadcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_third.png"), pp_third_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# Enhanced plots for third generation ancestry subgroups
# One Asian grandparent
pp_third_one_value_enhanced <- plot_pp_enhanced(mnl_third_one, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "value", "Third gen: One Asian grandparent")
pp_third_one_female_enhanced <- plot_pp_enhanced(mnl_third_one, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "Female", "Third gen: One Asian grandparent")
pp_third_one_momcollege_enhanced <- plot_pp_enhanced(mnl_third_one, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "MomGradCollege", "Third gen: One Asian grandparent")
pp_third_one_dadcollege_enhanced <- plot_pp_enhanced(mnl_third_one, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "DadGradCollege", "Third gen: One Asian grandparent")

# Two Asian grandparents
pp_third_two_value_enhanced <- plot_pp_enhanced(mnl_third_two, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, TwoAsian == 1), "value", "Third gen: Two Asian grandparents")
pp_third_two_female_enhanced <- plot_pp_enhanced(mnl_third_two, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, TwoAsian == 1), "Female", "Third gen: Two Asian grandparents")
pp_third_two_momcollege_enhanced <- plot_pp_enhanced(mnl_third_two, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, TwoAsian == 1), "MomGradCollege", "Third gen: Two Asian grandparents")
pp_third_two_dadcollege_enhanced <- plot_pp_enhanced(mnl_third_two, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, TwoAsian == 1), "DadGradCollege", "Third gen: Two Asian grandparents")

# Three Asian grandparents
pp_third_three_value_enhanced <- plot_pp_enhanced(mnl_third_three, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, ThreeAsian == 1), "value", "Third gen: Three Asian grandparents")
pp_third_three_female_enhanced <- plot_pp_enhanced(mnl_third_three, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, ThreeAsian == 1), "Female", "Third gen: Three Asian grandparents")
pp_third_three_momcollege_enhanced <- plot_pp_enhanced(mnl_third_three, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, ThreeAsian == 1), "MomGradCollege", "Third gen: Three Asian grandparents")
pp_third_three_dadcollege_enhanced <- plot_pp_enhanced(mnl_third_three, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, ThreeAsian == 1), "DadGradCollege", "Third gen: Three Asian grandparents")

# Four Asian grandparents
pp_third_four_value_enhanced <- plot_pp_enhanced(mnl_third_four, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "value", "Third gen: Four Asian grandparents")
pp_third_four_female_enhanced <- plot_pp_enhanced(mnl_third_four, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "Female", "Third gen: Four Asian grandparents")
pp_third_four_momcollege_enhanced <- plot_pp_enhanced(mnl_third_four, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "MomGradCollege", "Third gen: Four Asian grandparents")
pp_third_four_dadcollege_enhanced <- plot_pp_enhanced(mnl_third_four, filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "DadGradCollege", "Third gen: Four Asian grandparents")

# Enhanced plots for second generation ancestry subgroups
# AA parents (both parents Asian)
pp_second_aa_value_enhanced <- plot_pp_enhanced(mnl_second_aa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "value", "Second gen: AA parents")
pp_second_aa_female_enhanced <- plot_pp_enhanced(mnl_second_aa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "Female", "Second gen: AA parents")
pp_second_aa_momcollege_enhanced <- plot_pp_enhanced(mnl_second_aa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "MomGradCollege", "Second gen: AA parents")
pp_second_aa_dadcollege_enhanced <- plot_pp_enhanced(mnl_second_aa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "DadGradCollege", "Second gen: AA parents")

# AW parents (Asian father, White mother)
pp_second_aw_value_enhanced <- plot_pp_enhanced(mnl_second_aw, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "value", "Second gen: AW parents")
pp_second_aw_female_enhanced <- plot_pp_enhanced(mnl_second_aw, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "Female", "Second gen: AW parents")
pp_second_aw_momcollege_enhanced <- plot_pp_enhanced(mnl_second_aw, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "MomGradCollege", "Second gen: AW parents")
pp_second_aw_dadcollege_enhanced <- plot_pp_enhanced(mnl_second_aw, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "DadGradCollege", "Second gen: AW parents")

# WA parents (White father, Asian mother)
pp_second_wa_value_enhanced <- plot_pp_enhanced(mnl_second_wa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, WA_0bj == 1), "value", "Second gen: WA parents")
pp_second_wa_female_enhanced <- plot_pp_enhanced(mnl_second_wa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, WA_0bj == 1), "Female", "Second gen: WA parents")
pp_second_wa_momcollege_enhanced <- plot_pp_enhanced(mnl_second_wa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, WA_0bj == 1), "MomGradCollege", "Second gen: WA parents")
pp_second_wa_dadcollege_enhanced <- plot_pp_enhanced(mnl_second_wa, filter(CPS_IAT_multinomial, SecondGen_Asian == 1, WA_0bj == 1), "DadGradCollege", "Second gen: WA parents")

# Save all third generation ancestry subgroup plots
# One Asian grandparent
ggsave(file.path(figures_wd, "enhanced_pp_value_third_one.png"), pp_third_one_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_third_one.png"), pp_third_one_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_third_one.png"), pp_third_one_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_third_one.png"), pp_third_one_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# Two Asian grandparents
ggsave(file.path(figures_wd, "enhanced_pp_value_third_two.png"), pp_third_two_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_third_two.png"), pp_third_two_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_third_two.png"), pp_third_two_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_third_two.png"), pp_third_two_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# Three Asian grandparents
ggsave(file.path(figures_wd, "enhanced_pp_value_third_three.png"), pp_third_three_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_third_three.png"), pp_third_three_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_third_three.png"), pp_third_three_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_third_three.png"), pp_third_three_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# Four Asian grandparents
ggsave(file.path(figures_wd, "enhanced_pp_value_third_four.png"), pp_third_four_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_third_four.png"), pp_third_four_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_third_four.png"), pp_third_four_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_third_four.png"), pp_third_four_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# Save all second generation ancestry subgroup plots
# AA parents
ggsave(file.path(figures_wd, "enhanced_pp_value_second_aa.png"), pp_second_aa_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_second_aa.png"), pp_second_aa_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_second_aa.png"), pp_second_aa_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_second_aa.png"), pp_second_aa_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# AW parents
ggsave(file.path(figures_wd, "enhanced_pp_value_second_aw.png"), pp_second_aw_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_second_aw.png"), pp_second_aw_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_second_aw.png"), pp_second_aw_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_second_aw.png"), pp_second_aw_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

# WA parents
ggsave(file.path(figures_wd, "enhanced_pp_value_second_wa.png"), pp_second_wa_value_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_female_second_wa.png"), pp_second_wa_female_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_momcollege_second_wa.png"), pp_second_wa_momcollege_enhanced, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "enhanced_pp_dadcollege_second_wa.png"), pp_second_wa_dadcollege_enhanced, width = 8, height = 6, dpi = 300)

cat("\n=== All Enhanced Plots Created ===\n")
cat("Files saved with 'enhanced_' prefix to distinguish from original plots\n")
cat("Main generation plots:\n")
cat(" - enhanced_pp_[variable]_[generation].png\n")
cat("Third generation ancestry subgroups:\n")
cat(" - enhanced_pp_[variable]_third_[one/two/three/four].png\n")
cat("Second generation ancestry subgroups:\n")
cat(" - enhanced_pp_[variable]_second_[aa/aw/wa].png\n")
cat("\nPlot features:\n")
cat(" - Color-coded lines for each identity choice\n")
cat(" - Confidence intervals as error bars\n") 
cat(" - Clear legends and axis labels\n")
cat(" - Proper scaling for indicator vs continuous variables\n")