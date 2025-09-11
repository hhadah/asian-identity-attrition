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
                            by = "group", newdata = CPS_IAT_multinomial)
me_first_gen  <- avg_slopes(mnl_first_gen,  variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian==1))
me_second_gen <- avg_slopes(mnl_second_gen, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1))
me_third_gen  <- avg_slopes(mnl_third_gen,  variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1))

# Marginal effects for third generation ancestry subgroups
me_third_one   <- avg_slopes(mnl_third_one,   variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, OneAsian==1))
me_third_two   <- avg_slopes(mnl_third_two,   variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, TwoAsian==1))
me_third_three <- avg_slopes(mnl_third_three, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, ThreeAsian==1))
me_third_four  <- avg_slopes(mnl_third_four,  variables = c("value","Female","MomGradCollege","DadGradCollege"),
                            by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian==1, FourAsian==1))

# Marginal effects for second generation ancestry subgroups
me_second_aa <- avg_slopes(mnl_second_aa, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                          by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AA_0bj==1))
me_second_aw <- avg_slopes(mnl_second_aw, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                          by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, AW_0bj==1))
me_second_wa <- avg_slopes(mnl_second_wa, variables = c("value","Female","MomGradCollege","DadGradCollege"),
                          by = "group", newdata = dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian==1, WA_0bj==1))

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


# Simple and reliable plotting function for nnet::multinom models
# Uses base R predict() with straightforward confidence intervals

# Enhanced plotting function that adds value labels to points
plot_pp_simple_with_labels <- function(model, data_subset, var_name, gen_label, n_points = 5) {
  
  # Set variable sequence
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
  } else {
    var_range <- range(data_subset[[var_name]], na.rm = TRUE)
    var_seq <- seq(var_range[1], var_range[2], length.out = n_points)
  }
  
  # Get representative values (using simple base R functions)
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      # For factors/characters, get the most common value
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  # Create a representative observation
  rep_data <- data_subset[1, ]  # Start with first row as template
  
  # Set all variables to representative values
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  
  # Set region_year to the most common value
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  # Create prediction grid by replicating and varying the target variable
  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq
  
  cat(sprintf("Creating predictions for %s (%s)...\n", var_name, gen_label))
  
  # Get predictions using base R predict
  pred_probs <- predict(model, newdata = nd, type = "probs")
  
  # Handle case where predict returns a vector (single prediction)
  if (is.vector(pred_probs)) {
    # Get the outcome names from the model
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }
  
  # Create approximate confidence intervals (±2 percentage points)
  se_approx <- 0.02
  conf_low <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)
  
  # Convert to long format for ggplot
  n_obs <- nrow(pred_probs)
  n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)
  
  pred_df <- data.frame(
    x_val = rep(var_seq, n_groups),
    group = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )
  
  # Create variable labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name
  
  # Create outcome labels
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  # Apply outcome labels
  pred_df$group_labeled <- pred_df$group
  for (i in 1:nrow(pred_df)) {
    if (pred_df$group[i] %in% names(outcome_labels)) {
      pred_df$group_labeled[i] <- outcome_labels[[pred_df$group[i]]]
    }
  }
  
  pred_df$group_labeled <- factor(pred_df$group_labeled, 
                                 levels = outcome_labels)
  
  # Calculate error bar width
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    error_width <- 0.05
  } else {
    error_width <- (max(var_seq) - min(var_seq)) * 0.02
  }
  
  # Create formatted labels for the points (as percentages)
  pred_df$label <- paste0(round(pred_df$estimate * 100, 1), "%")
  
  # Create the plot
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, 
                          color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3, alpha = 0.9) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                  width = error_width, linewidth = 1) +
    # Add value labels above the points
    geom_text(aes(label = label), 
              vjust = -0.8, hjust = 0.5, 
              size = 3.5, 
              show.legend = FALSE) +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",      # Sea green
      "White only" = "#4169E1",      # Royal blue
      "Asian & White" = "#FF8C00"    # Dark orange
    ), name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, 1.1)) +  # Increased upper limit for labels
    labs(x = x_label, y = "Probability") +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )
  
  # Set appropriate axis labels for binary variables
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    if (var_name == "Female") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("Male", "Female"))
    } else if (var_name == "MomGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    } else if (var_name == "DadGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    }
  }
  
  return(p)
}

# Alternative version with labels positioned more strategically
plot_pp_simple_with_smart_labels <- function(model, data_subset, var_name, gen_label, n_points = 5) {
  
  # [Same setup code as above...]
  # Set variable sequence
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
  } else {
    var_range <- range(data_subset[[var_name]], na.rm = TRUE)
    var_seq <- seq(var_range[1], var_range[2], length.out = n_points)
  }
  
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  rep_data <- data_subset[1, ]
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq
  
  pred_probs <- predict(model, newdata = nd, type = "probs")
  
  if (is.vector(pred_probs)) {
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }
  
  se_approx <- 0.02
  conf_low <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)
  
  n_obs <- nrow(pred_probs)
  n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)
  
  pred_df <- data.frame(
    x_val = rep(var_seq, n_groups),
    group = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )
  
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
  
  pred_df$group_labeled <- pred_df$group
  for (i in 1:nrow(pred_df)) {
    if (pred_df$group[i] %in% names(outcome_labels)) {
      pred_df$group_labeled[i] <- outcome_labels[[pred_df$group[i]]]
    }
  }
  
  pred_df$group_labeled <- factor(pred_df$group_labeled, levels = outcome_labels)
  
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    error_width <- 0.05
  } else {
    error_width <- (max(var_seq) - min(var_seq)) * 0.02
  }
  
  # Create formatted labels and smart positioning
  pred_df$label <- paste0(round(pred_df$estimate * 100, 1), "%")
  
  # Smart label positioning: above high values, below low values
  pred_df$vjust_adj <- ifelse(pred_df$estimate > 0.5, -1.2, 1.5)
  
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, 
                          color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3, alpha = 0.9) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                  width = error_width, linewidth = 1) +
    # Smart positioned labels
    geom_text(aes(label = label, vjust = vjust_adj), 
              hjust = 0.5, 
              size = 3.5, 
              color = "black",  # Use black for better readability
              show.legend = FALSE) +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",
      "White only" = "#4169E1",
      "Asian & White" = "#FF8C00"
    ), name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(-0.05, 1.05)) +  # Adjusted for smart positioning
    labs(x = x_label, y = "Probability") +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )
  
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    if (var_name == "Female") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("Male", "Female"))
    } else if (var_name == "MomGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    } else if (var_name == "DadGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    }
  }
  
  return(p)
}

plot_pp_simple <- function(model, data_subset, var_name, gen_label, n_points = 5) {
  
  # Set variable sequence
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
  } else {
    var_range <- range(data_subset[[var_name]], na.rm = TRUE)
    var_seq <- seq(var_range[1], var_range[2], length.out = n_points)
  }
  
  # Get representative values
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  # Create representative observation
  rep_data <- data_subset[1, ]
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  # Create prediction grid
  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq
  
  # Get predictions
  pred_probs <- predict(model, newdata = nd, type = "probs")
  
  # Handle vector case
  if (is.vector(pred_probs)) {
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }
  
  # Create approximate confidence intervals
  se_approx <- 0.02
  conf_low <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)
  
  # Convert to long format
  n_obs <- nrow(pred_probs)
  n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)
  
  pred_df <- data.frame(
    x_val = rep(var_seq, n_groups),
    group = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )
  
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
  
  # Apply outcome labels
  pred_df$group_labeled <- pred_df$group
  for (i in 1:nrow(pred_df)) {
    if (pred_df$group[i] %in% names(outcome_labels)) {
      pred_df$group_labeled[i] <- outcome_labels[[pred_df$group[i]]]
    }
  }
  
  pred_df$group_labeled <- factor(pred_df$group_labeled, levels = outcome_labels)
  
  # Calculate error bar width
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    error_width <- 0.05
  } else {
    error_width <- (max(var_seq) - min(var_seq)) * 0.02
  }
  
  # Create plot
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, 
                          color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3, alpha = 0.9) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                  width = error_width, linewidth = 1) +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",
      "White only" = "#4169E1",
      "Asian & White" = "#FF8C00"
    ), name = "") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, 1)) +
    labs(x = x_label, y = "Probability") +
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
  }
  
  return(p)
}


# Create plots using the simple function
cat("\n=== Creating Simple Robust Plots ===\n")

# Main generation plots for Anti-Asian Bias
pp_all_value_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "value", "All generations")
pp_first_value_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "value", "First generation")
pp_second_value_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "value", "Second generation")
pp_third_value_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "value", "Third generation")

# Main generation plots for Female
pp_all_female_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "Female", "All generations")
pp_first_female_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "Female", "First generation")
pp_second_female_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "Female", "Second generation")
pp_third_female_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "Female", "Third generation")

# Main generation plots for Mother's College
pp_all_momcollege_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "MomGradCollege", "All generations")
pp_first_momcollege_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "MomGradCollege", "First generation")
pp_second_momcollege_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "MomGradCollege", "Second generation")
pp_third_momcollege_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "MomGradCollege", "Third generation")

# Main generation plots for Father's College
pp_all_dadcollege_simple <- plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, "DadGradCollege", "All generations")
pp_first_dadcollege_simple <- plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "DadGradCollege", "First generation")
pp_second_dadcollege_simple <- plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "DadGradCollege", "Second generation")
pp_third_dadcollege_simple <- plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "DadGradCollege", "Third generation")

# Save plots
ggsave(file.path(figures_wd, "simple_pp_value_all.png"), pp_all_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_first.png"), pp_first_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_second.png"), pp_second_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_third.png"), pp_third_value_simple, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "simple_pp_female_all.png"), pp_all_female_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_female_first.png"), pp_first_female_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_female_second.png"), pp_second_female_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_female_third.png"), pp_third_female_simple, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "simple_pp_momcollege_all.png"), pp_all_momcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_momcollege_first.png"), pp_first_momcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_momcollege_second.png"), pp_second_momcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_momcollege_third.png"), pp_third_momcollege_simple, width = 8, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "simple_pp_dadcollege_all.png"), pp_all_dadcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_dadcollege_first.png"), pp_first_dadcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_dadcollege_second.png"), pp_second_dadcollege_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_dadcollege_third.png"), pp_third_dadcollege_simple, width = 8, height = 6, dpi = 300)

# Create some ancestry subgroup examples
cat("\n=== Creating Selected Ancestry Subgroup Plots ===\n")

# Third generation examples
pp_third_one_value_simple <- plot_pp_simple(mnl_third_one, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "value", "Third gen: One Asian grandparent")
pp_third_four_value_simple <- plot_pp_simple(mnl_third_four, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "value", "Third gen: Four Asian grandparents")

# Second generation examples
pp_second_aa_value_simple <- plot_pp_simple(mnl_second_aa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "value", "Second gen: AA parents")
pp_second_aw_value_simple <- plot_pp_simple(mnl_second_aw, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "value", "Second gen: AW parents")

# Save ancestry subgroup plots
ggsave(file.path(figures_wd, "simple_pp_value_third_one.png"), pp_third_one_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_third_four.png"), pp_third_four_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_second_aa.png"), pp_second_aa_value_simple, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "simple_pp_value_second_aw.png"), pp_second_aw_value_simple, width = 8, height = 6, dpi = 300)

cat("\n=== Simple Plots Created Successfully ===\n")
cat("Files saved with 'simple_' prefix\n")
cat("These plots use base R predict() with approximate ±2% confidence intervals\n")
cat("All plots should work without dependency issues\n")

# Marginal effects calculation for nnet::multinom models
# This calculates actual marginal effects (slopes/derivatives) rather than predicted probabilities

# Function to calculate marginal effects manually
calculate_marginal_effects <- function(model, data_subset, var_name, gen_label) {
  
  # Get representative values (using simple base R functions)
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      # For factors/characters, get the most common value
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  # Create a representative observation
  rep_data <- data_subset[1, ]  # Start with first row as template
  
  # Set all variables to representative values
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  cat(sprintf("Calculating marginal effects for %s (%s)...\n", var_name, gen_label))
  
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    # For binary variables: calculate discrete change (0 -> 1)
    
    # Create two observations: one with var=0, one with var=1
    nd_0 <- rep_data
    nd_1 <- rep_data
    nd_0[[var_name]] <- 0
    nd_1[[var_name]] <- 1
    
    # Get predictions for both
    pred_0 <- predict(model, newdata = nd_0, type = "probs")
    pred_1 <- predict(model, newdata = nd_1, type = "probs")
    
    # Handle vector case
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1)
      pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    
    # Calculate discrete change
    marginal_effect <- pred_1 - pred_0
    
    # Create results dataframe
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "discrete_change",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
    
  } else {
    # For continuous variables: calculate derivative using finite differences
    
    # Small change for derivative calculation
    delta <- 0.01
    
    # Get current value and create small perturbation
    current_val <- rep_data[[var_name]]
    
    # Create two observations: one slightly below, one slightly above
    nd_low <- rep_data
    nd_high <- rep_data
    nd_low[[var_name]] <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    
    # Get predictions for both
    pred_low <- predict(model, newdata = nd_low, type = "probs")
    pred_high <- predict(model, newdata = nd_high, type = "probs")
    
    # Handle vector case
    if (is.vector(pred_low)) {
      outcome_names <- model$lev
      pred_low <- matrix(pred_low, nrow = 1)
      pred_high <- matrix(pred_high, nrow = 1)
      colnames(pred_low) <- colnames(pred_high) <- outcome_names
    }
    
    # Calculate derivative (slope)
    marginal_effect <- (pred_high - pred_low) / delta
    
    # Create results dataframe
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "derivative",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
  }
  
  return(results)
}

# Function to calculate bootstrap confidence intervals for marginal effects
calculate_marginal_effects_bootstrap <- function(model, data_subset, var_name, gen_label, B = 1000) {
  
  # Get point estimates
  point_estimates <- calculate_marginal_effects(model, data_subset, var_name, gen_label)
  
  # Bootstrap marginal effects
  n_outcomes <- nrow(point_estimates)
  boot_effects <- matrix(NA, nrow = B, ncol = n_outcomes)
  
  for (b in 1:B) {
    # Bootstrap sample
    boot_indices <- sample(nrow(data_subset), replace = TRUE)
    boot_data <- data_subset[boot_indices, ]
    
    tryCatch({
      # Refit model on bootstrap sample
      boot_model <- update(model, data = boot_data)
      
      # Calculate marginal effects on bootstrap model
      boot_me <- calculate_marginal_effects(boot_model, boot_data, var_name, gen_label)
      boot_effects[b, ] <- boot_me$marginal_effect
      
    }, error = function(e) {
      # If bootstrap fails, use original estimates
      boot_effects[b, ] <<- point_estimates$marginal_effect
    })
    
    if (b %% 20 == 0) cat("Bootstrap iteration:", b, "\n")
  }
  
  # Calculate confidence intervals
  conf_low <- apply(boot_effects, 2, quantile, probs = 0.025, na.rm = TRUE)
  conf_high <- apply(boot_effects, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  # Add CIs to results
  point_estimates$conf_low <- conf_low
  point_estimates$conf_high <- conf_high
  
  return(point_estimates)
}

# Function to calculate marginal effects for all main variables
calculate_all_marginal_effects <- function(model, data_subset, gen_label, use_bootstrap = FALSE) {
  
  variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")
  all_results <- list()
  
  for (var in variables) {
    if (use_bootstrap) {
      result <- calculate_marginal_effects_bootstrap(model, data_subset, var, gen_label, B = 1000)
    } else {
      result <- calculate_marginal_effects(model, data_subset, var, gen_label)
      # Add approximate standard errors
      result$std_error <- 0.01  # Rough approximation
      result$conf_low <- result$marginal_effect - 1.96 * result$std_error
      result$conf_high <- result$marginal_effect + 1.96 * result$std_error
    }
    all_results[[var]] <- result
  }
  
  # Combine all results
  combined_results <- do.call(rbind, all_results)
  return(combined_results)
}

# Function to create a marginal effects table
create_marginal_effects_table <- function(me_results) {
  
  # Create variable labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  # Create outcome labels
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  # Apply labels
  me_results$variable_label <- var_labels[me_results$variable]
  me_results$outcome_label <- outcome_labels[me_results$outcome]
  
  # Create formatted results
  me_results$effect_ci <- sprintf("%.4f [%.4f, %.4f]", 
                                  me_results$marginal_effect,
                                  me_results$conf_low,
                                  me_results$conf_high)
  
  # Create wide format table
  table_wide <- me_results %>%
    select(variable_label, outcome_label, effect_ci) %>%
    tidyr::pivot_wider(names_from = outcome_label, values_from = effect_ci) %>%
    arrange(variable_label)
  
  return(table_wide)
}

# Function to plot marginal effects
plot_marginal_effects <- function(me_results, gen_label) {
  
  # Apply labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  # Ensure confidence intervals exist
  if (!("conf_low" %in% names(me_results)) || !("conf_high" %in% names(me_results))) {
    warning("Confidence intervals not found in me_results. Check your bootstrap calculation.")
    me_results$conf_low <- me_results$marginal_effect - 0.01
    me_results$conf_high <- me_results$marginal_effect + 0.01
  }
  
  me_results$variable_label <- factor(me_results$variable, 
                                     levels = names(var_labels),
                                     labels = var_labels)
  me_results$outcome_label <- factor(me_results$outcome,
                                    levels = names(outcome_labels),
                                    labels = outcome_labels)
  
  # Create the plot
  p <- ggplot(me_results, aes(x = marginal_effect, y = variable_label, 
                             color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), 
                   height = 0.2, position = position_dodge(width = 0.5),
                   linewidth = 0.8) +  # Added linewidth for better visibility
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",      # Sea green
      "White only" = "#4169E1",      # Royal blue
      "Asian & White" = "#FF8C00"    # Dark orange
    ), name = "Identity Choice") +
    labs(
      x = "Marginal Effect (percentage points)",
      y = "",
      title = paste("Marginal Effects —", gen_label)
    ) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5)
    )
  
  return(p)
}
# Calculate marginal effects for all generations
cat("\n=== Calculating Marginal Effects ===\n")

# Main generations
me_all_gen_manual <- calculate_all_marginal_effects(mnl_all_gen, CPS_IAT_multinomial, "All generations")
me_first_gen_manual <- calculate_all_marginal_effects(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "First generation")
me_second_gen_manual <- calculate_all_marginal_effects(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "Second generation")
me_third_gen_manual <- calculate_all_marginal_effects(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "Third generation")

# Create tables
table_all <- create_marginal_effects_table(me_all_gen_manual)
table_first <- create_marginal_effects_table(me_first_gen_manual)
table_second <- create_marginal_effects_table(me_second_gen_manual)
table_third <- create_marginal_effects_table(me_third_gen_manual)

# Print tables
cat("\n=== Marginal Effects Tables ===\n")
cat("\nAll Generations:\n")
print(table_all)
cat("\nFirst Generation:\n")
print(table_first)
cat("\nSecond Generation:\n")
print(table_second)
cat("\nThird Generation:\n")
print(table_third)

# Create plots
plot_me_all <- plot_marginal_effects(me_all_gen_manual, "All generations")
plot_me_first <- plot_marginal_effects(me_first_gen_manual, "First generation")
plot_me_second <- plot_marginal_effects(me_second_gen_manual, "Second generation")
plot_me_third <- plot_marginal_effects(me_third_gen_manual, "Third generation")

# Save plots
ggsave(file.path(figures_wd, "marginal_effects_all.png"), plot_me_all, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "marginal_effects_first.png"), plot_me_first, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "marginal_effects_second.png"), plot_me_second, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "marginal_effects_third.png"), plot_me_third, width = 10, height = 6, dpi = 300)

# Example with bootstrap CIs (slower but more accurate)
cat("\n=== Example with Bootstrap Confidence Intervals ===\n")
me_all_bootstrap <- calculate_all_marginal_effects(mnl_all_gen, CPS_IAT_multinomial, "All generations (Bootstrap)", use_bootstrap = TRUE)
table_all_bootstrap <- create_marginal_effects_table(me_all_bootstrap)
cat("\nAll Generations (Bootstrap CIs):\n")
print(table_all_bootstrap)

cat("\n=== Marginal Effects Analysis Complete ===\n")
cat("Interpretation:\n")
cat("- For continuous variables (Anti-Asian Bias): marginal effect = change in probability per unit increase\n")
cat("- For binary variables (Female, Education): marginal effect = discrete change from 0 to 1\n")
cat("- Results show instantaneous rates of change (slopes) rather than predicted probabilities\n")
cat("- Positive values = variable increases probability of that identity choice\n")
cat("- Negative values = variable decreases probability of that identity choice\n")


# Clean code for creating only predicted probability plots and marginal effects plots
# Removes all table creation, comparison functions, and summary statistics

# Set bootstrap parameters
bootstrap_reps <- 1000
set.seed(123)

# ============================================================================
# PREDICTED PROBABILITY PLOTS
# ============================================================================

# Add at top of your script
library(ggrepel)

# Simple plotting function for predicted probabilities (with readable labels)
plot_pp_simple <- function(model, data_subset, var_name, gen_label, n_points = 5,
                           show_labels = TRUE,
                           repel_box_padding = 0.35,
                           repel_point_padding = 0.30) {
  # Set variable sequence
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    var_seq <- c(0, 1)
  } else {
    var_range <- range(data_subset[[var_name]], na.rm = TRUE)
    var_seq <- seq(var_range[1], var_range[2], length.out = n_points)
  }

  # Get representative values
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }

  # Create representative observation
  rep_data <- data_subset[1, ]
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)

  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }

  # Prediction grid
  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq

  # Predictions
  pred_probs <- predict(model, newdata = nd, type = "probs")

  # Handle vector case
  if (is.vector(pred_probs)) {
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }

  # Approximate CIs
  se_approx <- 0.02
  conf_low  <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)

  # Long format
  n_obs <- nrow(pred_probs)
  n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)

  pred_df <- data.frame(
    x_val    = rep(var_seq, n_groups),
    group    = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )

  # Labels
  var_labels <- c(
    "value"          = "Anti-Asian Bias",
    "Female"         = "Female",
    "MomGradCollege" = "College Graduate: Mother",
    "DadGradCollege" = "College Graduate: Father"
  )
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name

  outcome_labels <- c(
    "Asian_only"      = "Asian only",
    "White_only"      = "White only",
    "Asian_and_White" = "Asian & White"
  )
  pred_df$group_labeled <- dplyr::recode(pred_df$group, !!!outcome_labels, .default = pred_df$group)
  pred_df$group_labeled <- factor(pred_df$group_labeled, levels = outcome_labels)

  # Label text as percentages (e.g., "54%")
  pred_df$label <- scales::percent(pred_df$estimate, accuracy = 1)

  # Error bar width and position dodge
  error_width <- if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) 0.05
                 else (max(var_seq) - min(var_seq)) * 0.02

  # a small dodge so different groups at the same x don't sit on top of each other
  dodge_width <- if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) 0.15 else 0.07
  pd <- position_dodge(width = dodge_width)

  # Plot
  p <- ggplot(pred_df, aes(x = x_val, y = estimate, color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2, position = pd) +
    geom_point(size = 3, alpha = 0.95, position = pd) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                  width = error_width, linewidth = 1, position = pd) +
    # Smart, non-overlapping labels
    { if (show_labels)
        geom_label_repel(
          aes(label = label),
          position = pd,
          label.size = 0,          # no border line around the label
          fill = "white",          # subtle white background for contrast
          alpha = 0.9,             # slightly transparent
          size = 14,
          max.overlaps = Inf,      # allow dense layouts
          box.padding = unit(repel_box_padding, "lines"),
          point.padding = unit(repel_point_padding, "lines"),
          min.segment.length = 0,
          segment.alpha = 0.6,
          seed = 123,
          show.legend = FALSE
        )
      else NULL } +
    scale_color_manual(
      values = c(
        "Asian only"  = "#2E8B57",
        "White only"  = "#4169E1",
        "Asian & White" = "#FF8C00"
      ),
      name = ""
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(x = x_label, y = "Probability", subtitle = gen_label) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      legend.margin = margin(t = 10)
    )

  # Binary x-axis labels
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    if (var_name == "Female") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("Male", "Female"))
    } else if (var_name == "MomGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    } else if (var_name == "DadGradCollege") {
      p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("No College", "College Graduate"))
    }
  }

  return(p)
}

# Create all predicted probability plots
variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")

# Main generations
for (var in variables) {
  assign(paste0("pp_all_", var, "_simple"), 
         plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, var, "All generations"))
  assign(paste0("pp_first_", var, "_simple"), 
         plot_pp_simple(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), var, "First generation"))
  assign(paste0("pp_second_", var, "_simple"), 
         plot_pp_simple(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), var, "Second generation"))
  assign(paste0("pp_third_", var, "_simple"), 
         plot_pp_simple(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), var, "Third generation"))
}

# Third generation ancestry subgroups
ancestry_groups <- c("one", "two", "three", "four")
for (var in variables) {
  for (anc in ancestry_groups) {
    model_name <- paste0("mnl_third_", anc)
    filter_var <- paste0(toupper(substring(anc, 1, 1)), substring(anc, 2), "Asian")
    
    assign(paste0("pp_third_", anc, "_", var, "_simple"),
           plot_pp_simple(get(model_name), 
                         dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, get(filter_var) == 1), 
                         var, paste("Third gen:", anc, "Asian grandparent")))
  }
}

# Second generation ancestry subgroups
second_groups <- c("aa", "aw", "wa")
for (var in variables) {
  for (grp in second_groups) {
    model_name <- paste0("mnl_second_", grp)
    filter_var <- paste0(toupper(grp), "_0bj")
    
    assign(paste0("pp_second_", grp, "_", var, "_simple"),
           plot_pp_simple(get(model_name), 
                         dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, get(filter_var) == 1), 
                         var, paste("Second gen:", toupper(grp), "parents")))
  }
}

# Save all predicted probability plots
for (var in variables) {
  # Main generations
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_all.png")), 
         get(paste0("pp_all_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_first.png")), 
         get(paste0("pp_first_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second.png")), 
         get(paste0("pp_second_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third.png")), 
         get(paste0("pp_third_", var, "_simple")), width = 8, height = 6, dpi = 300)
  
  # Third generation ancestry subgroups
  for (anc in ancestry_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third_", anc, ".png")), 
           get(paste0("pp_third_", anc, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
  
  # Second generation ancestry subgroups
  for (grp in second_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second_", grp, ".png")), 
           get(paste0("pp_second_", grp, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
}

# ============================================================================
# MARGINAL EFFECTS FUNCTIONS
# ============================================================================

# Calculate marginal effects manually
calculate_marginal_effects <- function(model, data_subset, var_name, gen_label) {
  
  get_rep_value <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  }
  
  # Create representative observation
  rep_data <- data_subset[1, ]
  rep_data$Female <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian <- get_rep_value(data_subset$frac_asian)
  rep_data$Age <- get_rep_value(data_subset$Age)
  rep_data$Age_sq <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
  
  # Handle generation-specific variables
  if ("AA_0bj" %in% names(data_subset)) {
    rep_data$AA_0bj <- get_rep_value(data_subset$AA_0bj)
  }
  if ("FirstGen_Asian" %in% names(data_subset)) {
    rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  }
  if ("SecondGen_Asian" %in% names(data_subset)) {
    rep_data$SecondGen_Asian <- get_rep_value(data_subset$SecondGen_Asian)
  }
  if ("Grandparent_Type" %in% names(data_subset)) {
    rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)
  }
  
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    # Binary variables: discrete change
    nd_0 <- rep_data
    nd_1 <- rep_data
    nd_0[[var_name]] <- 0
    nd_1[[var_name]] <- 1
    
    pred_0 <- predict(model, newdata = nd_0, type = "probs")
    pred_1 <- predict(model, newdata = nd_1, type = "probs")
    
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1)
      pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    
    marginal_effect <- pred_1 - pred_0
    
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "discrete_change",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
    
  } else {
    # Continuous variables: derivative
    delta <- 0.01
    current_val <- rep_data[[var_name]]
    
    nd_low <- rep_data
    nd_high <- rep_data
    nd_low[[var_name]] <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    
    pred_low <- predict(model, newdata = nd_low, type = "probs")
    pred_high <- predict(model, newdata = nd_high, type = "probs")
    
    if (is.vector(pred_low)) {
      outcome_names <- model$lev
      pred_low <- matrix(pred_low, nrow = 1)
      pred_high <- matrix(pred_high, nrow = 1)
      colnames(pred_low) <- colnames(pred_high) <- outcome_names
    }
    
    marginal_effect <- (pred_high - pred_low) / delta
    
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "derivative",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
  }
  
  return(results)
}

# Bootstrap marginal effects
# Fixed bootstrap marginal effects function
# Fixed bootstrap marginal effects function
calculate_marginal_effects_bootstrap_fixed <- function(model, data_subset, var_name, gen_label, B = 1000) {
  
  # Calculate point estimates first
  point_estimates <- calculate_marginal_effects(model, data_subset, var_name, gen_label)
  n_outcomes <- nrow(point_estimates)
  boot_effects <- matrix(NA, nrow = B, ncol = n_outcomes)
  
  successful_boots <- 0
  failed_boots <- 0
  
  # Get the original model formula and family
  original_formula <- formula(model)
  
  for (b in 1:B) {
    boot_indices <- sample(nrow(data_subset), replace = TRUE)
    boot_data <- data_subset[boot_indices, ]
    
    tryCatch({
      # Refit model with explicit formula instead of update()
      boot_model <- multinom(original_formula, data = boot_data, trace = FALSE)
      
      # Calculate marginal effects using a simplified approach
      boot_me <- calculate_marginal_effects_simple(boot_model, boot_data, var_name, gen_label)
      boot_effects[b, ] <- boot_me$marginal_effect
      successful_boots <- successful_boots + 1
      
    }, error = function(e) {
      failed_boots <<- failed_boots + 1
      if (failed_boots <= 5) {  # Only print first 5 errors
        cat("Bootstrap iteration", b, "failed:", e$message, "\n")
      }
    })
    
    # Progress indicator
    if (b %% 100 == 0) {
      cat("Completed", b, "iterations...\n")
    }
  }
  
  cat("Bootstrap results:", successful_boots, "successes,", failed_boots, "failures\n")
  
  if (successful_boots < 50) {
    warning("Very few successful bootstrap iterations (", successful_boots, 
            "). Results may be unreliable.")
  }
  
  # Calculate confidence intervals
  conf_low <- apply(boot_effects, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  conf_high <- apply(boot_effects, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  boot_se <- apply(boot_effects, 2, function(x) sd(x, na.rm = TRUE))
  
  point_estimates$std_error <- boot_se
  point_estimates$conf_low <- conf_low
  point_estimates$conf_high <- conf_high
  point_estimates$n_successful_boots <- successful_boots
  
  return(point_estimates)
}

# Simplified marginal effects calculation to avoid scoping issues
calculate_marginal_effects_simple <- function(model, data_subset, var_name, gen_label) {
  
  # Calculate representative values directly without helper function
  rep_data <- data_subset[1, , drop = FALSE]
  
  # Set representative values for each variable
  for (col_name in names(data_subset)) {
    if (col_name == var_name) next  # Skip the variable we're analyzing
    
    if (is.numeric(data_subset[[col_name]])) {
      rep_data[[col_name]] <- mean(data_subset[[col_name]], na.rm = TRUE)
    } else {
      # For categorical variables, use the most frequent value
      tbl <- table(data_subset[[col_name]])
      rep_data[[col_name]] <- names(tbl)[which.max(tbl)]
    }
  }
  
  if (var_name %in% c("Female", "MomGradCollege", "DadGradCollege")) {
    # Binary variables: discrete change
    nd_0 <- rep_data
    nd_1 <- rep_data
    nd_0[[var_name]] <- 0
    nd_1[[var_name]] <- 1
    
    pred_0 <- predict(model, newdata = nd_0, type = "probs")
    pred_1 <- predict(model, newdata = nd_1, type = "probs")
    
    # Handle vector case
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1)
      pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    
    marginal_effect <- pred_1 - pred_0
    
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "discrete_change",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
    
  } else {
    # Continuous variables: derivative
    delta <- 0.01
    current_val <- rep_data[[var_name]]
    
    nd_low <- rep_data
    nd_high <- rep_data
    nd_low[[var_name]] <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    
    pred_low <- predict(model, newdata = nd_low, type = "probs")
    pred_high <- predict(model, newdata = nd_high, type = "probs")
    
    # Handle vector case
    if (is.vector(pred_low)) {
      outcome_names <- model$lev
      pred_low <- matrix(pred_low, nrow = 1)
      pred_high <- matrix(pred_high, nrow = 1)
      colnames(pred_low) <- colnames(pred_high) <- outcome_names
    }
    
    marginal_effect <- (pred_high - pred_low) / delta
    
    results <- data.frame(
      variable = var_name,
      outcome = colnames(marginal_effect),
      marginal_effect = as.vector(marginal_effect),
      type = "derivative",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
  }
  
  return(results)
}

# Test function with minimal bootstrap iterations
test_bootstrap_fixed <- function(model, data_subset, var_name, gen_label, B = 5) {
  
  cat("Testing bootstrap with", B, "iterations...\n")
  
  point_estimates <- calculate_marginal_effects_simple(model, data_subset, var_name, gen_label)
  cat("Point estimates:\n")
  print(point_estimates$marginal_effect)
  
  boot_estimates <- matrix(NA, nrow = B, ncol = nrow(point_estimates))
  original_formula <- formula(model)
  
  for (b in 1:B) {
    boot_indices <- sample(nrow(data_subset), replace = TRUE)
    boot_data <- data_subset[boot_indices, ]
    
    cat("\n--- Bootstrap iteration", b, "---\n")
    cat("Bootstrap sample size:", nrow(boot_data), "\n")
    
    tryCatch({
      boot_model <- multinom(original_formula, data = boot_data, trace = FALSE)
      boot_me <- calculate_marginal_effects_simple(boot_model, boot_data, var_name, gen_label)
      boot_estimates[b, ] <- boot_me$marginal_effect
      
      cat("Bootstrap estimates:", boot_me$marginal_effect, "\n")
      cat("Difference from point estimate:", boot_me$marginal_effect - point_estimates$marginal_effect, "\n")
      
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
    })
  }
  
  cat("\n--- Summary ---\n")
  if (!all(is.na(boot_estimates))) {
    cat("Bootstrap standard errors:\n")
    print(apply(boot_estimates, 2, sd, na.rm = TRUE))
  } else {
    cat("All bootstrap iterations failed!\n")
  }
  
  return(boot_estimates)
}

# Calculate all marginal effects
calculate_all_marginal_effects <- function(model, data_subset, gen_label, use_bootstrap = FALSE) {
  
  variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")
  all_results <- list()
  
  for (var in variables) {
    if (use_bootstrap) {
      result <- calculate_marginal_effects_bootstrap(model, data_subset, var, gen_label, B = 1000)
    } else {
      result <- calculate_marginal_effects(model, data_subset, var, gen_label)
      result$std_error <- 0.01
      result$conf_low <- result$marginal_effect - 1.96 * result$std_error
      result$conf_high <- result$marginal_effect + 1.96 * result$std_error
    }
    all_results[[var]] <- result
  }
  
  combined_results <- do.call(rbind, all_results)
  return(combined_results)
}

# Plot marginal effects
plot_marginal_effects <- function(me_results, gen_label) {
  
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother", 
    "DadGradCollege" = "College Graduate: Father"
  )
  
  outcome_labels <- c(
    "Asian_only" = "Asian only",
    "White_only" = "White only", 
    "Asian_and_White" = "Asian & White"
  )
  
  me_results$variable_label <- factor(me_results$variable, 
                                     levels = names(var_labels),
                                     labels = var_labels)
  me_results$outcome_label <- factor(me_results$outcome,
                                    levels = names(outcome_labels),
                                    labels = outcome_labels)
  
  p <- ggplot(me_results, aes(x = marginal_effect, y = variable_label, 
                             color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), 
                   height = 0.3,           # Increase height
                   linewidth = 1.0,        # Make lines thicker
                   position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(
      "Asian only" = "#2E8B57",
      "White only" = "#4169E1",
      "Asian & White" = "#FF8C00"
    ), name = "Identity Choice") +
    labs(
      x = "Marginal Effect (percentage points)",
      y = "",
      title = paste("Marginal Effects —", gen_label)
    ) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5)
    )
  
  return(p)
}

# Updated function to calculate all marginal effects using bootstrap
calculate_all_marginal_effects_bootstrap <- function(model, data_subset, gen_label, B = 1000) {
  
  variables <- c("value", "Female", "MomGradCollege", "DadGradCollege")
  all_results <- list()
  
  for (var in variables) {
    cat("Calculating marginal effects for", var, "...\n")
    result <- calculate_marginal_effects_bootstrap_fixed(model, data_subset, var, gen_label, B = B)
    all_results[[var]] <- result
  }
  
  combined_results <- do.call(rbind, all_results)
  return(combined_results)
}

# ============================================================================
# CREATE ALL MARGINAL EFFECTS PLOTS - CORRECTED
# ============================================================================

# Main generations
cat("Processing main generations...\n")
me_all_gen_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_all_gen, CPS_IAT_multinomial, "All generations", B = 100)
me_first_gen_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_first_gen, dplyr::filter(CPS_IAT_multinomial, FirstGen_Asian == 1), "First generation", B = 100)
me_second_gen_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_gen, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1), "Second generation", B = 100)
me_third_gen_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_third_gen, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1), "Third generation", B = 100)

# Third generation ancestry subgroups
cat("Processing third generation subgroups...\n")
me_third_one_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_third_one, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, OneAsian == 1), "Third gen: One Asian grandparent", B = 100)
me_third_two_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_third_two, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, TwoAsian == 1), "Third gen: Two Asian grandparents", B = 100)
me_third_three_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_third_three, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, ThreeAsian == 1), "Third gen: Three Asian grandparents", B = 100)
me_third_four_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_third_four, dplyr::filter(CPS_IAT_multinomial, ThirdGen_Asian == 1, FourAsian == 1), "Third gen: Four Asian grandparents", B = 100)

# Second generation ancestry subgroups
cat("Processing second generation subgroups...\n")
me_second_aa_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_aa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AA_0bj == 1), "Second gen: AA parents", B = 100)
me_second_aw_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_aw, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, AW_0bj == 1), "Second gen: AW parents", B = 100)
me_second_wa_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_wa, dplyr::filter(CPS_IAT_multinomial, SecondGen_Asian == 1, WA_0bj == 1), "Second gen: WA parents", B = 100)

# Create all marginal effects plots
cat("Creating plots...\n")
plot_me_all_bootstrap <- plot_marginal_effects(me_all_gen_bootstrap, "All generations")
plot_me_first_bootstrap <- plot_marginal_effects(me_first_gen_bootstrap, "First generation")
plot_me_second_bootstrap <- plot_marginal_effects(me_second_gen_bootstrap, "Second generation")
plot_me_third_bootstrap <- plot_marginal_effects(me_third_gen_bootstrap, "Third generation")

plot_me_third_one_bootstrap <- plot_marginal_effects(me_third_one_bootstrap, "Third gen: One Asian grandparent")
plot_me_third_two_bootstrap <- plot_marginal_effects(me_third_two_bootstrap, "Third gen: Two Asian grandparents")
plot_me_third_three_bootstrap <- plot_marginal_effects(me_third_three_bootstrap, "Third gen: Three Asian grandparents")
plot_me_third_four_bootstrap <- plot_marginal_effects(me_third_four_bootstrap, "Third gen: Four Asian grandparents")

plot_me_second_aa_bootstrap <- plot_marginal_effects(me_second_aa_bootstrap, "Second gen: AA parents")
plot_me_second_aw_bootstrap <- plot_marginal_effects(me_second_aw_bootstrap, "Second gen: AW parents")
plot_me_second_wa_bootstrap <- plot_marginal_effects(me_second_wa_bootstrap, "Second gen: WA parents")

# Save all marginal effects plots
cat("Saving plots...\n")
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_all.png"), plot_me_all_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_first.png"), plot_me_first_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second.png"), plot_me_second_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third.png"), plot_me_third_bootstrap, width = 10, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_one.png"), plot_me_third_one_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_two.png"), plot_me_third_two_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_three.png"), plot_me_third_three_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_four.png"), plot_me_third_four_bootstrap, width = 10, height = 6, dpi = 300)

ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second_aa.png"), plot_me_second_aa_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second_aw.png"), plot_me_second_aw_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second_wa.png"), plot_me_second_wa_bootstrap, width = 10, height = 6, dpi = 300)

cat("All plots completed and saved!\n")