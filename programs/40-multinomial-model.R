# ================================================================
# Multinomial (nnet::multinom) with grid-aligned PP plots
# and FIXED bootstrap marginal effects plots
# ================================================================

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
})

set.seed(123)

# ------------------------------------------------
# Load & prepare data
# ------------------------------------------------
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian.csv")) |>
  rename(value = lw_index) |>
  mutate(
    OneAsian = case_when(
      Grandparent_Type %in% c("AWWW","WAWW","WWAW","WWWA") ~ 1, TRUE ~ 0),
    TwoAsian = case_when(
      Grandparent_Type %in% c("AAWW","AWAW","AWWA","WAAW","WWAA","WAWA") ~ 1, TRUE ~ 0),
    ThreeAsian = case_when(
      Grandparent_Type %in% c("AAAW","AAWA","AWAA","WAAA") ~ 1, TRUE ~ 0),
    FourAsian = case_when(Grandparent_Type == "AAAA" ~ 1, TRUE ~ 0)
  )

categorize_race_multinomial <- function(race_code) {
  dplyr::case_when(
    race_code %in% c(651) ~ "Asian_only",
    race_code %in% c(100) ~ "White_only",
    race_code %in% c(803, 804, 809, 813) ~ "Asian_and_White",
    TRUE ~ "Other"
  )
}

CPS_IAT_multinomial <- CPS_IAT |>
  mutate(
    identity_choice = categorize_race_multinomial(race),
    identity_choice = factor(identity_choice,
                             levels = c("Asian_only","White_only","Asian_and_White"))
  ) |>
  filter(identity_choice %in% c("Asian_only","White_only","Asian_and_White")) |>
  filter(!is.na(value), !is.na(identity_choice)) |>
  mutate(region_year = interaction(region, year, drop = TRUE))

CPS_IAT_multinomial <- CPS_IAT_multinomial |>
  mutate(identity_choice = relevel(identity_choice, ref = "Asian_only"))

cat("Distribution of identity choices:\n")
print(table(CPS_IAT_multinomial$identity_choice))
print(prop.table(table(CPS_IAT_multinomial$identity_choice)))

# ------------------------------------------------
# Fit multinomial models
# ------------------------------------------------
fit_multinomial_model <- function(data_subset, generation = "all", ancestry_filter = NULL) {
  if (generation == "first") {
    fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
      frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
  } else if (generation == "second") {
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("AA_0bj","AW_0bj","WA_0bj")) {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj + region_year
    }
  } else if (generation == "third") {
    if (!is.null(ancestry_filter) && ancestry_filter %in% c("OneAsian","TwoAsian","ThreeAsian","FourAsian")) {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + region_year
    } else {
      fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
        frac_asian + Age + Age_sq + Age_cube + Age_quad + Grandparent_Type + region_year
    }
  } else {
    fml <- identity_choice ~ value + Female + MomGradCollege + DadGradCollege +
      frac_asian + Age + Age_sq + Age_cube + Age_quad + AA_0bj +
      FirstGen_Asian + SecondGen_Asian + region_year
  }
  multinom(fml, data = data_subset, weights = data_subset$weight, trace = FALSE)
}

cat("Fitting multinomial models using nnet::multinom...\n")
mnl_all_gen    <- fit_multinomial_model(CPS_IAT_multinomial, "all")
mnl_first_gen  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(FirstGen_Asian == 1), "first")
mnl_second_gen <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1), "second")
mnl_third_gen  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1),  "third")

# Third-gen subgroups
cat("Fitting multinomial models for third generation ancestry subgroups...\n")
mnl_third_one   <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, OneAsian   == 1), "third", "OneAsian")
mnl_third_two   <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, TwoAsian   == 1), "third", "TwoAsian")
mnl_third_three <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, ThreeAsian == 1), "third", "ThreeAsian")
mnl_third_four  <- fit_multinomial_model(CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, FourAsian  == 1), "third", "FourAsian")

# Second-gen subgroups
cat("Fitting multinomial models for second generation ancestry subgroups...\n")
mnl_second_aa <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AA_0bj == 1), "second", "AA_0bj")
mnl_second_aw <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AW_0bj == 1), "second", "AW_0bj")
mnl_second_wa <- fit_multinomial_model(CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, WA_0bj == 1), "second", "WA_0bj")

# ------------------------------------------------
# GRID-ALIGNED Predicted probability plots
# ------------------------------------------------
plot_pp_simple <- function(model, data_subset, var_name, gen_label,
                           show_labels = TRUE,
                           repel_box_padding = 0.35,
                           repel_point_padding = 0.30,
                           grid_step = 0.5) {

  # x-grid sequence (keep first & last + interior grid)
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    var_seq  <- c(0, 1)
    x_breaks <- c(0, 1)
  } else {
    vrng  <- range(data_subset[[var_name]], na.rm = TRUE)
    vmin  <- vrng[1]; vmax <- vrng[2]
    x_breaks <- seq(floor(vmin / grid_step) * grid_step,
                    ceiling(vmax / grid_step) * grid_step,
                    by = grid_step)
    interior <- x_breaks[x_breaks > vmin & x_breaks < vmax]
    var_seq  <- sort(unique(c(vmin, interior, vmax)))
  }

  # representative values
  get_rep_value <- function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else {
    tbl <- table(x); names(tbl)[which.max(tbl)]
  }

  rep_data <- data_subset[1, ]
  rep_data$Female         <- get_rep_value(data_subset$Female)
  rep_data$MomGradCollege <- get_rep_value(data_subset$MomGradCollege)
  rep_data$DadGradCollege <- get_rep_value(data_subset$DadGradCollege)
  rep_data$frac_asian     <- get_rep_value(data_subset$frac_asian)
  rep_data$Age            <- get_rep_value(data_subset$Age)
  rep_data$Age_sq         <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube       <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad       <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year    <- get_rep_value(data_subset$region_year)
  if ("AA_0bj" %in% names(data_subset))        rep_data$AA_0bj        <- get_rep_value(data_subset$AA_0bj)
  if ("FirstGen_Asian" %in% names(data_subset)) rep_data$FirstGen_Asian <- get_rep_value(data_subset$FirstGen_Asian)
  if ("SecondGen_Asian" %in% names(data_subset))rep_data$SecondGen_Asian<- get_rep_value(data_subset$SecondGen_Asian)
  if ("Grandparent_Type" %in% names(data_subset)) rep_data$Grandparent_Type <- get_rep_value(data_subset$Grandparent_Type)

  nd <- rep_data[rep(1, length(var_seq)), ]
  nd[[var_name]] <- var_seq

  # predictions
  pred_probs <- predict(model, newdata = nd, type = "probs")
  if (is.vector(pred_probs)) {
    outcome_names <- model$lev
    pred_probs <- matrix(pred_probs, nrow = 1)
    colnames(pred_probs) <- outcome_names
  }

  # ~±2% CI band
  se_approx <- 0.02
  conf_low  <- pmax(0, pred_probs - 1.96 * se_approx)
  conf_high <- pmin(1, pred_probs + 1.96 * se_approx)

  # long format
  n_obs <- nrow(pred_probs); n_groups <- ncol(pred_probs)
  group_names <- colnames(pred_probs)
  pred_df <- data.frame(
    x_val    = rep(var_seq, n_groups),
    group    = rep(group_names, each = n_obs),
    estimate = as.vector(pred_probs),
    conf_low = as.vector(conf_low),
    conf_high = as.vector(conf_high)
  )

  # labels
  var_labels <- c(
    "value" = "Anti-Asian Bias",
    "Female" = "Female",
    "MomGradCollege" = "College Graduate: Mother",
    "DadGradCollege" = "College Graduate: Father"
  )
  outcome_labels <- c(
    "Asian_only"      = "Asian only",
    "White_only"      = "White only",
    "Asian_and_White" = "Asian & White"
  )
  x_label <- if (var_name %in% names(var_labels)) var_labels[[var_name]] else var_name
  pred_df$group_labeled <- dplyr::recode(pred_df$group, !!!outcome_labels, .default = pred_df$group)
  pred_df$group_labeled <- factor(pred_df$group_labeled, levels = outcome_labels)
  pred_df$label <- scales::percent(pred_df$estimate, accuracy = 1)

  error_width <- if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) 0.05
                 else (max(var_seq) - min(var_seq)) * 0.02
  pd <- position_dodge(width = if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) 0.15 else 0.07)

  p <- ggplot(pred_df, aes(x = x_val, y = estimate, color = group_labeled, group = group_labeled)) +
    geom_line(linewidth = 1.2, position = pd) +
    geom_point(size = 3, alpha = 0.95, position = pd) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                  width = error_width, linewidth = 1, position = pd) +
    { if (show_labels)
        ggrepel::geom_label_repel(
          aes(label = label),
          position = pd, label.size = 0, fill = "white", alpha = 0.9, size = 6,
          max.overlaps = Inf,
          box.padding = grid::unit(repel_box_padding, "lines"),
          point.padding = grid::unit(repel_point_padding, "lines"),
          min.segment.length = 0, segment.alpha = 0.6, seed = 123, show.legend = FALSE
        )
      else NULL } +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"), name = "") +
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

  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    if (var_name == "Female")
      p <- p + scale_x_continuous(breaks = c(0,1), labels = c("Male","Female"))
    else
      p <- p + scale_x_continuous(breaks = c(0,1), labels = c("No College","College Graduate"))
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks)
  }
  p
}

# ---- Build & save PP plots ----
variables <- c("value","Female","MomGradCollege","DadGradCollege")

# Main generations
for (var in variables) {
  assign(paste0("pp_all_", var, "_simple"),
         plot_pp_simple(mnl_all_gen, CPS_IAT_multinomial, var, "All generations"))
  assign(paste0("pp_first_", var, "_simple"),
         plot_pp_simple(mnl_first_gen, CPS_IAT_multinomial |> filter(FirstGen_Asian == 1), var, "First generation"))
  assign(paste0("pp_second_", var, "_simple"),
         plot_pp_simple(mnl_second_gen, CPS_IAT_multinomial |> filter(SecondGen_Asian == 1), var, "Second generation"))
  assign(paste0("pp_third_", var, "_simple"),
         plot_pp_simple(mnl_third_gen, CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1), var, "Third generation"))
}

# Third-gen subgroups
ancestry_groups <- c("one","two","three","four")
for (var in variables) {
  for (anc in ancestry_groups) {
    model_name <- paste0("mnl_third_", anc)
    filter_var <- paste0(toupper(substring(anc,1,1)), substring(anc,2), "Asian")
    assign(paste0("pp_third_", anc, "_", var, "_simple"),
           plot_pp_simple(get(model_name),
                          CPS_IAT_multinomial |> filter(ThirdGen_Asian == 1, get(filter_var) == 1),
                          var, paste("Third gen:", anc, "Asian grandparent")))
  }
}

# Second-gen subgroups
second_groups <- c("aa","aw","wa")
for (var in variables) {
  for (grp in second_groups) {
    model_name <- paste0("mnl_second_", grp)
    filter_var <- paste0(toupper(grp), "_0bj")
    assign(paste0("pp_second_", grp, "_", var, "_simple"),
           plot_pp_simple(get(model_name),
                          CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, get(filter_var) == 1),
                          var, paste("Second gen:", toupper(grp), "parents")))
  }
}

# Save all PP plots
for (var in variables) {
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_all.png")),
         get(paste0("pp_all_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_first.png")),
         get(paste0("pp_first_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second.png")),
         get(paste0("pp_second_", var, "_simple")), width = 8, height = 6, dpi = 300)
  ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third.png")),
         get(paste0("pp_third_", var, "_simple")), width = 8, height = 6, dpi = 300)
  for (anc in ancestry_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_third_", anc, ".png")),
           get(paste0("pp_third_", anc, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
  for (grp in second_groups) {
    ggsave(file.path(figures_wd, paste0("simple_pp_", var, "_second_", grp, ".png")),
           get(paste0("pp_second_", grp, "_", var, "_simple")), width = 8, height = 6, dpi = 300)
  }
}

# ------------------------------------------------
# Marginal effects: FIXED bootstrap pipeline
# ------------------------------------------------

# Simplified ME for a single variable (used in bootstrap)
calculate_marginal_effects_simple <- function(model, data_subset, var_name, gen_label) {
  rep_data <- data_subset[1, , drop = FALSE]

  for (col_name in names(data_subset)) {
    if (col_name == var_name) next
    if (is.numeric(data_subset[[col_name]])) {
      rep_data[[col_name]] <- mean(data_subset[[col_name]], na.rm = TRUE)
    } else {
      tbl <- table(data_subset[[col_name]])
      rep_data[[col_name]] <- names(tbl)[which.max(tbl)]
    }
  }

  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    nd_0 <- rep_data; nd_1 <- rep_data
    nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1
    pred_0 <- predict(model, newdata = nd_0, type = "probs")
    pred_1 <- predict(model, newdata = nd_1, type = "probs")
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1); pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    me <- pred_1 - pred_0
    data.frame(variable = var_name, outcome = colnames(me),
               marginal_effect = as.vector(me),
               type = "discrete_change", generation = gen_label, stringsAsFactors = FALSE)
  } else {
    delta <- 0.01
    current_val <- rep_data[[var_name]]
    nd_low <- rep_data; nd_high <- rep_data
    nd_low[[var_name]]  <- current_val - delta/2
    nd_high[[var_name]] <- current_val + delta/2
    pred_low  <- predict(model, newdata = nd_low,  type = "probs")
    pred_high <- predict(model, newdata = nd_high, type = "probs")
    if (is.vector(pred_low)) {
      outcome_names <- model$lev
      pred_low <- matrix(pred_low, nrow = 1); pred_high <- matrix(pred_high, nrow = 1)
      colnames(pred_low) <- colnames(pred_high) <- outcome_names
    }
    me <- (pred_high - pred_low) / delta
    data.frame(variable = var_name, outcome = colnames(me),
               marginal_effect = as.vector(me),
               type = "derivative", generation = gen_label, stringsAsFactors = FALSE)
  }
}

# Fixed bootstrap (refits with explicit formula; aligns outcomes)
calculate_marginal_effects_bootstrap_fixed <- function(model, data_subset, var_name, gen_label, B = 1000) {
  point_est <- calculate_marginal_effects_simple(model, data_subset, var_name, gen_label)
  boot_effects <- matrix(NA_real_, nrow = B, ncol = nrow(point_est))
  successful <- 0; failed <- 0
  original_formula <- formula(model)

  for (b in 1:B) {
    boot_idx <- sample(nrow(data_subset), replace = TRUE)
    boot_data <- data_subset[boot_idx, ]
    tryCatch({
      boot_model <- multinom(original_formula, data = boot_data, trace = FALSE)
      boot_me <- calculate_marginal_effects_simple(boot_model, boot_data, var_name, gen_label)
      idx <- match(point_est$outcome, boot_me$outcome)
      aligned <- rep(NA_real_, nrow(point_est))
      aligned[!is.na(idx)] <- boot_me$marginal_effect[idx[!is.na(idx)]]
      boot_effects[b, ] <- aligned
      successful <- successful + 1
    }, error = function(e) {
      failed <<- failed + 1
      if (failed <= 5) cat("Bootstrap iteration", b, "failed:", e$message, "\n")
    })
    if (b %% 100 == 0) cat("Completed", b, "iterations...\n")
  }

  cat("Bootstrap results:", successful, "successes,", failed, "failures\n")
  if (successful < 50) warning("Very few successful bootstrap iterations (", successful, "). Results may be unreliable.")

  conf_low  <- apply(boot_effects, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  conf_high <- apply(boot_effects, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  boot_se   <- apply(boot_effects,  2, function(x) sd(x, na.rm = TRUE))

  point_est$std_error <- boot_se
  point_est$conf_low  <- conf_low
  point_est$conf_high <- conf_high
  point_est$n_successful_boots <- successful
  point_est
}

# Collect all variables' MEs (bootstrap)
calculate_all_marginal_effects_bootstrap <- function(model, data_subset, gen_label, B = 1000) {
  variables <- c("value","Female","MomGradCollege","DadGradCollege")
  all_results <- lapply(variables, function(v) {
    cat("Calculating marginal effects for", v, "...\n")
    calculate_marginal_effects_bootstrap_fixed(model, data_subset, v, gen_label, B = B)
  })
  do.call(rbind, all_results)
}

# ME plot
plot_marginal_effects <- function(me_results, gen_label) {
  var_labels <- c("value"="Anti-Asian Bias","Female"="Female",
                  "MomGradCollege"="College Graduate: Mother","DadGradCollege"="College Graduate: Father")
  outcome_labels <- c("Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White")

  me_results$variable_label <- factor(me_results$variable, levels = names(var_labels), labels = var_labels)
  me_results$outcome_label  <- factor(me_results$outcome,  levels = names(outcome_labels), labels = outcome_labels)

  ggplot(me_results, aes(x = marginal_effect, y = variable_label, color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high),
                   height = 0.3, linewidth = 1.0, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"),
                       name = "Identity Choice") +
    labs(x = "Marginal Effect (percentage points)", y = "", title = paste("Marginal Effects —", gen_label)) +
    theme_customs() +
    theme(legend.position = "bottom",
          axis.title = element_text(size = 12),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5))
}

# ---- Build & save ME (bootstrap) plots ----
cat("Processing main generations...\n")
me_all_gen_bootstrap    <- calculate_all_marginal_effects_bootstrap(mnl_all_gen,    CPS_IAT_multinomial,                                                     "All generations",    B = 100)
me_first_gen_bootstrap  <- calculate_all_marginal_effects_bootstrap(mnl_first_gen,  CPS_IAT_multinomial |> filter(FirstGen_Asian  == 1),                     "First generation",   B = 100)
me_second_gen_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_gen, CPS_IAT_multinomial |> filter(SecondGen_Asian == 1),                     "Second generation",  B = 100)
me_third_gen_bootstrap  <- calculate_all_marginal_effects_bootstrap(mnl_third_gen,  CPS_IAT_multinomial |> filter(ThirdGen_Asian  == 1),                     "Third generation",   B = 100)

cat("Processing third generation subgroups...\n")
me_third_one_bootstrap   <- calculate_all_marginal_effects_bootstrap(mnl_third_one,   CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, OneAsian   == 1), "Third gen: One Asian grandparent",   B = 100)
me_third_two_bootstrap   <- calculate_all_marginal_effects_bootstrap(mnl_third_two,   CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, TwoAsian   == 1), "Third gen: Two Asian grandparents", B = 100)
me_third_three_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_third_three, CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, ThreeAsian == 1), "Third gen: Three Asian grandparents", B = 100)
me_third_four_bootstrap  <- calculate_all_marginal_effects_bootstrap(mnl_third_four,  CPS_IAT_multinomial |> filter(ThirdGen_Asian==1, FourAsian  == 1), "Third gen: Four Asian grandparents",  B = 100)

cat("Processing second generation subgroups...\n")
me_second_aa_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_aa, CPS_IAT_multinomial |> filter(SecondGen_Asian==1, AA_0bj==1), "Second gen: AA parents", B = 100)
me_second_aw_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_aw, CPS_IAT_multinomial |> filter(SecondGen_Asian==1, AW_0bj==1), "Second gen: AW parents", B = 100)
me_second_wa_bootstrap <- calculate_all_marginal_effects_bootstrap(mnl_second_wa, CPS_IAT_multinomial |> filter(SecondGen_Asian==1, WA_0bj==1), "Second gen: WA parents", B = 100)

cat("Creating plots...\n")
plot_me_all_bootstrap    <- plot_marginal_effects(me_all_gen_bootstrap,    "All generations")
plot_me_first_bootstrap  <- plot_marginal_effects(me_first_gen_bootstrap,  "First generation")
plot_me_second_bootstrap <- plot_marginal_effects(me_second_gen_bootstrap, "Second generation")
plot_me_third_bootstrap  <- plot_marginal_effects(me_third_gen_bootstrap,  "Third generation")
plot_me_third_one_bootstrap   <- plot_marginal_effects(me_third_one_bootstrap,   "Third gen: One Asian grandparent")
plot_me_third_two_bootstrap   <- plot_marginal_effects(me_third_two_bootstrap,   "Third gen: Two Asian grandparents")
plot_me_third_three_bootstrap <- plot_marginal_effects(me_third_three_bootstrap, "Third gen: Three Asian grandparents")
plot_me_third_four_bootstrap  <- plot_marginal_effects(me_third_four_bootstrap,  "Third gen: Four Asian grandparents")
plot_me_second_aa_bootstrap <- plot_marginal_effects(me_second_aa_bootstrap, "Second gen: AA parents")
plot_me_second_aw_bootstrap <- plot_marginal_effects(me_second_aw_bootstrap, "Second gen: AW parents")
plot_me_second_wa_bootstrap <- plot_marginal_effects(me_second_wa_bootstrap, "Second gen: WA parents")

cat("Saving plots...\n")
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_all.png"),    plot_me_all_bootstrap,    width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_first.png"),  plot_me_first_bootstrap,  width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second.png"), plot_me_second_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third.png"),  plot_me_third_bootstrap,  width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_one.png"),   plot_me_third_one_bootstrap,   width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_two.png"),   plot_me_third_two_bootstrap,   width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_three.png"), plot_me_third_three_bootstrap, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_third_four.png"),  plot_me_third_four_bootstrap,  width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second_aa.png"),   plot_me_second_aa_bootstrap,   width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second_aw.png"),   plot_me_second_aw_bootstrap,   width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_marginal_effects_second_wa.png"),   plot_me_second_wa_bootstrap,   width = 10, height = 6, dpi = 300)

cat("All plots completed and saved!\n")
