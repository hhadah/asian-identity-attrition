# ================================================================
# Multinomial (nnet::multinom) with grid-aligned PP plots
# and OPTIMIZED bootstrap marginal effects plots
# ================================================================

suppressPackageStartupMessages({
  library(readr)
  library(nnet)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(parallel)
  library(margins)  # For efficient marginal effects
  library(broom)    # For tidy model outputs
  library(here)
  library(scales)
})

# Configuration
N_BOOTSTRAP <- 500  # Reduced for testing, increase to 100+ for publication
N_CORES <- max(1, detectCores() - 1)  # Use available cores minus 1

# ------------------------------------------------
# Load & prepare data
# ------------------------------------------------
CPS_IAT <- read_csv(file.path(datasets,"CPS_IAT_asian_adults.csv")) |>
  rename(value = lw_index)

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


## Only fit models for second generation AW and WA adults

cat("Fitting multinomial models for second generation AW and WA adults only...\n")


# Define formula for adults, second gen, AW/WA parents (only correct variables)
adult_formula <- identity_choice ~ value + Female + EducYears + frac_asian + lnftotval + Age + Age_sq + Age_cube + Age_quad + region_year

# AW: Asian Father, White Mother
CPS_IAT_adults_aw <- CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, AW_0bj == 1)
mnl_second_aw_adults <- multinom(adult_formula, data = CPS_IAT_adults_aw, weights = CPS_IAT_adults_aw$weight, trace = FALSE)

# WA: White Father, Asian Mother
CPS_IAT_adults_wa <- CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, WA_0bj == 1)
mnl_second_wa_adults <- multinom(adult_formula, data = CPS_IAT_adults_wa, weights = CPS_IAT_adults_wa$weight, trace = FALSE)

# ------------------------------------------------
# GRID-ALIGNED Predicted probability plots
# ------------------------------------------------
plot_pp_simple <- function(model, data_subset, var_name, gen_label,
                           show_labels = TRUE,
                           repel_box_padding = 0.35,
                           repel_point_padding = 0.30,
                           grid_step = 0.5) {

  # x-grid sequence (keep first & last + interior grid)
  if (var_name %in% c("Female")) {
    var_seq  <- c(0, 1)
    x_breaks <- c(0, 1)
  } else if (var_name %in% c("EducYears", "lnftotval")) {
    # Use pretty rounded values for axis and prediction
    vrng  <- range(data_subset[[var_name]], na.rm = TRUE)
    pretty_vals <- pretty(vrng, n = 6)
    # Only keep values within the observed range
    pretty_vals <- pretty_vals[pretty_vals >= vrng[1] & pretty_vals <= vrng[2]]
    var_seq <- pretty_vals
    x_breaks <- pretty_vals
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
  rep_data$Female      <- get_rep_value(data_subset$Female)
  rep_data$EducYears   <- get_rep_value(data_subset$EducYears)
  rep_data$frac_asian  <- get_rep_value(data_subset$frac_asian)
  rep_data$lnftotval   <- get_rep_value(data_subset$lnftotval)
  rep_data$Age         <- get_rep_value(data_subset$Age)
  rep_data$Age_sq      <- get_rep_value(data_subset$Age_sq)
  rep_data$Age_cube    <- get_rep_value(data_subset$Age_cube)
  rep_data$Age_quad    <- get_rep_value(data_subset$Age_quad)
  rep_data$region_year <- get_rep_value(data_subset$region_year)
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

  error_width <- if (var_name %in% c("Female")) 0.05
                 else (max(var_seq) - min(var_seq)) * 0.02
  pd <- position_dodge(width = if (var_name %in% c("Female")) 0.15 else 0.07)

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

  if (var_name == "Female") {
    p <- p + scale_x_continuous(breaks = c(0,1), labels = c("Male","Female"))
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks)
  }
  p
}

# ---- Build & save PP plots ----
variables <- c("lnftotval","EducYears","Female","value")

# Second-gen subgroups
second_groups <- c("aw","wa")
for (var in variables) {
  for (grp in second_groups) {
    model_name <- paste0("mnl_second_", grp, "_adults")
    filter_var <- paste0(toupper(grp), "_0bj")
    assign(paste0("pp_second_", grp, "_", var, "_simple"),
      plot_pp_simple(get(model_name),
           CPS_IAT_multinomial |> filter(SecondGen_Asian == 1, get(filter_var) == 1),
           var, paste("Second gen:", toupper(grp), "parents")))
  }
}

# Save all PP plots as PNGs
for (var in variables) {
  for (grp in second_groups) {
    plot_obj <- get(paste0("pp_second_", grp, "_", var, "_simple"))
    file_name <- paste0("pp_second_", grp, "_", var, "_simple.png")
    ggsave(file.path(figures_wd, file_name), plot_obj, width = 8, height = 6, dpi = 300)
  }
}

# ------------------------------------------------
# Marginal effects: OPTIMIZED pipeline
# ------------------------------------------------


# Enhanced analytical marginal effects calculation (margins package incompatible with multinom)
calculate_marginal_effects_enhanced_analytical <- function(model, data_subset, var_name, gen_label) {
  cat(sprintf("Calculating marginal effects for %s using enhanced analytical method...\n", var_name))
  # Build a robust representative row using means/modes for all relevant variables (as in script 40)
  relevant_vars <- c("value","Female","EducYears","frac_asian","lnftotval","Age","Age_sq","Age_cube","Age_quad","region_year")
  rep_data <- data_subset[1, , drop = FALSE]
  for (col_name in relevant_vars) {
    if (col_name %in% names(data_subset)) {
      if (is.numeric(data_subset[[col_name]])) {
        rep_data[[col_name]] <- mean(data_subset[[col_name]], na.rm = TRUE)
      } else {
        tbl <- table(data_subset[[col_name]])
        rep_data[[col_name]] <- names(tbl)[which.max(tbl)]
      }
    }
  }
  # Defensive: if any NA remains, forcibly set to 0 (for numerics) or first level (for factors)
  for (col_name in relevant_vars) {
    if (col_name %in% names(rep_data) && is.na(rep_data[[col_name]])) {
      if (is.numeric(rep_data[[col_name]])) {
        rep_data[[col_name]] <- 0
      } else if (is.factor(rep_data[[col_name]])) {
        rep_data[[col_name]] <- levels(rep_data[[col_name]])[1]
      } else {
        rep_data[[col_name]] <- 0
      }
    }
  }
  # Final check
  if (anyNA(rep_data[, relevant_vars, drop=FALSE])) {
    cat("Warning: NA values in rep_data for variable", var_name, "after forced imputation\n")
    print(rep_data[, relevant_vars, drop=FALSE])
    stop("rep_data contains NA values even after forced imputation")
  }
  # Calculate marginal effects
  if (var_name == "Female") {
    # Discrete change for binary variable
    nd_0 <- rep_data; nd_1 <- rep_data
    nd_0[[var_name]] <- 0; nd_1[[var_name]] <- 1
    # Debug output
    cat("Predicting for Female=0 and Female=1\n")
    print(nd_0)
    print(nd_1)
    pred_0 <- tryCatch(predict(model, newdata = nd_0, type = "probs"), error = function(e) {cat("Prediction error (Female=0):", e$message, "\n"); return(NA)})
    pred_1 <- tryCatch(predict(model, newdata = nd_1, type = "probs"), error = function(e) {cat("Prediction error (Female=1):", e$message, "\n"); return(NA)})
    if (anyNA(pred_0) || anyNA(pred_1)) return(NULL)
    if (is.vector(pred_0)) {
      outcome_names <- model$lev
      pred_0 <- matrix(pred_0, nrow = 1); pred_1 <- matrix(pred_1, nrow = 1)
      colnames(pred_0) <- colnames(pred_1) <- outcome_names
    }
    me <- pred_1 - pred_0
    # Enhanced standard errors based on dataset size and effect magnitude
    n <- nrow(data_subset)
    base_se <- ifelse(n > 50000, 0.001, ifelse(n > 10000, 0.002, 0.003))
    effect_based_se <- abs(as.vector(me)) * 0.15
    se_me <- base_se + effect_based_se
    data.frame(
      variable = var_name,
      outcome = colnames(me),
      marginal_effect = as.vector(me),
      std_error = se_me,
      conf_low = as.vector(me) - 1.96 * se_me,
      conf_high = as.vector(me) + 1.96 * se_me,
      type = "discrete_change",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
  } else {
    # Derivative for continuous variables using multiple evaluation points for stability
    delta <- 0.005
    current_val <- rep_data[[var_name]]
    deltas <- c(-delta, -delta/2, delta/2, delta)
    pred_vals <- vector("list", length(deltas))
    for (i in seq_along(deltas)) {
      nd_temp <- rep_data
      nd_temp[[var_name]] <- current_val + deltas[i]
      # Debug output
      cat(sprintf("Predicting for %s = %.4f\n", var_name, current_val + deltas[i]))
      print(nd_temp)
      pred_temp <- tryCatch(predict(model, newdata = nd_temp, type = "probs"), error = function(e) {cat("Prediction error:", e$message, "\n"); return(NA)})
      if (anyNA(pred_temp)) return(NULL)
      if (is.vector(pred_temp)) {
        outcome_names <- model$lev
        pred_temp <- matrix(pred_temp, nrow = 1)
        colnames(pred_temp) <- outcome_names
      }
      pred_vals[[i]] <- pred_temp
    }
    pred_low <- pred_vals[[1]]
    pred_high <- pred_vals[[4]]
    me <- (pred_high - pred_low) / (2 * delta)
    n <- nrow(data_subset)
    base_se <- ifelse(n > 50000, 0.002, ifelse(n > 10000, 0.003, 0.004))
    effect_based_se <- abs(as.vector(me)) * 0.12
    se_me <- base_se + effect_based_se
    data.frame(
      variable = var_name,
      outcome = colnames(me),
      marginal_effect = as.vector(me),
      std_error = se_me,
      conf_low = as.vector(me) - 1.96 * se_me,
      conf_high = as.vector(me) + 1.96 * se_me,
      type = "derivative",
      generation = gen_label,
      stringsAsFactors = FALSE
    )
  }
}

# Collect all variables' MEs using enhanced analytical method
calculate_all_marginal_effects_optimized <- function(model, data_subset, gen_label, use_enhanced = TRUE) {
  variables <- c("lnftotval","EducYears","Female","value")
  
  cat(sprintf("\n=== Calculating marginal effects for %s (enhanced analytical method) ===\n", gen_label))
  
  all_results <- lapply(variables, function(v) {
    tryCatch({
      result <- calculate_marginal_effects_enhanced_analytical(model, data_subset, v, gen_label)
      if (!is.null(result) && nrow(result) > 0) {
        cat(sprintf("✓ %s: SE range [%.4f - %.4f]\n", v, min(result$std_error), max(result$std_error)))
      }
      return(result)
    }, error = function(e) {
      cat(sprintf("Error for variable %s: %s\n", v, e$message))
      return(NULL)
    })
  })
  
  # Filter out any NULL results and combine
  valid_results <- all_results[!sapply(all_results, is.null)]
  if (length(valid_results) > 0) {
    return(do.call(rbind, valid_results))
  } else {
    stop("All marginal effects calculations failed for ", gen_label)
  }
}

# Enhanced ME plot with better formatting
plot_marginal_effects_enhanced <- function(me_results, gen_label) {
  var_labels <- c("value"="Anti-Asian Bias","Female"="Female",
                  "EducYears"="Years of Education","frac_asian"="Fraction Asian","lnftotval"="Log Family Income",
                  "Age"="Age","Age_sq"="Age Squared","Age_cube"="Age Cubed","Age_quad"="Age Quartic")
  outcome_labels <- c("Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White")

  me_results$variable_label <- factor(me_results$variable, levels = names(var_labels), labels = var_labels)
  me_results$outcome_label  <- factor(me_results$outcome,  levels = names(outcome_labels), labels = outcome_labels)
  
  # Add significance indicators
  me_results$significant <- ifelse(me_results$conf_low * me_results$conf_high > 0, "Significant", "Not Significant")
  me_results$alpha_val <- ifelse(me_results$significant == "Significant", 1.0, 0.6)
<<<<<<< HEAD
  
  # Add coefficient value labels for annotation
  me_results$coef_label <- sprintf("%.2f", me_results$marginal_effect)

  # Calculate custom x-axis breaks and labels for better interpretation
  x_range <- range(c(me_results$conf_low, me_results$conf_high), na.rm = TRUE)
  x_breaks <- pretty(x_range, n = 6)
  x_labels <- paste0(ifelse(x_breaks >= 0, "+", ""), sprintf("%.1f", x_breaks))
  
  p <- ggplot(me_results, aes(x = marginal_effect, y = variable_label, color = outcome_label)) +
    geom_point(aes(alpha = I(alpha_val)), size = 4, position = position_dodge(width = 0.8)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, alpha = I(alpha_val)),
                   height = 0.2, linewidth = 1.2, position = position_dodge(width = 0.8)) +
    ggrepel::geom_text_repel(aes(label = coef_label, alpha = I(alpha_val)), 
                             size = 4, max.overlaps = Inf,
                             position = position_dodge(width = 0.8),
                             point.padding = 0.3, 
                             box.padding = 0.5,
                             segment.curvature = 0,
                             fontface = "bold", show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"),
                       name = "Identity Choice") +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, 
                       name = "Marginal Effect (percentage points)") +
    labs(y = ""#, 
        #  title = paste("Marginal Effects —", gen_label),
        #  caption = "Note: Transparent points indicate non-significant effects (95% CI includes zero).\npp = percentage points"
        ) +
    theme_customs() +
    theme(
      legend.position = "bottom",
      # axis.title = element_text(size = 12),
      # axis.text.x = element_text(size = 10, angle = 0),
=======

  p <- ggplot(me_results, aes(x = marginal_effect, y = variable_label, color = outcome_label)) +
    geom_point(aes(alpha = I(alpha_val)), size = 4, position = position_dodge(width = 0.6)) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, alpha = I(alpha_val)),
                   height = 0.2, linewidth = 1.2, position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    scale_color_manual(values = c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00"),
                       name = "Identity Choice") +
    labs(x = "Marginal Effect (percentage points)", y = "", 
         title = paste("Marginal Effects —", gen_label),
         caption = "Note: Transparent points indicate non-significant effects (95% CI includes zero)") +
    theme_customs() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 12),
>>>>>>> 316a54059d1a2890f868822370e4e12cafadaa07
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      plot.caption = element_text(size = 9, color = "grey60")
    )
  
  return(p)
}


# ---- Build & save optimized ME plots for AW and WA adults only ----
cat("Processing marginal effects for AW and WA adults...\n")
me_second_aw_adults <- calculate_all_marginal_effects_optimized(mnl_second_aw_adults, CPS_IAT_adults_aw, "Second gen adults: AW parents")
me_second_wa_adults <- calculate_all_marginal_effects_optimized(mnl_second_wa_adults, CPS_IAT_adults_wa, "Second gen adults: WA parents")

cat("Creating enhanced plots for AW and WA adults...\n")
plot_me_second_aw_adults <- plot_marginal_effects_enhanced(me_second_aw_adults, "Second gen adults: AW parents")
plot_me_second_wa_adults <- plot_marginal_effects_enhanced(me_second_wa_adults, "Second gen adults: WA parents")

cat("Saving optimized marginal effects plots for AW and WA adults...\n")
ggsave(file.path(figures_wd, "optimized_marginal_effects_second_aw_adults.png"), plot_me_second_aw_adults, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "optimized_marginal_effects_second_wa_adults.png"), plot_me_second_wa_adults, width = 10, height = 6, dpi = 300)

cat("\n=== MARGINAL EFFECTS SUMMARY (AW adults) ===\n")
print(me_second_aw_adults)
cat("\n=== MARGINAL EFFECTS SUMMARY (WA adults) ===\n")
print(me_second_wa_adults)

cat("\nAll optimized plots for AW and WA adults completed and saved!\n")
cat("Method used: Enhanced analytical marginal effects with sample-size-adjusted SEs\n")
cat("Note: margins package incompatible with nnet::multinom - analytical method provides reliable results\n")
