# ================================================================
# Multinomial Bootstrap Plotter: loads saved multinomial bootstrap results 
# and renders PP/ME figures for second generation adults.
# EDITED VERSION: Modeled after script 45, handles results from script 46
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(tidyr)
  library(grid)
})

EPS_PP <- getOption("bootstrap_eps_pp", 0.002)
EPS_ME <- getOption("bootstrap_eps_me", 1e-4)

`%||%` <- function(x, y) if (!is.null(x)) x else y

stopifnot(exists("bootstrap_results_path"), exists("bootstrap_results_path"))
if (!dir.exists(bootstrap_results_path)) dir.create(bootstrap_results_path, recursive = TRUE)


theme_customs <- function() {
  ggplot2::theme_minimal(base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(color = "black", fill = "white", size = 1.5),
      legend.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
      axis.text.y  = ggplot2::element_text(size = 18),
      axis.text.x  = ggplot2::element_text(size = 24),
      axis.title.x = ggplot2::element_text(size = 24),
      axis.title.y = ggplot2::element_text(size = 28),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1))
    )
}


VAR_LABELS <- c(
  value = "Anti-Asian Bias",
  Female = "Female",
  EducYears = "Years of Education",
  frac_asian = "Fraction Asian in County",
  lnftotval = "Log Household Income"
)

OUTCOME_LABELS <- c(
  Asian_only = "Asian only",
  White_only = "White only",
  Asian_and_White = "Asian & White"
)

OUTCOME_COLS <- c(
  "Asian only" = "#2E8B57",
  "White only" = "#4169E1",
  "Asian & White" = "#FF8C00"
)

match_with_tolerance <- function(values, targets, tol = 1e-8) {
  if (!length(targets)) return(rep(FALSE, length(values)))
  vapply(
    values,
    function(v) if (is.na(v)) FALSE else any(abs(v - targets) <= tol),
    logical(1)
  )
}

snap_to_existing <- function(candidates, unique_x) {
  if (!length(candidates) || !length(unique_x)) return(numeric())
  sapply(candidates, function(val) unique_x[which.min(abs(unique_x - val))]) |> unique()
}

select_pp_highlights <- function(x_vals, var_name) {
  unique_x <- sort(unique(stats::na.omit(x_vals)))
  if (!length(unique_x)) return(unique_x)

  if (var_name %in% c("value", "Female")) {
    return(unique_x)
  }

  if (var_name %in% c("lnftotval", "frac_asian", "EducYears")) {
    rng <- range(unique_x)
    n_targets <- min(5, length(unique_x))
    if (n_targets <= 1) return(unique_x)
    candidates <- seq(rng[1], rng[2], length.out = n_targets)
    return(snap_to_existing(candidates, unique_x))
  }

  near_zero <- unique_x[which.min(abs(unique_x))]
  unique(c(min(unique_x), near_zero, max(unique_x)))
}

.checkpoint_paths <- function(label_slug, output_dir = bootstrap_results_path) {
  list(
    rds = file.path(output_dir, sprintf("logit_boot_results_%s.rds", label_slug)),
    metadata = file.path(output_dir, sprintf("logit_boot_results_%s.txt", label_slug))
  )
}

.safe_ggsave <- function(filename, plot, ...) {
  tryCatch({
    ggplot2::ggsave(filename = filename, plot = plot, ...)
    TRUE
  }, error = function(e) {
    warning("Failed to save plot '", basename(filename), "': ", conditionMessage(e))
    FALSE
  })
}

plot_pp_boot <- function(pp_summ, var_name, gen_label){
  pp <- pp_summ |>
    mutate(
      group_labeled = dplyr::recode(outcome, !!!as.list(OUTCOME_LABELS)),
      group_labeled = factor(group_labeled, levels = OUTCOME_LABELS),
      label = scales::percent(p_main, accuracy = 1)
    )
  x_label <- VAR_LABELS[[var_name]] %||% var_name

  highlight_x <- select_pp_highlights(pp$x_val, var_name)
  highlight_mask <- match_with_tolerance(pp$x_val, highlight_x)
  pp$show_label <- highlight_mask
  pp$show_ci <- highlight_mask

  if (var_name %in% c("Female")) {
    # ----- Binary branch: use factor x + shared dodge -----
    pp <- pp |> mutate(
      x_fac = factor(
        ifelse(x_val == 0, 0, 1),
        levels = c(0, 1),
        labels = c("Male", "Female")
      )
    )
    pd <- position_dodge(width = 0.45)
    ggplot(pp, aes(x = x_fac, y = p_main, color = group_labeled, group = group_labeled)) +
      geom_point(size = 3, position = pd) +
      geom_errorbar(
        data = pp |> filter(show_ci),
        aes(ymin = p_lo_plot, ymax = p_hi_plot),
        width = 0.15, linewidth = 1, position = pd
      ) +
      ggrepel::geom_text_repel(
        data = pp |> filter(show_label),
        aes(label = label),
        position = pd,
        size = 5,
        direction = "y",
        force = 1.2,
        max.time = 1,
        max.overlaps = Inf,
        box.padding = unit(0.4, "lines"),
        point.padding = unit(0.25, "lines"),
        min.segment.length = 0,
        segment.alpha = 0.5,
        segment.size = 0.4,
        segment.color = "grey40",
        seed = 123,
        show.legend = FALSE
      ) +
      scale_x_discrete(NULL) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      scale_color_manual(values = OUTCOME_COLS, name = "") +
      labs(x = x_label, y = "Probability", title = gen_label) +
      theme_customs() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5)
      )
  } else {
    # ----- Continuous branch: NO dodge; bars under dots -----
    rng <- range(pp$x_val)
    error_width <- (rng[2] - rng[1]) * 0.02
    ggplot(pp, aes(x = x_val, y = p_main, color = group_labeled, group = group_labeled)) +
      geom_line(linewidth = 1.2) +
      geom_errorbar(
        data = pp |> filter(show_ci),
        aes(ymin = p_lo_plot, ymax = p_hi_plot),
        width = error_width, linewidth = 1
      ) +
      geom_point(data = pp |> filter(show_label), size = 3) +
      ggrepel::geom_text_repel(
        data = pp |> filter(show_label),
        aes(label = label),
        size = 5,
        direction = "y",
        force = 1.2,
        max.time = 1.25,
        max.overlaps = Inf,
        box.padding = unit(0.4, "lines"),
        point.padding = unit(0.25, "lines"),
        min.segment.length = 0,
        segment.alpha = 0.5,
        segment.size = 0.4,
        segment.color = "grey40",
        seed = 123,
        show.legend = FALSE
      ) +
      scale_x_continuous(
        breaks = if (var_name %in% c("lnftotval", "frac_asian")) highlight_x else waiver(),
        labels = if (var_name == "frac_asian") percent_format(accuracy = 1) else waiver(),
        minor_breaks = if (var_name %in% c("lnftotval", "frac_asian")) NULL else waiver()
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      scale_color_manual(values = OUTCOME_COLS, name = "") +
      labs(x = x_label, y = "Probability", title = gen_label) +
      theme_customs() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5)
      )
  }
}

plot_me_boot <- function(me_summ, gen_label){
  me_summ |>
    mutate(
      variable_label = factor(variable, levels = names(VAR_LABELS), labels = VAR_LABELS),
      outcome_label  = factor(outcome, levels = names(OUTCOME_LABELS), labels = OUTCOME_LABELS),
      coef_label = sprintf("%.2f", ame_point)
    ) |>
    ggplot(aes(x = ame_point, y = variable_label, color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.8)) +
    geom_errorbarh(
      aes(xmin = ame_lo_plot, xmax = ame_hi_plot),
      height = 0.2, linewidth = 1.1,
      position = position_dodge(width = 0.8)
    ) +
    ggrepel::geom_text_repel(
      aes(label = coef_label),
      size = 4, max.overlaps = Inf,
      position = position_dodge(width = 0.8),
      point.padding = 0.3, box.padding = 0.5,
      segment.curvature = 0, fontface = "bold",
      show.legend = FALSE
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = OUTCOME_COLS, name = "Identity Choice") +
    labs(y = NULL, x = "Average marginal effect (per unit of variable)", title = gen_label) +
    theme_customs() +
    theme(legend.position = "bottom")
}

render_bootstrap_plots <- function(label_slug, label_pretty){
  paths <- .checkpoint_paths(label_slug)
  if (!file.exists(paths$rds)) {
    warning("No bootstrap RDS found for ", label_slug, "; skipping plot generation.")
    return(invisible(NULL))
  }

  boot_res <- readRDS(paths$rds)
  pp_tbl <- boot_res$pp
  if (!is.null(pp_tbl) && nrow(pp_tbl)) {
    vars_present <- unique(pp_tbl$variable)
    vars_present <- vars_present[!is.na(vars_present)]
    vars_present <- setdiff(vars_present, "frac_asian")  # Exclude frac_asian from plots
    for (var_name in vars_present) {
      pp_data <- pp_tbl |> dplyr::filter(variable == var_name)
      if (!nrow(pp_data)) next
      plt_pp <- plot_pp_boot(pp_data, var_name, label_pretty)
      .safe_ggsave(
        filename = file.path(bootstrap_results_path, sprintf("logit_boot_pp_%s_%s.png", label_slug, var_name)),
        plot = plt_pp,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  } else {
    warning("No PP results to plot for: ", label_pretty)
  }

  me_tbl <- boot_res$me
  if (!is.null(me_tbl) && nrow(me_tbl)) {
    # Filter out frac_asian from ME plots
    me_tbl <- me_tbl |> dplyr::filter(variable != "frac_asian")
    if (nrow(me_tbl) == 0) {
      warning("No ME results to plot after filtering for: ", label_pretty)
    } else {
      plt_me <- plot_me_boot(me_tbl, label_pretty)
      .safe_ggsave(
        filename = file.path(bootstrap_results_path, sprintf("logit_boot_me_%s.png", label_slug)),
        plot = plt_me,
        width = 10,
        height = 6,
        dpi = 300
      )
    }
  } else {
    warning("No ME results to plot for: ", label_pretty)
  }
}

# Models to plot - matching the four models from script 46
models_to_plot <- list(
  list(label_slug = "second_AA_adults_multinom", label_pretty = "Second gen adults: AA parents (Multinomial)"),
  list(label_slug = "second_AW_adults_multinom", label_pretty = "Second gen adults: AW parents (Multinomial)"),
  list(label_slug = "second_WA_adults_multinom", label_pretty = "Second gen adults: WA parents (Multinomial)"),
  list(label_slug = "second_gen_adults_multinom", label_pretty = "Second gen adults (Multinomial)")
)

for (spec in models_to_plot) {
  message(sprintf("Rendering plots for %s (%s)", spec$label_pretty, spec$label_slug))
  render_bootstrap_plots(spec$label_slug, spec$label_pretty)
}

message(sprintf("All figures saved in: %s", bootstrap_results_path))
