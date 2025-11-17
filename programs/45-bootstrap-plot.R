# ================================================================
# Bootstrap plotter: loads saved bootstrap results and renders PP/ME
# figures for each specified model configuration.
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

.checkpoint_paths <- function(label_slug, output_dir = bootstrap_results_path) {
  list(
    rds = file.path(output_dir, sprintf("boot_results_%s.rds", label_slug)),
    metadata = file.path(output_dir, sprintf("boot_results_%s.txt", label_slug))
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
  var_labels <- c("value"="Anti-Asian Bias","Female"="Female",
                  "MomGradCollege"="College Graduate: Mother","DadGradCollege"="College Graduate: Father",
                  "education"="Years of Education","income"="Household Income")

  pp <- pp_summ |>
    mutate(
      group_labeled = dplyr::recode(outcome,
        "Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White"),
      group_labeled = factor(group_labeled, levels=c("Asian only","White only","Asian & White")),
      label = scales::percent(p_main, accuracy = 1)
    )

  # which CIs/labels to show
  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    pp$show_label <- TRUE; pp$show_ci <- TRUE
  } else if (var_name == "value") {
    pp$show_label <- TRUE; pp$show_ci <- TRUE
  } else {
    near0 <- which.min(abs(pp$x_val))
    pp$show_label <- pp$x_val %in% c(min(pp$x_val), max(pp$x_val)) | seq_len(nrow(pp))==near0
    pp$show_ci <- pp$show_label
  }

  cols <- c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00")

  if (var_name %in% c("Female","MomGradCollege","DadGradCollege")) {
    # ----- Binary branch: use factor x + shared dodge -----
    pp <- pp |> mutate(x_fac = factor(ifelse(x_val==0, 0, 1), levels=c(0,1),
                                      labels=if (var_name=="Female") c("Male","Female") else c("No College","College Graduate")))
    pd <- position_dodge(width=0.45)
    ggplot(pp, aes(x=x_fac, y=p_main, color=group_labeled, group=group_labeled)) +
      geom_point(size=3, position=pd) +
      geom_errorbar(data=pp |> filter(show_ci),
                    aes(ymin=p_lo_plot, ymax=p_hi_plot),
                    width=0.15, linewidth=1, position=pd) +
      ggrepel::geom_text_repel(data=pp |> filter(show_label),
                               aes(label=label), position=pd, size=6,
                               max.overlaps=Inf, box.padding=unit(0.35,"lines"),
                               point.padding=unit(0.3,"lines"),
                               min.segment.length=0, segment.alpha=0.6,
                               seed=123, show.legend=FALSE) +
      scale_x_discrete(NULL) +
      scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
      scale_color_manual(values=cols, name="") +
      labs(x=var_labels[[var_name]], y="Probability", title=gen_label) +
      theme_customs() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major.x=element_line(color="grey90", linewidth=0.5),
            panel.grid.major.y=element_line(color="grey90", linewidth=0.5))
  } else {
    # ----- Continuous branch: NO dodge; bars under dots -----
    rng <- range(pp$x_val); error_width <- (rng[2]-rng[1]) * 0.02
    ggplot(pp, aes(x=x_val, y=p_main, color=group_labeled, group=group_labeled)) +
      geom_line(linewidth=1.2) +
      geom_errorbar(data=pp |> filter(show_ci),
                    aes(ymin=p_lo_plot, ymax=p_hi_plot),
                    width=error_width, linewidth=1) +
      geom_point(size=3) +
      ggrepel::geom_text_repel(data=pp |> filter(show_label),
                               aes(label=label), size=6,
                               max.overlaps=Inf, box.padding=unit(0.35,"lines"),
                               point.padding=unit(0.3,"lines"),
                               min.segment.length=0, segment.alpha=0.6,
                               seed=123, show.legend=FALSE) +
      scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
      scale_color_manual(values=cols, name="") +
      labs(x=var_labels[[var_name]], y="Probability", title=gen_label) +
      theme_customs() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major.x=element_line(color="grey90", linewidth=0.5),
            panel.grid.major.y=element_line(color="grey90", linewidth=0.5))
  }
}

plot_me_boot <- function(me_summ, gen_label){
  var_labels <- c(
    "value"="Anti-Asian Bias","Female"="Female",
    "MomGradCollege"="College Graduate: Mother","DadGradCollege"="College Graduate: Father",
    "education"="Years of Education","income"="Household Income"
  )
  outcome_labels <- c(
    "Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White"
  )
  cols <- c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00")

  me_summ |>
    mutate(
      variable_label = factor(variable, levels=names(var_labels), labels=var_labels),
      outcome_label  = factor(outcome,  levels=names(outcome_labels), labels=outcome_labels),
      coef_label     = sprintf("%.2f", ame_point)
    ) |>
    ggplot(aes(x = ame_point, y = variable_label, color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = 0.8)) +
    geom_errorbarh(aes(xmin = ame_lo_plot, xmax = ame_hi_plot),
                   height = 0.2, linewidth = 1.1,
                   position = position_dodge(width = 0.8)) +
    ggrepel::geom_text_repel(aes(label = coef_label),
                             size = 4, max.overlaps = Inf,
                             position = position_dodge(width = 0.8),
                             point.padding = 0.3, box.padding = 0.5,
                             segment.curvature = 0, fontface = "bold",
                             show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = cols, name = "Identity Choice") +
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
    for (var_name in vars_present) {
      pp_data <- pp_tbl |> dplyr::filter(variable == var_name)
      if (!nrow(pp_data)) next
      plt_pp <- plot_pp_boot(pp_data, var_name, label_pretty)
      .safe_ggsave(
        filename = file.path(bootstrap_results_path, sprintf("boot_pp_%s_%s.png", label_slug, var_name)),
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
    plt_me <- plot_me_boot(me_tbl, label_pretty)
    .safe_ggsave(
      filename = file.path(bootstrap_results_path, sprintf("boot_me_%s.png", label_slug)),
      plot = plt_me,
      width = 10,
      height = 6,
      dpi = 300
    )
  } else {
    warning("No ME results to plot for: ", label_pretty)
  }
}

models_to_plot <- list(
  list(label_slug = "all_generations", label_pretty = "All generations"),
  list(label_slug = "second_generation", label_pretty = "Second generation"),
  list(label_slug = "third_generation", label_pretty = "Third generation"),
  list(label_slug = "second_AA", label_pretty = "Second gen: AA parents"),
  list(label_slug = "second_AW", label_pretty = "Second gen: AW parents"),
  list(label_slug = "second_WA", label_pretty = "Second gen: WA parents"),
  list(label_slug = "third_oneAsian", label_pretty = "Third gen: One Asian grandparent"),
  list(label_slug = "third_twoAsian", label_pretty = "Third gen: Two Asian grandparents"),
  list(label_slug = "third_threeAsian", label_pretty = "Third gen: Three Asian grandparents"),
  list(label_slug = "third_fourAsian", label_pretty = "Third gen: Four Asian grandparents")
)

for (spec in models_to_plot) {
  message(sprintf("Rendering plots for %s (%s)", spec$label_pretty, spec$label_slug))
  render_bootstrap_plots(spec$label_slug, spec$label_pretty)
}

message(sprintf("All figures saved in: %s", bootstrap_results_path))
