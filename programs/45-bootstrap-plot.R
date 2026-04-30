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

# ------------------------------------------------------------------
# Pairwise Wald tests for marginal effects
# ------------------------------------------------------------------
# For each variable, tests all pairwise differences between outcome
# AMEs using approximate SEs recovered from the bootstrap percentile
# CIs already stored in the ME summary.  No raw bootstrap draws needed.
#
#   SE_k  = (CI_hi_k - CI_lo_k) / (2 * 1.96)
#   z     = (AME_i - AME_j) / sqrt(SE_i^2 + SE_j^2)
#   p     = 2 * pnorm(-|z|)
#
# Returns a tidy data frame with the difference estimate, z-statistic,
# p-value, and significance stars  (* p<.10, ** p<.05, *** p<.01).
# ------------------------------------------------------------------
compute_pairwise_me_tests <- function(me_summ) {
  if (is.null(me_summ) || nrow(me_summ) == 0) return(NULL)

  results <- list()
  for (var_name in unique(me_summ$variable)) {
    vd <- me_summ |> dplyr::filter(variable == var_name)
    if (nrow(vd) < 2) next

    vd <- vd |> dplyr::mutate(
      se_approx = (ame_hi_plot - ame_lo_plot) / (2 * 1.96)
    )

    outcomes <- vd$outcome
    pairs <- combn(seq_along(outcomes), 2)
    for (k in seq_len(ncol(pairs))) {
      i <- pairs[1, k]; j <- pairs[2, k]
      diff_est <- vd$ame_point[i] - vd$ame_point[j]
      se_diff  <- sqrt(vd$se_approx[i]^2 + vd$se_approx[j]^2)
      if (!is.finite(se_diff) || se_diff < 1e-12) next

      z_stat  <- diff_est / se_diff
      p_value <- 2 * stats::pnorm(-abs(z_stat))

      sig <- dplyr::case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )

      results[[length(results) + 1]] <- data.frame(
        variable = var_name,
        outcome1 = outcomes[i],
        outcome2 = outcomes[j],
        diff_est = diff_est,
        se_diff  = se_diff,
        z_stat   = z_stat,
        p_value  = p_value,
        sig      = sig,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results)) dplyr::bind_rows(results) else NULL
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

plot_me_boot <- function(me_summ, gen_label, me_diff = NULL){
  DODGE_W <- 0.8
  var_labels <- c(
    "value"="Anti-Asian Bias","Female"="Female",
    "MomGradCollege"="College Graduate: Mother","DadGradCollege"="College Graduate: Father",
    "education"="Years of Education","income"="Household Income"
  )
  outcome_labels <- c(
    "Asian_only"="Asian only","White_only"="White only","Asian_and_White"="Asian & White"
  )
  cols <- c("Asian only"="#2E8B57","White only"="#4169E1","Asian & White"="#FF8C00")

  plot_data <- me_summ |>
    mutate(
      variable_label = factor(variable, levels=names(var_labels), labels=var_labels),
      outcome_label  = factor(outcome,  levels=names(outcome_labels), labels=outcome_labels),
      coef_label     = sprintf("%.2f", ame_point)
    )

  p <- ggplot(plot_data, aes(x = ame_point, y = variable_label, color = outcome_label)) +
    geom_point(size = 3, position = position_dodge(width = DODGE_W)) +
    geom_errorbarh(aes(xmin = ame_lo_plot, xmax = ame_hi_plot),
                   height = 0.2, linewidth = 1.1,
                   position = position_dodge(width = DODGE_W)) +
    ggrepel::geom_text_repel(aes(label = coef_label),
                             size = 4, max.overlaps = Inf,
                             position = position_dodge(width = DODGE_W),
                             point.padding = 0.3, box.padding = 0.5,
                             segment.curvature = 0, fontface = "bold",
                             show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = cols, name = "Identity Choice") +
    labs(y = NULL, x = "Average marginal effect (per unit of variable)", title = gen_label) +
    theme_customs() +
    theme(legend.position = "bottom")

  # ---- Bracket annotations for pairwise significance tests ----
  # Show ALL pairwise comparisons for the "value" variable (Anti-Asian Bias)
  # regardless of significance, so brackets are consistent across all plots.
  if (!is.null(me_diff) && nrow(me_diff) > 0) {

    sig_diff <- me_diff |>
      dplyr::filter(variable %in% me_summ$variable)

    if (nrow(sig_diff) > 0) {

      out_keys   <- names(outcome_labels)
      n_out      <- length(out_keys)
      dodge_offsets <- stats::setNames(
        DODGE_W * (seq_along(out_keys) - (n_out + 1) / 2) / n_out,
        out_keys
      )

      var_levs <- levels(droplevels(plot_data$variable_label))
      var_ypos <- stats::setNames(seq_along(var_levs), var_levs)

      x_maxes <- plot_data |>
        dplyr::group_by(variable_label) |>
        dplyr::summarize(x_max = max(ame_hi_plot, na.rm = TRUE), .groups = "drop")

      x_range      <- diff(range(c(plot_data$ame_lo_plot, plot_data$ame_hi_plot), na.rm = TRUE))
      bracket_gap  <- x_range * 0.08
      bracket_step <- x_range * 0.07
      tick_len     <- x_range * 0.015

      seg_list <- list()
      lab_list <- list()

      for (vn in unique(sig_diff$variable)) {
        vl <- var_labels[[vn]]
        if (is.na(vl) || !(vl %in% names(var_ypos))) next
        y_center <- var_ypos[[vl]]
        x_base   <- x_maxes$x_max[x_maxes$variable_label == vl]

        vd <- sig_diff |>
          dplyr::filter(variable == vn) |>
          dplyr::mutate(
            y1   = y_center + dodge_offsets[outcome1],
            y2   = y_center + dodge_offsets[outcome2],
            span = abs(y2 - y1)
          ) |>
          dplyr::arrange(span)

        for (idx in seq_len(nrow(vd))) {
          row <- vd[idx, ]
          x_bk <- x_base + bracket_gap + (idx - 1) * bracket_step
          y1 <- row$y1;  y2 <- row$y2

          seg_list[[length(seg_list) + 1]] <- data.frame(
            x = x_bk - tick_len, xend = x_bk, y = y1, yend = y1)
          seg_list[[length(seg_list) + 1]] <- data.frame(
            x = x_bk, xend = x_bk, y = y1, yend = y2)
          seg_list[[length(seg_list) + 1]] <- data.frame(
            x = x_bk - tick_len, xend = x_bk, y = y2, yend = y2)

          p_fmt <- if (row$p_value < 0.001) {
            sprintf("p < 0.001 %s", row$sig)
          } else {
            sprintf("p = %s %s", formatC(row$p_value, format = "f", digits = 3), row$sig)
          }
          lab_list[[length(lab_list) + 1]] <- data.frame(
            x = x_bk + tick_len * 0.8,
            y = (y1 + y2) / 2,
            label = p_fmt
          )
        }
      }

      if (length(seg_list) > 0) {
        seg_df <- dplyr::bind_rows(seg_list)
        lab_df <- dplyr::bind_rows(lab_list)

        p <- p +
          geom_segment(
            data = seg_df,
            aes(x = x, xend = xend, y = y, yend = yend),
            inherit.aes = FALSE,
            color = "gray30", linewidth = 0.35
          ) +
          geom_text(
            data = lab_df,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size = 2.8, color = "gray20", hjust = 0, vjust = 0.5,
            family = "serif"
          ) +
          coord_cartesian(clip = "off") +
          theme(plot.margin = margin(5.5, 80, 5.5, 5.5, unit = "pt"))
      }
    }
  }

  p
}

render_bootstrap_plots <- function(label_slug, label_pretty){
  paths <- .checkpoint_paths(label_slug)
  if (!file.exists(paths$rds)) {
    warning("No bootstrap RDS found for ", label_slug, "; skipping plot generation.")
    return(invisible(NULL))
  }

  boot_res <- readRDS(paths$rds)
  # Predicted-probability plots disabled — manuscript uses marginal-effect plots only.
  # Restore the block below to re-enable PP figure rendering.
  # pp_tbl <- boot_res$pp
  # if (!is.null(pp_tbl) && nrow(pp_tbl)) {
  #   vars_present <- unique(pp_tbl$variable)
  #   vars_present <- vars_present[!is.na(vars_present)]
  #   for (var_name in vars_present) {
  #     pp_data <- pp_tbl |> dplyr::filter(variable == var_name)
  #     if (!nrow(pp_data)) next
  #     plt_pp <- plot_pp_boot(pp_data, var_name, label_pretty)
  #     .safe_ggsave(
  #       filename = file.path(bootstrap_results_path, sprintf("boot_pp_%s_%s.png", label_slug, var_name)),
  #       plot = plt_pp,
  #       width = 8,
  #       height = 6,
  #       dpi = 300
  #     )
  #   }
  # } else {
  #   warning("No PP results to plot for: ", label_pretty)
  # }

  me_tbl <- boot_res$me
  if (!is.null(me_tbl) && nrow(me_tbl)) {
    # Compute pairwise Wald tests from ME summary stats
    me_diff <- compute_pairwise_me_tests(me_tbl)
    if (!is.null(me_diff) && nrow(me_diff) > 0) {
      message(sprintf("  Pairwise ME tests for %s:", label_pretty))
      for (r in seq_len(nrow(me_diff))) {
        row <- me_diff[r, ]
        message(sprintf("    %s: %s vs %s | diff = %.4f | z = %.3f | p = %.4f %s",
                        row$variable, row$outcome1, row$outcome2,
                        row$diff_est, row$z_stat, row$p_value, row$sig))
      }
    }

    plt_me <- plot_me_boot(me_tbl, label_pretty, me_diff = me_diff)
    .safe_ggsave(
      filename = file.path(bootstrap_results_path, sprintf("boot_me_%s.png", label_slug)),
      plot = plt_me,
      width = 12,
      height = 6,
      dpi = 300
    )
  } else {
    warning("No ME results to plot for: ", label_pretty)
  }
}

models_to_plot <- list(
  # Blocks not used in current manuscript — commented out
  # list(label_slug = "all_generations", label_pretty = "All generations"),
  # list(label_slug = "second_generation", label_pretty = "Second generation"),
  # list(label_slug = "third_generation", label_pretty = "Third generation"),
  # list(label_slug = "second_AA", label_pretty = "Second gen: AA parents"),
  # list(label_slug = "third_fourAsian", label_pretty = "Third gen: Four Asian grandparents"),
  list(label_slug = "second_AW", label_pretty = "Second gen: AW parents"),
  list(label_slug = "second_WA", label_pretty = "Second gen: WA parents"),
  list(label_slug = "second_AW_WA", label_pretty = "Second gen: AW + WA parents (pooled)"),
  list(label_slug = "third_oneAsian", label_pretty = "Third gen: One Asian grandparent"),
  list(label_slug = "third_twoAsian", label_pretty = "Third gen: Two Asian grandparents"),
  list(label_slug = "third_threeAsian", label_pretty = "Third gen: Three Asian grandparents"),
  list(label_slug = "third_oneTwoAsian", label_pretty = "Third gen: 1+2 Asian grandparents (pooled)"),
  list(label_slug = "third_oneTwoThreeAsian", label_pretty = "Third gen: 1+2+3 Asian grandparents (pooled)")
)

for (spec in models_to_plot) {
  message(sprintf("Rendering plots for %s (%s)", spec$label_pretty, spec$label_slug))
  render_bootstrap_plots(spec$label_slug, spec$label_pretty)
}

# ------------------------------------------------------------------
# Between-model Wald tests
# ------------------------------------------------------------------
# Test whether the AME for each (variable, outcome) differs between
# two independently bootstrapped models.  Because the bootstraps are
# drawn from disjoint samples (e.g., AW vs WA parents), the two AMEs
# are independent, so:
#
#   SE_k ≈ (CI_hi_k − CI_lo_k) / (2 × 1.96)
#   z    = (AME_a − AME_b) / sqrt(SE_a² + SE_b²)
#   p    = 2 Φ(−|z|)
#
# Significance stars: * p<.10, ** p<.05, *** p<.01.
# ------------------------------------------------------------------
compute_between_model_me_tests <- function(me_a, me_b, label_a, label_b) {
  if (is.null(me_a) || is.null(me_b) || nrow(me_a) == 0 || nrow(me_b) == 0) return(NULL)

  a <- me_a |> dplyr::transmute(
    variable, outcome,
    ame_a = ame_point,
    se_a  = (ame_hi_plot - ame_lo_plot) / (2 * 1.96)
  )
  b <- me_b |> dplyr::transmute(
    variable, outcome,
    ame_b = ame_point,
    se_b  = (ame_hi_plot - ame_lo_plot) / (2 * 1.96)
  )

  dplyr::inner_join(a, b, by = c("variable", "outcome")) |>
    dplyr::mutate(
      model_a = label_a,
      model_b = label_b,
      diff_est = ame_a - ame_b,
      se_diff  = sqrt(se_a^2 + se_b^2),
      z_stat   = ifelse(is.finite(se_diff) & se_diff > 1e-12, diff_est / se_diff, NA_real_),
      p_value  = ifelse(is.na(z_stat), NA_real_, 2 * stats::pnorm(-abs(z_stat))),
      sig = dplyr::case_when(
        is.na(p_value)   ~ "",
        p_value < 0.01   ~ "***",
        p_value < 0.05   ~ "**",
        p_value < 0.10   ~ "*",
        TRUE             ~ ""
      )
    ) |>
    dplyr::select(variable, outcome, model_a, model_b,
                  ame_a, ame_b, diff_est, se_diff, z_stat, p_value, sig)
}

.load_me_summary <- function(label_slug) {
  paths <- .checkpoint_paths(label_slug)
  if (!file.exists(paths$rds)) {
    warning("Cannot load ME summary — missing RDS for ", label_slug)
    return(NULL)
  }
  readRDS(paths$rds)$me
}

run_between_model_tests <- function(pairs_list, out_csv = NULL) {
  results <- list()
  for (pair in pairs_list) {
    me_a <- .load_me_summary(pair$a_slug)
    me_b <- .load_me_summary(pair$b_slug)
    res <- compute_between_model_me_tests(me_a, me_b, pair$a_label, pair$b_label)
    if (is.null(res) || nrow(res) == 0) next

    message(sprintf("Between-model ME tests: %s vs %s", pair$a_label, pair$b_label))
    for (r in seq_len(nrow(res))) {
      row <- res[r, ]
      message(sprintf("  %-16s | %-18s | diff = %+.4f | z = %+.3f | p = %.4f %s",
                      row$variable, row$outcome, row$diff_est, row$z_stat, row$p_value, row$sig))
    }
    results[[length(results) + 1]] <- res
  }
  out <- if (length(results)) dplyr::bind_rows(results) else NULL
  if (!is.null(out) && !is.null(out_csv)) {
    tryCatch(utils::write.csv(out, out_csv, row.names = FALSE),
             error = function(e) warning("Failed to write ", out_csv, ": ", conditionMessage(e)))
  }
  invisible(out)
}

between_pairs <- list(
  list(a_slug = "second_AW",     a_label = "Second gen: AW",
       b_slug = "second_WA",     b_label = "Second gen: WA"),
  list(a_slug = "third_oneAsian", a_label = "Third gen: 1 Asian GP",
       b_slug = "third_twoAsian", b_label = "Third gen: 2 Asian GP"),
  list(a_slug = "third_oneAsian",   a_label = "Third gen: 1 Asian GP",
       b_slug = "third_threeAsian", b_label = "Third gen: 3 Asian GP"),
  list(a_slug = "third_twoAsian",   a_label = "Third gen: 2 Asian GP",
       b_slug = "third_threeAsian", b_label = "Third gen: 3 Asian GP")
)

run_between_model_tests(
  between_pairs,
  out_csv = file.path(bootstrap_results_path, "boot_me_between_model_tests.csv")
)

message(sprintf("All figures saved in: %s", bootstrap_results_path))
