#!/usr/bin/env Rscript
# ================================================================
# Between-model AME difference coefficient plots
# Validates the preserved 1,000-replication checkpoints and comparison CSV.
# Never fits models or reruns a bootstrap. Produces four PNGs:
#   1. Second gen: AW vs. WA
#   2. Third gen: 1 vs. 2 Asian GP
#   3. Third gen: 1 vs. 3 Asian GP
#   4. Third gen: 2 vs. 3 Asian GP
# ================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})
resolve_root <- function() {
  env_root <- Sys.getenv("ASIAN_IDENTITY_ROOT")
  if (nzchar(env_root)) return(normalizePath(env_root, mustWork = TRUE))
  if (exists("git_mdir", inherits = TRUE)) {
    return(normalizePath(get("git_mdir", inherits = TRUE), mustWork = TRUE))
  }
  args <- commandArgs(trailingOnly = TRUE)
  normalizePath(if (length(args) >= 1L) args[[1L]] else getwd(), mustWork = TRUE)
}
root <- resolve_root()
source(file.path(root, "programs", "bootstrap-comparison-data.R"), local = TRUE)
figures_wd <- Sys.getenv("ASIAN_BOOTSTRAP_FIGURES_DIR",
                         unset = file.path(root, "output", "figures"))
dir.create(figures_wd, recursive = TRUE, showWarnings = FALSE)

# ---- color-blind-friendly palette ----
outcome_colors <- c(
  "Asian Only"    = "#0072B2",
  "White Only"    = "#D55E00",
  "Asian & White" = "#009E73"
)

outcome_shapes <- c(
  "Asian Only"    = 21,
  "White Only"    = 24,
  "Asian & White" = 22
)

# ---- load data ----
df <- read_bootstrap_comparisons(
  file.path(root, "data", "raw", "bootstrapping-data"))$data

# ---- clean variable and outcome labels ----
df <- df |>
  mutate(
    variable_label = case_when(
      variable == "value"          ~ "Anti-Asian bias",
      variable == "Female"         ~ "Female",
      variable == "MomGradCollege" ~ "College grad: mother",
      variable == "DadGradCollege" ~ "College grad: father",
      TRUE ~ variable
    ),
    variable_label = factor(variable_label,
                            levels = c("Anti-Asian bias",
                                       "Female",
                                       "College grad: mother",
                                       "College grad: father")),
    outcome_label = case_when(
      outcome == "Asian_only"      ~ "Asian Only",
      outcome == "White_only"      ~ "White Only",
      outcome == "Asian_and_White" ~ "Asian & White",
      TRUE ~ outcome
    ),
    outcome_label = factor(outcome_label,
                           levels = c("Asian Only", "White Only", "Asian & White")),
    ci_lo = diff_est - 1.96 * se_diff,
    ci_hi = diff_est + 1.96 * se_diff,
    sig05 = p_value < 0.05,
    fill_group = if_else(sig05, as.character(outcome_label), "ns"),
    fill_group = factor(fill_group,
                        levels = c("Asian Only", "White Only", "Asian & White", "ns"))
  )

# ---- plotting function ----
plot_between_model <- function(data, subtitle_text) {

  pd <- position_dodge(width = 0.5)

  ggplot(data, aes(x = variable_label, y = diff_est,
                   color = outcome_label, shape = outcome_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi, fill = fill_group),
                    position = pd, size = 0.7,
                    show.legend = TRUE) +
    scale_color_manual(values = outcome_colors, name = "Outcome") +
    scale_shape_manual(values = outcome_shapes, name = "Outcome") +
    scale_fill_manual(
      values = c(outcome_colors, "ns" = "white"),
      guide  = "none"
    ) +
    labs(
      subtitle = subtitle_text,
      x = NULL,
      y = "Difference in AME (95% CI)"
    ) +
    coord_flip() +
    # Same serif/grid-free style as script 45, explicit for standalone runs.
    theme_minimal(base_family = "serif") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold", size = rel(1)),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 24),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 28),
      axis.line = element_line(colour = "black"),
      legend.text = element_text(size = rel(1)),
      plot.subtitle = element_text(size = 11, face = "italic",
                                   margin = margin(b = 8)),
      plot.margin   = margin(t = 10, r = 35, b = 5, l = 5),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    guides(color = guide_legend(override.aes = list(size = 1.2)))
}

# ---- define comparisons ----
comparisons <- list(
  list(
    model_a  = "Second gen: AW",
    model_b  = "Second gen: WA",
    subtitle = "Difference in AME: Asian father–White mother vs.\nWhite father–Asian mother",
    filename = "between_model_second_AW_WA.png"
  ),
  list(
    model_a  = "Third gen: 1 Asian GP",
    model_b  = "Third gen: 2 Asian GP",
    subtitle = "Difference in AME: One Asian grandparent vs.\ntwo Asian grandparents",
    filename = "between_model_third_1v2GP.png"
  ),
  list(
    model_a  = "Third gen: 1 Asian GP",
    model_b  = "Third gen: 3 Asian GP",
    subtitle = "Difference in AME: One Asian grandparent vs.\nthree Asian grandparents",
    filename = "between_model_third_1v3GP.png"
  ),
  list(
    model_a  = "Third gen: 2 Asian GP",
    model_b  = "Third gen: 3 Asian GP",
    subtitle = "Difference in AME: Two Asian grandparents vs.\nthree Asian grandparents",
    filename = "between_model_third_2v3GP.png"
  )
)

# ---- generate and save plots ----
for (comp in comparisons) {
  plot_data <- df |>
    filter(model_a == comp$model_a, model_b == comp$model_b)

  p <- plot_between_model(plot_data, comp$subtitle)

  ggsave(
    filename = file.path(figures_wd, comp$filename),
    plot     = p,
    width    = 8,
    height   = 4.5,
    dpi      = 300,
    bg       = "white"
  )

  message("Saved: ", comp$filename)
}
