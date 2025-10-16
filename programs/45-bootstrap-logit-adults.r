# ================================================================
# BOOTSTRAP FOR ADULT SAMPLES
# ================================================================
source(file.path(programs, "bootstrap-functions.R"))

# Bootstrap for AW adults
me_aw_adults_boot <- bootstrap_all_marginal_effects(
  model = mnl_second_aw_adults,
  data_subset = CPS_IAT_adults_aw,
  gen_label = "Second gen adults: AW parents",
  variables = c("lnftotval", "EducYears", "Female", "value"),
  n_boot = N_BOOTSTRAP,
  n_cores = N_CORES
)

# Bootstrap for WA adults
me_wa_adults_boot <- bootstrap_all_marginal_effects(
  model = mnl_second_wa_adults,
  data_subset = CPS_IAT_adults_wa,
  gen_label = "Second gen adults: WA parents",
  variables = c("lnftotval", "EducYears", "Female", "value"),
  n_boot = N_BOOTSTRAP,
  n_cores = N_CORES
)

# Create plots
plot_me_aw_adults_boot <- plot_me_bootstrap(me_aw_adults_boot, 
                                           "Second gen adults: AW parents")
plot_me_wa_adults_boot <- plot_me_bootstrap(me_wa_adults_boot, 
                                           "Second gen adults: WA parents")

# Save
ggsave(file.path(figures_wd, "bootstrap_me_aw_adults.png"), 
       plot_me_aw_adults_boot, width = 10, height = 6, dpi = 300)
ggsave(file.path(figures_wd, "bootstrap_me_wa_adults.png"), 
       plot_me_wa_adults_boot, width = 10, height = 6, dpi = 300)