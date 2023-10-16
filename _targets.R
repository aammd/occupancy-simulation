library(targets)
library(stantargets)
library(dplyr)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("functions.R")
tar_option_set(seed = 3)

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "ggplot2"))

# data_values <- expand.grid(
#   n_per_group = c(1, 3, 5, 10, 15),
#   J = c(3, 10, 20)) |>
#   mutate(sim_id = paste0("sim", 1:length(n_per_group)))

# End this file with a list of target objects.
list(
  tar_stan_mcmc_rep_summary(
    simple_occ,
    stan_files = "simple_occ.stan",
    batches = 4,
    reps = 3,
    data = simulate_occ_effort(prob_pres = .7, 
                               prob_detect = .3, 
                               nsample = 200),
    variables = c("prob_pres", "prob_detect"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975))
    ),
    quiet = TRUE
  ),
  tar_stan_mcmc_rep_summary(
    vary_params,
    stan_files = "simple_occ.stan",
    batches = 8,
    reps = 5,
    data = simulate_occ_eff_params(nsample = 200),
    variables = c("prob_pres", "prob_detect"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975))
    ),
    quiet = TRUE,
    refresh = 0L,
    parallel_chains = 4
  ),
  tar_target(
    cov_vary_params,
    command = calc_coverage(vary_params)
  )
  # tar_stan_mcmc_rep_summary(
  #   somegroup, stan_files = "some_groups.stan",
  #   batches = 4,
  #   reps = 3,
  #   data = simulate_normal_group(n_per_group = 3, J = 10),
  #   quiet = TRUE
  # ),
  # tar_target(
  #   name = no_groups,
  #   command = cmdstanr::cmdstan_model(stan_file = "no_groups.stan")
  # ),
  # tar_target(
  #   name = some_groups,
  #   command = cmdstanr::cmdstan_model(stan_file = "some_groups.stan")
  # ),
  # tar_stan_mcmc(
  #   demo_groups,
  #   stan_files = "some_groups.stan",
  #   data = simulate_normal_group(n_per_group = 10, J = 10),
  #   quiet = TRUE
  # ),
  # tarchetypes::tar_map_rep(
  #   increase_group_reps,
  #   command = compare_two_models_loo(
  #     model1 = no_groups,
  #     model2 = some_groups,
  #     names = c("no_groups", "some_groups"),
  #     n_per_group = n_per_group, J = J),
  #   values = data_values,
  #   batches = 2,
  #   reps = 4,
  #   names = tidyselect::any_of("scenario")
  # ),
  # tar_target(
  #   power_fig,
  #   command = increase_group_reps |>
  #     filter(model == "no_groups") |>
  #     ggplot(aes(x = n_per_group, y = elpd_diff)) + geom_point() +
  #     facet_wrap(~J)
  # ),
  # tarchetypes::tar_render(
  #   readme,
  #   path = "README.Rmd"
  # )
)

