library(targets)
library(stantargets)
library(tarchetypes)
library(quarto)

# Define custom functions and other global objects.
source("functions.R")
tar_option_set(seed = 3)

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "ggplot2"))

# End this file with a list of target objects.
list(
  tar_target(
    fake_data,
    head(simulate_occ_effort(prob_pres = .7,
                             prob_detect = .3,
                             nsample = 200
    ))
  ),
  tar_stan_mcmc_rep_summary( # Run models on multiple data sets with fixed parameter values
    simple_occ,
    stan_files = "simple_occ.stan",
    batches = 8,
    reps = 5,
    data = simulate_occ_effort(prob_pres = .7,
                               prob_detect = .3,
                               nsample = 200),
    variables = c("prob_pres", "prob_detect"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975)),
      mean = ~mean(.x)
    ),
    quiet = TRUE
  ),
  tar_target( # Evaluate the coverage of the posterior. What % of time the true parameters falls within the posterior interval
    cov_params,
    command = calc_coverage(simple_occ)
  ),
  tar_stan_mcmc_rep_summary( # Run models on multiples data sets with uniformly sample parameter values
    vary_params,
    stan_files = "simple_occ.stan",
    batches = 16,
    reps = 5,
    data = simulate_occ_eff_params(nsample = 200),
    variables = c("prob_pres", "prob_detect"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975)), # Extract the quantile interval of the posterior distributions
      mean = ~mean(.x)
    ),
    quiet = TRUE,
    refresh = 0L,
    parallel_chains = 4
  ),
  tar_target( # Evaluate the coverage of the posterior. What % of time the true parameters falls within the posterior interval
    cov_vary_params,
    command = calc_coverage(vary_params)
  ),
  tar_target(
    fake_data_time,
    head(simulate_occ_eff_time(log_a1 = log(1),
                               log_a2 = log(0.2),
                               b1 = 160,
                               b2 = 200,
                               prob_detect = 0.3,
                               nsample = 200))
  ),
  tar_stan_mcmc_rep_summary( # Run models on multiple data sets with fixed parameter values, but presence fct of time
    fixed_eff_time,
    stan_files = "occ_eff_time.stan",
    batches = 8,
    reps = 5,
    data = simulate_occ_eff_time(log_a1 = log(1),
                                 log_a2 = log(0.2),
                                 b1 = 160,
                                 b2 = 200,
                                 prob_detect = 0.3,
                                 nsample = 200),
    variables = c("prob_detect", "log_a1", "log_a2", "b1", "b2"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975)),
      mean = ~mean(.x)
    ),
    quiet = TRUE,
    refresh = 0L
  ),
  tar_target(
    cov_fixed_eff_time,
    command = calc_coverage(fixed_eff_time)
  ),
    tar_stan_mcmc_rep_summary( # Run models on multiples data sets with uniformly sample parameter values, but presence fct of time
    vary_eff_time,
    stan_files = "occ_eff_time.stan",
    batches = 8,
    reps = 5,
    data = simulate_occ_eff_time_param(nsample = 200),
    variables = c("prob_detect", "log_a1", "log_a2", "b1", "b2"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975)),
      mean = ~mean(.x)
    ),
    quiet = TRUE,
    refresh = 0L,
    parallel_chains = 4
  ),
  tar_target( # Evaluate the coverage of the posterior. What % of time the true parameters falls within the posterior interval
    cov_vary_time_params,
    command = calc_coverage(vary_eff_time)
  ),
  tar_target(
    obs_data,
    command = source(file = "load_data.R")
  ),
  tar_stan_mcmc_rep_summary( # Run models on multiples data sets with uniformly sample parameter values, but presence fct of time
    vary_effN_time,
    stan_files = "occ_eff_time.stan",
    batches = 8,
    reps = 5,
    data = simulate_occ_effN_time_param(nsample = 200),
    variables = c("prob_detect", "log_a1", "log_a2", "b1", "b2"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975)),
      mean = ~mean(.x)
    ),
    quiet = TRUE,
    refresh = 0L,
    parallel_chains = 4
  ),
  tar_stan_mcmc_rep_summary( # Run models on multiples data sets with uniformly sample parameter values, but presence fct of time
    vary_effN_timeN,
    stan_files = "occ_eff_time.stan",
    batches = 8,
    reps = 5,
    data = simulate_occ_effN_timeN_param(nsample = 200),
    variables = c("prob_detect", "log_a1", "log_a2", "b1", "b2"),
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975)),
      mean = ~mean(.x)
    ),
    quiet = TRUE,
    refresh = 0L,
    parallel_chains = 4
  ),
  tar_target(
    duck_data,
    command = load_duck(data = obs_data)
  ),
  tar_target(
    duck_fixed_year,
    command = stan_duck(duck_data)
  ),
  ## run a stan model and get the posterior
  tar_stan_mcmc(
    gq_post,stan_files = "occ_eff_time_gq.stan",data = duck_data, parallel_chains = 4
  ),
  tar_target(
    new_dates, 
    command = seq(from = 1, to = 365, by = 14)
  ),
  tar_target(
    duck_data_line,
    command = purrr::list_modify(duck_data, nline = length(new_dates), newdate = new_dates)
  ),
  tar_stan_mcmc(
    gq_line, 
    stan_files = "occ_eff_time_line.stan", data = duck_data_line, parallel_chains = 4
  ),
  
  
  # tar_stan_mcmc_rep_summary( # Run models on multiple data sets with fixed parameter values, but presence fct of time
  #   fixed_eff_time_log,
  #   stan_files = "occ_eff_time_log.stan",
  #   batches = 1,
  #   reps = 4,
  #   data = simulate_occ_eff_time_logscale(nsample = 200),
  #   variables = c("logit_prob_detect", "log_a1", "log_a2", "b1", "b2"),
  #   summaries = list(
  #     ~posterior::quantile2(.x, probs = c(0.025, 0.975))
  #   ),
  #   quiet = TRUE,
  #   refresh = 0L
  # ),
  # tar_target(
  #   cov_fixed_eff_time_log,
  #   command = calc_coverage(fixed_eff_time_log)
  # )
  tar_quarto(report, "occupancy_STAN.qmd")
)

