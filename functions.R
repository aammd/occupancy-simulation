# Function to simulate data with :
# Presence ~ 1
# Detection ~ Effort
# Fixed parameters values
simulate_occ_effort <- function(prob_pres, prob_detect, nsample = 200) {
  df_sim <- tibble(sample_id = 1:nsample,
         real_pres = rbinom(n = nsample,
                            p = prob_pres,
                            size = 1),
         effort = round(
           runif(
             n = nsample,
             min = 1, max = 25)
         )) |>
    rowwise() |> 
    mutate(pa = 1 - (1 - prob_detect)^effort,
           y = rbinom(n = 1, p = pa, size = 1) * real_pres)
  
  list(
    N = nsample,
    y = df_sim$y,
    effort = df_sim$effort,
    .join_data = list(
      prob_pres = prob_pres, 
      prob_detect = prob_detect
    )
  )
}

# Function to simulate data with :
# Presence ~ 1
# Detection ~ Effort
# Varying parameters values
simulate_occ_eff_params <- function(nsample = 200) {
  
  prob_pres <- rbeta(1, 2, 2)
  prob_detect <- rbeta(1, 2, 5)
  
  df_sim <- tibble(sample_id = 1:nsample,
         real_pres = rbinom(n = nsample,
                            p = prob_pres,
                            size = 1),
         effort = round(
           runif(
             n = nsample,
             min = 1, max = 25)
         )) |>
    rowwise() |> 
    mutate(pa = 1 - (1 - prob_detect)^effort,
           y = rbinom(n = 1, p = pa, size = 1) * real_pres)
  
  list(
    N = nsample,
    y = df_sim$y,
    effort = df_sim$effort,
    .join_data = list(
      prob_pres = prob_pres, 
      prob_detect = prob_detect
    )
  )
}

# Function to extract probability of presence based on a 
prob_pres_HOF <- function(a1, a2, b1, b2, jj_date) ((1 / (1 + exp(-a1*(jj_date - b1)))) * (1 / (1 + exp(a2*(jj_date - b2)))))

# Function to simulate data with :
# Presence ~ Date
# Detection ~ Effort
# Fixed parameters values
simulate_occ_eff_time <- function(nsample = 200) {
  
  prob_detect <- rbeta(1, 2, 5)
  
  log_a1 <- rnorm(1, -2, .2)
  log_a2 <- rnorm(1, -2, .2)
  b1 <- rnorm(1, 150, 7)
  b2 <- rnorm(1, 220, 7)
  
  
  df_sim <- tibble(sample_id = 1:nsample,
                   jj_date = runif(n = nsample,
                                   min = 130, max = 240),
                   real_pres = prob_pres_HOF(exp(log_a1),
                                             exp(log_a2), 
                                             b1, 
                                             b2, 
                                             jj_date),
                   effort = round(
                     runif(
                       n = nsample,
                       min = 1, max = 25)
                   )) |>
    rowwise() |> 
    mutate(pa = 1 - (1 - prob_detect)^effort,
           y = rbinom(n = 1, p = pa, size = 1) * rbinom(n = 1, p = real_pres, size = 1))
  
  list(
    N = nsample,
    y = df_sim$y,
    jj_date = df_sim$jj_date,
    effort = df_sim$effort,
    .join_data = list(
      prob_detect = prob_detect,
      log_a1 = log_a1, 
      log_a2 = log_a2,
      b1 = b1, 
      b2 = b2
    )
  )
}

# Function to extract the quantile interval of the posterior distributions
calc_coverage <- function(df_joined){
  df_joined |> 
    group_by(variable) |> 
    summarize(coverage = mean(q2.5 < .join_data & .join_data < q97.5))
  
}




simulate_occ_eff_time_logscale <- function(nsample = 200) {
  
  logit_prob_detect <- rnorm(1, -1, .2)
  
  log_a1 <- rnorm(1, -2, .2)
  log_a2 <- rnorm(1, -2, .2)
  b1 <- rnorm(1, 150, 7)
  b2 <- rnorm(1, 220, 7)
  
  prob_detect <- plogis(logit_prob_detect)
  
  df_sim <- tibble(sample_id = 1:nsample,
                   jj_date = runif(n = nsample,
                                   min = 130, max = 240),
                   real_pres = prob_pres_HOF(exp(log_a1),
                                             exp(log_a2), 
                                             b1, 
                                             b2, 
                                             jj_date),
                   effort = round(
                     runif(
                       n = nsample,
                       min = 1, max = 25)
                   )) |>
    rowwise() |> 
    mutate(pa = 1 - (1 - prob_detect)^effort,
           y = rbinom(n = 1, p = pa, size = 1) * rbinom(n = 1, p = real_pres, size = 1))
  
  list(
    N = nsample,
    y = df_sim$y,
    jj_date = df_sim$jj_date,
    effort = df_sim$effort,
    .join_data = list(
      logit_prob_detect = logit_prob_detect,
      log_a1 = log_a1, 
      log_a2 = log_a2,
      b1 = b1, 
      b2 = b2
    )
  )
}
