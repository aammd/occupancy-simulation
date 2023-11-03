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
simulate_occ_eff_time <-
  function(log_a1, log_a2, b1, b2, prob_detect, nsample = 200) {
    # prob_detect <- rbeta(1, 2, 5)
    #
    # log_a1 <- rnorm(1, -2, .2)
    # log_a2 <- rnorm(1, -2, .2)
    # b1 <- rnorm(1, 150, 7)
    # b2 <- rnorm(1, 220, 7)
    
    
    df_sim <- tibble(
      sample_id = 1:nsample,
      jj_date = floor(runif(
        n = nsample,
        min = 130, 
        max = 240
      )),
      real_pres = prob_pres_HOF(exp(log_a1),
                                exp(log_a2),
                                b1,
                                b2,
                                jj_date),
      effort = round(runif(
        n = nsample,
        min = 1, max = 25
      ))
    ) |>
      rowwise() |>
      mutate(
        pa = 1 - (1 - prob_detect) ^ effort,
        y = rbinom(n = 1, p = pa, size = 1) * rbinom(n = 1, p = real_pres, size = 1)
      )
    
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


simulate_occ_eff_time_param <- function(nsample = 200) {
  
  prob_detect <- rbeta(1, 2, 5)
  
  log_a1 <- rnorm(1, -1, .8)
  log_a2 <- rnorm(1, -1, .8)
  b1 <- floor(runif(1, 135, 175))
  b2 <- floor(runif(1, 185, 225))
  
  df_sim <- tibble(sample_id = 1:nsample,
                   jj_date = floor(runif(n = nsample,
                                   min = 130, max = 240)),
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



simulate_occ_effN_time_param <- function(nsample = 200) {
  
  prob_detect <- rbeta(1, 2, 5)
  
  log_a1 <- rnorm(1, -1, .8)
  log_a2 <- rnorm(1, -1, .8)
  b1 <- floor(runif(1, 135, 175))
  b2 <- floor(runif(1, 185, 225))
  
  df_sim <- tibble(sample_id = 1:nsample,
                   jj_date = floor(runif(n = nsample,
                                   min = 130, max = 240)),
                   real_pres = prob_pres_HOF(exp(log_a1),
                                             exp(log_a2), 
                                             b1, 
                                             b2, 
                                             jj_date),
                   effort = round(
                     rnorm(
                       n = nsample,
                       mean = 6,
                       sd = 2.5)
                   )) |>
    mutate(effort = if_else(effort <= 0, 0.5, effort)) |> 
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



simulate_occ_effN_timeN_param <- function(nsample = 200) {
  prob_detect <- rbeta(1, 2, 5)
  
  log_a1 <- rnorm(1,-1, .8)
  log_a2 <- rnorm(1,-1, .8)
  b1 <- floor(runif(1, 135, 175))
  b2 <- floor(runif(1, 185, 225))
  
  df_sim <- tibble(
    sample_id = 1:nsample,
    jj_date = floor(rnorm(
      n = nsample, 
      mean = 180, 
      sd = 15
    )),
    real_pres = prob_pres_HOF(exp(log_a1),
                              exp(log_a2),
                              b1,
                              b2,
                              jj_date),
    effort = round(rnorm(
      n = nsample,
      mean = 6,
      sd = 2.5
    ))
  ) |>
    mutate(effort = if_else(effort <= 0, 0.5, effort)) |>
    rowwise() |>
    mutate(
      pa = 1 - (1 - prob_detect) ^ effort,
      y = rbinom(n = 1, p = pa, size = 1) * rbinom(n = 1, p = real_pres, size = 1)
    )
  
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


load_duck <- function(data) {
  data <- data[[1]] |>
    group_by(Year,
             Date,
             Observed_sp,
             groupID,
             Nb_observers,
             Nb_field_hours) |>
    dplyr::select(Year,
                  Date,
                  Observed_sp,
                  Nb_ind,
                  groupID,
                  Nb_observers,
                  Nb_field_hours) |>
    summarise(Nb_ind = mean(Nb_ind)) |>
    tidyr::pivot_wider(
      names_from = Observed_sp,
      values_from = Nb_ind,
      values_fill = 0
    ) |>
    dplyr::ungroup() |>
    dplyr::select(Year, Date, 'Long-tailed Duck', Nb_observers, Nb_field_hours) |>
    rename("ltdu" = 'Long-tailed Duck') |>
    tidyr::drop_na() |>
    filter(Nb_field_hours > 0 & Nb_observers > 0) |>
    mutate(obs = ifelse(ltdu >= 1, yes = 1, no = 0)) |>
    mutate(Date = as.numeric(Date))
  
  list(
    N = nrow(data),
    y = data$obs,
    jj_date = data$Date,
    effort = data$Nb_field_hours
  )
}


stan_duck <- function(data) {
  model <- cmdstanr::cmdstan_model("occ_eff_time.stan")
  
  model_result <- model$sample(data = data)
  
  posterior <- model_result$draws()
  
  return(list(model_result,
              posterior))

} 




# Function to simulate data with :
# Presence ~ Date
# B1 ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_add_b1 <-
  function(log_a1, log_a2, b2, n.year, prob_detect, nsample = 200) {

    df_sim <- tibble()
    
    # Draw a new b1 for each year
    b1 <- floor(runif(n.year, 140, 160))
    
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF(exp(log_a1),
                                  exp(log_a2),
                                  b1[i],
                                  b2,
                                  jj_date),
        effort = round(runif(
          n = nsample,
          min = 1, max = 25
        ))
      ) |>
        rowwise() |>
        mutate(
          pa = 1 - (1 - prob_detect) ^ effort,
          y = rbinom(n = 1, p = pa, size = 1) * rbinom(n = 1, p = real_pres, size = 1)
        )
      
      df_sim <- rbind(df_sim, df_year)
    }
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = df_sim$year,
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
