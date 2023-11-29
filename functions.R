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


# Function to plot the fake data
plot_fake <- function(fake_data){
  
  n.year <- length(unique(fake_data$year))
  
  if (length(fake_data$.join_data$log_a1) != n.year) {
    log_a1 <- rep(fake_data$.join_data$log_a1, n.year)
  } else {
    log_a1 <- fake_data$.join_data$log_a1
  }
  
  if (length(fake_data$.join_data$log_a2) != n.year) {
    log_a2 <- rep(fake_data$.join_data$log_a2, n.year)
  } else {
    log_a2 <- fake_data$.join_data$log_a2
  }
  
  if (length(fake_data$.join_data$b1) != n.year) {
    b1 <- rep(fake_data$.join_data$b1, n.year)
  } else {
    b1 <- fake_data$.join_data$b1
  }
  
  if (length(fake_data$.join_data$b2) != n.year) {
    b2 <- rep(fake_data$.join_data$b2, n.year)
  } else {
    b2 <- fake_data$.join_data$b2
  }
  
  fake_year <- data.frame(year = unique(fake_data$year),
                           a1 = log_a1,
                           a2 = log_a2,
                           b1 = b1,
                           b2 = b2,
                           xlim = rep(seq(130, 240, by = 1), each = n.year))
  
  
  fake_year$y <-  with(fake_year,  1 / (1 + exp(-exp(a1) * (xlim - b1))) * 1 / (1 + exp(exp(a2) * (xlim - b2))))
  
  fake_data |> 
    as.data.frame() |> 
    group_by(y, year, jj_date) |> 
    count() |> 
    ggplot(aes(x = jj_date, y = y)) +
    geom_point(alpha = 0.2, aes(size = n)) +
    theme_classic() +
    geom_line(data = fake_year, mapping = aes(x = xlim, y = y)) +
    facet_wrap(~ year)
}


# Function to plot the fake data with logit detour
plot_fake_logit <- function(fake_data){
  
  n.year <- length(unique(fake_data$year))
  
  if (length(fake_data$.join_data$a) != n.year) {
    a <- rep(fake_data$.join_data$a, n.year)
  } else {
    a <- fake_data$.join_data$a
  }
  
  if (length(fake_data$.join_data$f) != n.year) {
    f <- rep(fake_data$.join_data$f, n.year)
  } else {
    f <- fake_data$.join_data$f
  }
  
  if (length(fake_data$.join_data$b1) != n.year) {
    b1 <- rep(fake_data$.join_data$b1, n.year)
  } else {
    b1 <- fake_data$.join_data$b1
  }
  
  if (length(fake_data$.join_data$b2) != n.year) {
    b2 <- rep(fake_data$.join_data$b2, n.year)
  } else {
    b2 <- fake_data$.join_data$b2
  }
  
  fake_year <- data.frame(year = unique(fake_data$year),
                          a = a,
                          f = f,
                          b1 = b1,
                          b2 = b2,
                          xlim = rep(seq(130, 240, by = 1), each = n.year))
  
  
  fake_year$y <-  with(fake_year, plogis(exp(a)*plogis(f) * (xlim - b1)) * plogis(-exp(a)*(1-plogis(f)) * (xlim - b2)))
  
  fake_data |> 
    as.data.frame() |> 
    group_by(y, year, jj_date) |> 
    count() |> 
    ggplot(aes(x = jj_date, y = y)) +
    geom_point(alpha = 0.2, aes(size = n)) +
    theme_classic() +
    geom_line(data = fake_year, mapping = aes(x = xlim, y = y)) +
    facet_wrap(~ year)
}


# Function to simulate data with :
# Presence ~ Date
# B1 ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_add_b1 <-
  function(log_a1, log_a2, b2, n.year, prob_detect, nsample = 200, n_new = 20) {

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
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        log_a1 = log_a1,
        log_a2 = log_a2,
        b1 = b1,
        b2 = b2
      )
    )
  }


# Function to simulate data with :
# Presence ~ Date
# B1 ~ Year
# B2 ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_add_b1_b2 <-
  function(log_a1, log_a2, n.year, prob_detect, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new b1 for each year
    b1 <- floor(runif(n.year, 140, 160))
    # Draw a new b1 for each year
    b2 <- floor(runif(n.year, 190, 210))
    
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
                                  b2[i],
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
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        log_a1 = log_a1,
        log_a2 = log_a2,
        b1 = b1,
        b2 = b2
      )
    )
  }




# Function to simulate data with :
# Presence ~ Date
# log_a1 ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_add_a1 <-
  function(log_a2, b1, b2, n.year, prob_detect, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new log_a1 for each year
    log_a1 <- rnorm(n.year, -1.5, 1)
    
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF(exp(log_a1[i]),
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
      
      df_sim <- rbind(df_sim, df_year)
    }
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        log_a1 = log_a1,
        log_a2 = log_a2,
        b1 = b1,
        b2 = b2
      )
    )
  }



# Function to simulate data with :
# Presence ~ Date
# log_a1 ~ Year
# log_a2 ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_add_a1_a2 <-
  function(b1, b2, n.year, prob_detect, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new log_a1 for each year
    log_a1 <- rnorm(n.year, -1, 0.8)
    # Draw a new log_a2 for each year
    log_a2 <- rnorm(n.year, -1, 0.8)
    
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF(exp(log_a1[i]),
                                  exp(log_a2[i]),
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
      
      df_sim <- rbind(df_sim, df_year)
    }
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        log_a1 = log_a1,
        log_a2 = log_a2,
        b1 = b1,
        b2 = b2
      )
    )
  }


# LOGIT
# Function to simulate data with :
# Presence ~ Date
# log_a1 ~ Year
# log_a2 ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_a_logit <-
  function(b1, b2, n.year, prob_detect, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new log_a1 for each year
    log_a1 <- rnorm(n.year, -1, 0.2)
    # Draw a new log_a2 for each year
    logit_f <- rnorm(n.year, 1, 0.5)
    
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF(exp(log_a1[i]),
                                  exp(log_a1[i] + plogis(logit_f[i], log.p = TRUE)),
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
      
      df_sim <- rbind(df_sim, df_year)
    }
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        log_a1 = log_a1,
        logit_f = logit_f,
        b1 = b1,
        b2 = b2
      )
    )
  }



## HOF curve logis
prob_pres_HOF_logis <- 
  function(a, logis_f, b1, b2, jj_date){
    plogis(a*logis_f * (jj_date - b1)) * plogis(-a*(1-logis_f) * (jj_date - b2))
}

# LOGIT detour with f fraction to influence the a parameter
# Function to simulate data with :
# Presence ~ Date
# a ~ Year
# f ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_logis <-
  function(b1, b2, n.year, prob_detect, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new log_a1 for each year
    a <- rnorm(n.year, 0, 1)
    exp_a <- exp(a)
    # Draw a fraction added/subtracted to a
    f <- rnorm(n.year, 1, 1)
    logis_f <- plogis(f)
    
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF_logis(a = exp_a[i],
                                        logis_f = logis_f[i],
                                        b1 = b1,
                                        b2 = b2,
                                        jj_date = jj_date),
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
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        a = a,
        f = f,
        b1 = b1,
        b2 = b2
      )
    )
  }



# Function to simulate data with :
# Presence ~ Date
# b1 & b2 ~ Year
# log_a & f ~ Year
# Detection ~ Effort
# Fixed parameters values
simulate_add_all <-
  function(n.year, prob_detect, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new b1 for each year
    b1 <- floor(runif(n.year, 140, 160))
    
    # Draw a new b1 for each year
    b2 <- floor(runif(n.year, 190, 210))
    
    # Draw a new log_a1 for each year
    a <- rnorm(n.year, 0, 1)
    exp_a <- exp(a)
    # Draw a fraction added/subtracted to a
    f <- rnorm(n.year, 1, 1)
    logis_f <- plogis(f)
    
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF_logis(a = exp_a[i],
                                        logis_f = logis_f[i],
                                        b1 = b1[i],
                                        b2 = b2[i],
                                        jj_date = jj_date),
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
    
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        a = a,
        f = f,
        b1 = b1,
        b2 = b2
      )
    )
  }




# Format the duck data for the model with all ~ year
load_duck_all <- function(data, n_new = 20) {
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
  
  n.year <- unique(data$Year)
  
  ## make a fake data-frame for predicting
  newdat <- expand.grid(
    newdate = seq(from = 120, to = 250, length.out = n_new),
    newyear = 1:length(n.year))
  
  list(
    N = nrow(data),
    N_Y = length(n.year),
    y = data$obs,
    year = as.factor(data$Year),
    effort = data$Nb_field_hours,
    jj_date = data$Date,
    n_new = n_new,
    newdate = newdat$newdate,
    newyear = newdat$newyear
  )
}



# Function to simulate hierarchical data with :
# Presence ~ Date
# b1 & b2 ~ Year
# log_a & f ~ Year
# Detection ~ Effort
simulate_hierarchical <-
  function(n.year, prob_detect, 
           b1.mu = 150, b1.sigma = 2,
           b2.mu = 200, b2.sigma = 2, 
           a.mu  = -1,    a.sigma = 0.5,
           f.mu  = 1,    f.sigma = 0.5, nsample = 200, n_new = 20) {
    
    df_sim <- tibble()
    
    # Draw a new b1 for each year
    ## Each value is nested in a normal distribution with mean of b1.mu and variance of b1.sigma
    b1 <- floor(rnorm(n.year, mean = b1.mu, sd = b1.sigma))
    
    # Draw a new b1 for each year
    ## Each value is nested in a normal distribution with mean of b2.mu and variance of b2.sigma
    b2 <- floor(rnorm(n.year, mean = b2.mu, sd = b2.sigma))
    
    # Draw a new log_a1 for each year
    # Draw value in normal distribution with mean of a.mu and variance of a.sigma
    a <- rnorm(n.year, a.mu, a.sigma)
    exp_a <- exp(a)
    
    # Draw a fraction added/subtracted to a (slope)
    # Draw value in normal distribution with mean of f.mu and variance of f.sigma
    f <- rnorm(n.year, f.mu, f.sigma)
    logis_f <- plogis(f)
    
    # Create the fake data for each year
    for (i in 1:n.year) {
      
      df_year <- tibble(
        sample_id = 1:nsample,
        year = 2009 + i,
        jj_date = floor(runif(
          n = nsample,
          min = 130,
          max = 240
        )),
        real_pres = prob_pres_HOF_logis(a = exp_a[i],
                                        logis_f = logis_f[i],
                                        b1 = b1[i],
                                        b2 = b2[i],
                                        jj_date = jj_date),
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
    
    ## make a fake data-frame for predicting
    newdat <- expand.grid(
      newdate = seq(from = 120, to = 250, length.out = n_new),
      newyear = 1:n.year)
    
    # Return fake data set in list
    list(
      N = nsample*n.year,
      N_Y = n.year,
      y = df_sim$y,
      year = as.factor(df_sim$year),
      jj_date = df_sim$jj_date,
      effort = df_sim$effort,
      n_new = n_new,
      newdate = newdat$newdate,
      newyear = newdat$newyear,
      .join_data = list(
        prob_detect = prob_detect,
        a = a,
        f = f,
        b1 = b1,
        b2 = b2,
        hyperparameters = list(
          a.mu = a.mu,
          a.sigma = a.sigma,
          f.mu = f.mu,
          f.sigma = f.sigma,
          b1.mu = b1.mu,
          b1.sigma = b1.sigma,
          b2.mu = b2.mu,
          b2.sigma = b2.sigma
        )
      )
    )
  }