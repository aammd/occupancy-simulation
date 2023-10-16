
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

calc_coverage <- function(df_joined){
  df_joined |> 
    group_by(variable) |> 
    summarize(coverage = mean(q2.5 < .join_data & .join_data < q97.5))
  
}