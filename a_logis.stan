data {
  int<lower = 1> N; // Number of observation
  int<lower = 1> N_Y; // Number of years
  array[N] int<lower = 0, upper = 1> y; // Arrey of our observation (response)
  array[N] int<lower = 1, upper = N_Y> year; // Indices of years
  vector[N] effort; // Vector of effort (explanatory variable)
  vector[N] jj_date; // Vector of observation data (explanatory variable)
  int<lower=0> n_new;
  vector[n_new*N_Y] newdate;
  array[n_new*N_Y] int<lower=1, upper = N_Y> newyear;
}

// The parameters accepted by the model. Our model
// accepts five parameters.
parameters {
  real<lower=0, upper=1> prob_detect;
  
  vector[N_Y] a; // Value of a
  vector[N_Y] logis_f; // Link with slope of a
  real<lower=1, upper=365> b1;
  real<lower=1, upper=365> b2;
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  prob_detect ~ beta(2, 5);
//  log_a1 ~ normal(-1, 1);
//  logit_f ~ plogis(normal(1.5, 0.5));
  b1 ~ normal(150, 7);
  b2 ~ normal(210, 7);
  
//  y ~ bernoulli((1 - (1 - prob_detect)^effort) .* (1 / (1 + exp(-exp(log_a1[year]) .* (jj_date - b1))) .* (1 / (1 + exp(exp(log_a1[year] + log_inv_logit(logit_f[year]))  .* (jj_date - b2))))));
}
generated quantities {
//  vector[n_new*N_Y] mu_line = (1 / (1 + exp(-exp(log_a1[newyear]) .* (newdate - b1)))) .* (1 / (1 + exp(exp(log_a1[newyear] + log_inv_logit(logit_f[newyear])) .* (newdate - b2))));
}
