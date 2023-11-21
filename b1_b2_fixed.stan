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
  real log_a1;
  real log_a2;
  
  vector<lower=1, upper=365>[N_Y] b1; // Value of B
  vector<lower=1, upper=365>[N_Y] b2;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  prob_detect ~ beta(2, 5);
  log_a1 ~ normal(-1, 1);
  log_a2 ~ normal(-1, 1);
  b1 ~ normal(150, 7);
  b2 ~ normal(210, 7);
  
  y ~ bernoulli((1 - (1 - prob_detect)^effort) .* (1 / (1 + exp(-exp(log_a1) .* (jj_date - b1[year]))) .* (1 / (1 + exp(exp(log_a2) .* (jj_date - b2[year]))))));
}
generated quantities {
  vector[n_new*N_Y] mu_line = (1 / (1 + exp(-exp(log_a1) .* (newdate - b1[newyear])))) .* (1 / (1 + exp(exp(log_a2) .* (newdate - b2[newyear]))));
}
