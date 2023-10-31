data {
  int<lower=0> N; // Number of observation
  int<lower=0> N.Y; // Number of years
  array[N] int<lower=0, upper=1> y; // Arrey of our observation (response)
  array[N] int<lower=1, upper=N.Y> year; // Indices of years
  vector[N] effort; // Vector of effort (explanatory variable)
  vector[N] jj_date; // Vector of observation data (explanatory variable)
}

// The parameters accepted by the model. Our model
// accepts five parameters.
parameters {
  real<lower=0, upper=1> prob_detect;
  real log_a1;
  real log_a2;
  real<lower=1, upper=365> b2;
  
  real<lower=1, upper=365> b1[N]; // Value of B 
  real mu;
  real<lower=0> tau;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  prob_detect ~ beta(2, 5);
  log_a1 ~ normal(-1, 1);
  log_a2 ~ normal(-1, 1);
  b2 ~ normal(210, 7);
  
  b1 ~ normal(mu, tau);
  y ~ bernoulli((1 - (1 - prob_detect)^effort) .* (1 / (1 + exp(-exp(log_a1) .* (jj_date - b1))) .* (1 / (1 + exp(exp(log_a2) .* (jj_date - b2))))));
}

