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
  
  vector[N_Y] log_a; // Value of a
  vector[N_Y] f; // Link with slope of a
  vector<lower=1, upper=365>[N_Y] b1;
  vector<lower=1, upper=365>[N_Y] b2;
  
  real b1_avg;
  real<lower=0> b1_sigma;
  real<lower=b1_avg> b2_avg;
  real<lower=0> b2_sigma;
  real log_a_avg;
  real<lower=0> log_a_sigma;
  real f_avg;
  real<lower=0> f_sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

transformed parameters{
  vector[N_Y] f_prob = inv_logit(f);
  vector[N_Y] a_abs = exp(log_a);
}

model {
  b1_avg ~ normal(150, 10);
  b1_sigma ~ cauchy(0, 7);
  
  b2_avg ~ normal(220, 10);
  b2_sigma ~ cauchy(0, 7);
  
  log_a_avg ~ normal(-1, 0.8);
  log_a_sigma ~ cauchy(0, 1);
  
  f_avg ~ normal(1, 0.5);
  f_sigma ~ cauchy(0, 0.5);
  
  prob_detect ~ beta(2, 5);
  log_a ~  normal(log_a_avg, log_a_sigma);
  f ~  normal(f_avg, f_sigma);
  b1 ~ normal(b1_avg, b1_sigma);
  b2 ~ normal(b2_avg, b2_sigma);
  
  
  y ~ bernoulli((1 - (1 - prob_detect)^effort) .* 
                inv_logit(a_abs[year] .* f_prob[year] .* (jj_date - b1[year])) .*
                inv_logit(-a_abs[year] .* (1 - f_prob[year]) .* (jj_date - b2[year])));
}
generated quantities {
  vector[n_new*N_Y] mu_line = inv_logit(a_abs[newyear] .* f_prob[newyear] .* (newdate - b1[newyear])) .*
                inv_logit(-a_abs[newyear] .* (1 - f_prob[newyear]) .* (newdate - b2[newyear]));
                
  vector[N_Y] stay = b2 - b1;
}
