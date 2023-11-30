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
  
  real log_a; // Value of a
  real f; // Link with slope of a
  real<lower=1, upper=365> b2;
  
  vector<lower=1, upper=365>[N_Y] b1;
  
  real b1_avg;
  real<lower=0> b1_sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

transformed parameters{
//  vector[N_Y] f_prob = inv_logit(f);
//  vector[N_Y] a_abs = exp(log_a);
}

model {
  prob_detect ~ beta(2, 5);
  log_a ~  normal(-1, 0.8);
  f ~  normal(1, 0.5);
  b2 ~ normal(210, 7);
  
  b1_avg ~ normal(150, 7);
  b1_sigma ~ exponential();
  
  for(y in N.Y){
    
    b1[y] = normal_rng(b1_avg, b1_sigma)
    
    y ~ bernoulli((1 - (1 - prob_detect)^effort) .* 
                  inv_logit(-a_abs .* f_prob .* (jj_date - b1[y])) .*
                  inv_logit(a_abs .* (1 - f_prob) .* (jj_date - b2)));
  }
  
}
generated quantities {
  vector[n_new*N_Y] mu_line = inv_logit(-a_abs[newyear] .* f_prob[newyear] .* (newdate - b1[newyear])) .*
                inv_logit(a_abs[newyear] .* (1 - f_prob[newyear]) .* (newdate - b2[newyear]));
}