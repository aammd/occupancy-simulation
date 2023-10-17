data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
  vector[N] effort;
  vector[N] jj_date;
}

// The parameters accepted by the model. Our model
// accepts five parameters.
parameters {
  real logit_prob_detect;
  real log_a1;
  real log_a2;
  real<lower=1, upper=365> b1;
  real<lower=b1, upper=365> b2;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[N] mu;
  logit_prob_detect ~ normal(-1, .2);
  log_a1 ~ normal(-2, .2);
  log_a2 ~ normal(-2, .2);
  b1 ~ normal(150, 7);
  b2 ~ normal(220, 7);
  mu = log1m_exp(effort * log1m_inv_logit(logit_prob_detect)) + 
  log_inv_logit(-exp(log_a1) * (jj_date - b1)) + 
  log_inv_logit( exp(log_a2) * (jj_date - b2));
  
  y ~ bernoulli(exp(mu));
}

