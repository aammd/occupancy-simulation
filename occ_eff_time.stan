data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
  vector[N] effort;
  vector[N] jj_date;
}

// The parameters accepted by the model. Our model
// accepts five parameters.
parameters {
  real<lower=0, upper=1> prob_detect;
  real<lower=0, upper=100> a1;
  real<lower=0, upper=100> a2;
  real<lower=1, upper=365> b1;
  real<lower=1, upper=365> b2;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  prob_detect ~ beta(2, 5);
  a1 ~ uniform(0, 100);
  a2 ~ uniform(0, 100);
  b1 ~ uniform(1, 365);
  b2 ~ uniform(1, 365);
  y ~ bernoulli((1 - (1 - prob_detect)^effort) * ((1 / (1 + exp(-a1*(jj_date - b1)))) * (1 / (1 + exp(a2*(jj_date - b2))))));
}

