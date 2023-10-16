data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
  vector[N] effort;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper=1> prob_pres;
  real<lower=0, upper=1> prob_detect;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  prob_pres ~ beta(2, 2);
  prob_detect ~ beta(2, 5);
  y ~ bernoulli((1 - (1 - prob_detect)^effort)*prob_pres);
}

