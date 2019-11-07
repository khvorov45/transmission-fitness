// Measurement error model
// Arseniy Khvorov
// Created 2019/10/30
// Last edit 2019/10/30

data {
  int<lower=1> n;
  real<lower=0,upper=1> donor_observed[n];
  real<lower=0,upper=1> recipient_observed[n];
}

parameters {
  real relative_fitness;
  real<lower=0> kappa;
  real<lower=0,upper=1> donor_true[n];
  real<lower=0,upper=1> recipient_true[n];
}

transformed parameters {
  real<lower=0,upper=1> recipient_expected[n];
  for (i in 1:n) {
    recipient_expected[i] = donor_true[i] /
      (donor_true[i] + (1 - donor_true[i]) * exp(relative_fitness));
  }
}

model {

  // Prior for parameters of interest
  relative_fitness ~ normal(0, 3);
  kappa ~ exponential(0.1);

  // Measurement error
  donor_true ~ uniform(0, 1);
  recipient_true ~ uniform(0, 1);
  donor_observed ~ normal(donor_true, 0.05);
  recipient_observed ~ normal(recipient_true, 0.05);

  // Likelihood
  recipient_true ~ beta_proportion(recipient_expected, kappa);
}
