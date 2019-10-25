// Measurement error model
// Arseniy Khvorov
// Created 2019/10/25
// Last edit 2019/10/25

functions {
  // Stan's beta distribution definition sets density to 0 at 1 and 0
  // and disallows 1 and 0 as parameters.
  real[] remove_proportion_bounds(real[] vec) {
    int n = size(vec);
    real out[n];
    for (i in 1:size(vec)) {
      if (vec[i] == 1) out[i] = 0.99;
      else if (vec[i] == 0) out[i] = 0.01;
      else out[i] = vec[i];
    }
    return out;
  }
}

data {
  int<lower=1> n;
  real<lower=0,upper=1> donor_observed[n];
  real<lower=0,upper=1> recipient_observed[n];
}

transformed data {
  real<lower=0,upper=1> donor_modified[n];
  real<lower=0,upper=1> recipient_modified[n];
  donor_modified = remove_proportion_bounds(donor_observed);
  recipient_modified = remove_proportion_bounds(recipient_observed);
}

parameters {
  real relative_fitness;
  real<lower=0> kappa;
  //real donor_imputed[n];
  real recipient_imputed[n];
}

transformed parameters {
  real<lower=0,upper=1> recipient_expected[n];
  for (i in 1:n) {
    recipient_expected[i] = donor_modified[i] /
      (donor_modified[i] + (1 - donor_modified[i]) * exp(relative_fitness));
  }
}

model {
  relative_fitness ~ normal(0, 3);
  kappa ~ exponential(0.1);
  for (i in 1:n) {
    //recipient_imputed[i] ~ normal(0, 1);
    if (recipient_observed[i] <= 1e-4)
      recipient_imputed[i] ~ uniform(1e-6, 0.05);
    else if (fabs(recipient_observed[i] - 1) <= 1e-4)
      recipient_imputed[i] ~ uniform(0.95, 1 - 1e-6);
    else
      recipient_imputed[i] ~
        normal(recipient_observed[i], 0.02) T[1e-6, 1 - 1e-6];
    recipient_imputed[i] ~ beta_proportion(recipient_expected[i], kappa);
  }
}
