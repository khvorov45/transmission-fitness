// The modifications are:
//   variable names
//     r_hat to recipient_observed
//     d_hat to donor_observed
//     theta to relative_fitness
//     num_obs to n
//   sigma_pyro and sigma_tau fixed to 0.05 and 0.1

functions {
  /*
  Return the model prediction for the recipient given a donor proportion and the
  model parameter relative_fitness.
  */
  real f(real relative_fitness, real d) {
    return d / (d + (1 - d) * exp(relative_fitness));
  }

  /*
  Return a clipped x into [a, b]
  */
  real clipper(real x, real a, real b) {
    if (x > b) {
      return b;
    } else if (x < a) {
      return a;
    } else {
      return x;
    }
  }
}

data {
  int<lower=1> n;
  real<lower=0,upper=1> donor_observed[n];
  real<lower=0,upper=1> recipient_observed[n];
}

transformed data {

}

parameters {
  real relative_fitness;
  real<lower=0,upper=1> d[n];
  real tau[n];
}

transformed parameters {
  real<lower=0,upper=1> r[n];
  for (ix in 1:n)
    r[ix] = clipper(f(relative_fitness, d[ix]) + tau[ix], 0, 1);
}

model {
  // Prior distribution
  target += normal_lpdf(relative_fitness | 0, 1);
  for (ix in 1:n) {
    target += uniform_lpdf(d[ix] | 0, 1);
    target += normal_lpdf(tau[ix] | 0, 0.1);
    if (donor_observed[ix] == 0) {
      target += normal_lcdf(0 | d[ix], 0.05);
    } else if (donor_observed[ix] == 1) {
      target += normal_lccdf(1 | d[ix], 0.05);
    } else {
      target += normal_lpdf(donor_observed[ix] | d[ix], 0.05);
    }
    if (recipient_observed[ix] == 0) {
      target += normal_lcdf(0 | r[ix], 0.05);
    } else if (recipient_observed[ix] == 1) {
      target += normal_lccdf(1 | r[ix], 0.05);
    } else {
      target += normal_lpdf(recipient_observed[ix] | r[ix], 0.05);
    }
  }
}

generated quantities {

}
