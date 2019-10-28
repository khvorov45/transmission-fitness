# Testing simulated data
# Arseniy Khvorov
# Created 2019/10/28
# Last edit 2019/10/28

# Not meant to be run by itself

# beta reparametarisation
set.seed(1)
beta_repar <- rbeta_proportion(100, 0.6, 3)
set.seed(1)
beta_reg <- rbeta(100, 0.6 * 3, (1 - 0.6) * 3)
testthat::expect_equal(beta_repar, beta_reg)

# Simulation
pop <- simulate(100, 1, 3) # No measurement error
pop_er <- simulate(100, 1, 3, 0.02)
pop_likereal <- simulate_profile("likereal", data_dict)

# Models
nme <- stan_model(file.path(model_folder, "no-meas-error.stan"))
alex <- stan_model(file.path(model_folder, "alex-modified.stan"))

# Fit one
fit_nme <- fit_stan_model(nme, pop)
fit_alex <- fit_stan_model(alex, pop)

# Summarise one
summ_nme <- summ_stan_fit(fit_nme)
summ_alex <- summ_stan_fit(fit_alex)

# Simulating, fitting and summarising
fit_one_sim("likereal", data_dict, alex)
