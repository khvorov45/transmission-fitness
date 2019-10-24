# Simulated data
# Arseniy Khvorov
# Created 2019/10/22
# Last edit 2019/10/24

library(rstan)
library(dplyr)
library(purrr)
library(future)
library(furrr)
library(readr)

#plan(multiprocess) # Won't work on windows for me

data_folder <- "data"
model_folder <- "model"
simulation_folder <- "simulation"

# Stan-like reparameterisation of the beta distribution
# The larger the kappa, the smaller the variance
rbeta_proportion <- function(n, mean, kappa) {
  if (mean > 1 || mean < 0) abort("mean out of bounds")
  alpha <- mean * kappa
  beta <- (1 - mean) * kappa
  rbeta(n, alpha, beta)
}

# Simulates one dataset
simulate <- function(nsam, relative_fitness, kappa, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  tibble(.rows = nsam) %>%
    mutate(
      donor = runif(nsam, 0, 1),
      recipient_expected = donor /
        (donor + (1 - donor) * exp(relative_fitness)),
      recipient_observed = rbeta_proportion(n(), recipient_expected, kappa),
      relative_fitness = relative_fitness,
      kappa = kappa
    )
}

# Simulates one dataset and fits the model to it
fit_one_sim <- function(index, init_seed, model, ...) {
  seed <- init_seed + index
  simulated_data <- simulate(seed = seed, ...)
  stan_fit <- sampling(
    model,
    data = list(
      n = nrow(simulated_data),
      donor_observed = simulated_data$donor,
      recipient_observed = simulated_data$recipient_observed
    ),
    chains = 4,
    iter = 2000,
    cores = 4,
    open_progress = FALSE,
    seed = seed
  )
  summ <- summary(stan_fit, pars = c("relative_fitness", "kappa"))$summary
  summ %>%
    as_tibble() %>%
    mutate(term = rownames(summ), index = index, seed = seed) %>%
    select(term, everything())
}

fit_many_sims <- function(nsim, init_seed, model, ...) {
  future_map_dfr(1:nsim, fit_one_sim, init_seed, model, ...)
}

# Save an example to the data folder
dat <- simulate(nsam = 1e3, relative_fitness = 1, kappa = 5, seed = 20191024)
write_csv(
  select(dat, recipient = recipient_observed, donor, relative_fitness, kappa),
  file.path(data_folder, "simulated-example1.csv")
)

# Verify the no measurement error model
nsim <- 2
model_filename <- "no-meas-error"
model_compiled <- stan_model(file.path(
  model_folder, paste0(model_filename, ".stan")
))
true_rel_fit <- 1
true_kappa <- 5
data_name <- "no-meas-error"
res <- fit_many_sims(
  nsim, init_seed = 20191024, mod = model_compiled,
  nsam = 1e3, relative_fitness = true_rel_fit, kappa = true_kappa
)
res <- res %>%
  mutate(
    model = model_filename,
    data = data_name,
    true_value = case_when(
      term == "relative_fitness" ~ true_rel_fit,
      term == "kappa" ~ true_kappa
    )
  )
write_csv(
  res,
  file.path(
    simulation_folder,
    paste0(model_filename, "--", data_name, "--", nsim, "sims.csv")
  )
)
