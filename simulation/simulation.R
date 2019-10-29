# Simulated data
# Arseniy Khvorov
# Created 2019/10/22
# Last edit 2019/10/28

suppressPackageStartupMessages(library(rstan))
suppressPackageStartupMessages(library(dplyr))
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
  if (mean > 1 || mean < 0) rlang::abort("mean out of bounds")
  alpha <- mean * kappa
  beta <- (1 - mean) * kappa
  rbeta(n, alpha, beta)
}

# Simulates one dataset
simulate <- function(nsam,
                     relative_fitness, kappa,
                     measure_sd = 0,
                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  take_measurement <- function(vec) {
    truncnorm::rtruncnorm(n(), 0, 1, vec, measure_sd)
  }

  tibble(.rows = nsam) %>%
    mutate(
      donor = runif(nsam, 0, 1),
      recipient_expected = donor /
        (donor + (1 - donor) * exp(relative_fitness)),
      recipient_generated = rbeta_proportion(n(), recipient_expected, kappa),
      recipient_measured = take_measurement(recipient_generated),
      donor_measured = take_measurement(donor)
    )
}

# Simulate a profile according to a dictionary
simulate_profile <- function(data_name, data_dict, seed = NULL) {
  pars <- data_dict %>% filter(name == data_name) %>% select(-name)
  if (nrow(pars) == 0)
    rlang::abort(paste0("no name ", data_name, " in data_dict"))
  do.call(simulate, c(pars, seed = seed))
}

# Fits a model to a simulated dataset
fit_stan_model <- function(model_compiled,
                           simulated_data,
                           seed = sample.int(.Machine$integer.max, 1)) {
  # This will still return non-NULL when it fails
  sampling(
      model_compiled,
      data = list(
        n = nrow(simulated_data),
        donor_observed = simulated_data$donor_measured,
        recipient_observed = simulated_data$recipient_measured
      ),
      chains = 4,
      iter = 2000,
      cores = 4,
      open_progress = FALSE,
      seed = seed
  )
}

summ_stan_fit <- function(stan_fit,
                          model_name = NULL, data_name = NULL,
                          data_dict = NULL,
                          index = NULL, seed = NULL) {
  # Check for empty fit
  pars <- NULL
  try(
    expr = pars <- names(stan_fit)[!grepl("\\[", names(stan_fit))],
    silent = TRUE
  )
  if (is.null(pars)) return(NULL)
  pars <- pars[pars != "lp__"]
  summ <- summary(stan_fit, pars = pars)
  summ <- summ$summary
  summ <- summ %>%
    as_tibble() %>%
    mutate(
      term = rownames(summ), model = model_name, data = data_name,
      index = index, seed = seed
    )
  if (!is.null(data_dict)) {
    if (is.null(data_name)) rlang::abort("no data_name to go with data_dict")
    true_vals <- data_dict %>%
      filter(name == data_name) %>%
      select(-name) %>%
      tidyr::pivot_longer(
        everything(), names_to = "term", values_to = "true_value"
      )
    summ <- left_join(summ, true_vals, by = "term")
  }
  summ
}

# Simulates one dataset and fits the model to it
fit_one_sim <- function(data_name, data_dict,
                        model_compiled, model_name = NULL,
                        index = NULL, init_seed = NULL) {
  if (!is.null(index) && !is.null(init_seed)) seed <- init_seed + index
  else seed <- sample.int(.Machine$integer.max, 1)
  simulated_data <- simulate_profile(data_name, data_dict, seed = seed)
  stan_fit <- fit_stan_model(model_compiled, simulated_data, seed)
  summ_stan_fit(stan_fit, model_name, data_name, data_dict, index, seed)
}

fit_many_sims <- function(nsim, data_name, data_dict,
                          model_compiled,
                          model_name = NULL, init_seed = NULL) {
  future_map_dfr(
    1:nsim,
    function(index) fit_one_sim(
      data_name = data_name, data_dict = data_dict,
      model_compiled = model_compiled, model_name = model_name, index = index,
      init_seed = init_seed
    )
  )
}

verify_model <- function(model_name, data_name, data_dict,
                         nsim,
                         model_folder, simulation_folder,
                         model_compiled = NULL,
                         write_results = TRUE,
                         verbose = TRUE) {
  if (is.null(model_compiled)) {
    if (verbose) message(paste0("compiling model ", model_name))
    model_compiled <- stan_model(file.path(
      model_folder, paste0(model_name, ".stan")
    ))
  }
  if (verbose) message(paste0("fitting ", model_name, " to ", data_name))
  res <- fit_many_sims(
    nsim, data_name, data_dict,
    model_compiled, model_name ,
    init_seed = 20191024
  )
  csv_name <- paste0(model_name, "--", data_name, "--", nsim, "sims.csv")
  if (write_results) {
    if (verbose) message(paste0("writing ", csv_name))
    write_csv(res, file.path(simulation_folder, csv_name))
    return(invisible(res))
  }
  res
}

# Save an example to the data folder
write_csv(
  simulate(nsam = 1e3, relative_fitness = 1, kappa = 5, seed = 20191024) %>%
    select(recipient = recipient_measured, donor = donor_measured) %>%
    mutate(relative_fitness = 1, kappa = 5, seed = 20191024),
  file.path(data_folder, "simulated--no-meas-error.csv")
)

# Parameters for different data types

data_dict <- tribble(
  ~name, ~nsam, ~relative_fitness, ~kappa, ~measure_sd,
  "no-meas-error", 1e3, 1, 5, 0,
  "meas-error", 1e3, 1, 5, 0.02,
  "meas-error-neg", 1e3, -1, 5, 0.02,
  "likereal", 8, 1, 5, 0.02
)

# Simulating one of the data profiles and fitting one of the models to it

nsim <- 1000

verify_model(
  model_name = "no-meas-error", data_name = "likereal",
  data_dict = data_dict,
  nsim = nsim, model_folder = model_folder,
  simulation_folder = simulation_folder,
  write_results = TRUE
)
