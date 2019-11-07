# Fit model to data
# Arseniy Khvorov
# Created 2019/10/24
# Last edit 2019/11/07

library(tools)
suppressPackageStartupMessages(library(rstan))
library(readr)
suppressPackageStartupMessages(library(dplyr))
library(purrr)

fit_folder <- "fit"
data_folder <- "data"
model_folder <- "model"

# Functions ===================================================================

fit_stan_one <- function(data, model_compiled) {
  sampling(
    model_compiled,
    data = list(
      n = nrow(data),
      donor_observed = data$donor,
      recipient_observed = data$recipient
    ),
    chains = 4,
    iter = 40000,
    cores = 4,
    open_progress = FALSE,
    seed = 20191028
  )
}

extract_samples_one <- function(fit, data_name) {
  nchain <- length(get_inits(fit))
  pars <- names(fit)[!grepl("\\[", names(fit))]
  pars <- pars[pars != "lp__"]
  as.data.frame(fit, pars) %>%
    as_tibble() %>%
    mutate(chain = rep(1:nchain, each = n() / nchain), data = data_name)
}

extract_samples_model <- function(fit_list_model, model_name) {
  imap_dfr(fit_list_model, extract_samples_one)
}

fit_stan_model <- function(model_compiled, data_list) {
  map(data_list, fit_stan_one, model_compiled)
}

save_results_model <- function(samples_list_model, model_name) {
  iwalk(
    samples_list_model,
    ~ write_csv(.x, file.path(fit_folder, paste0(.y, "--", model_name, ".csv")))
  )
}

# Fit script ==================================================================

# Models
alex_model <- stan_model(file.path(model_folder, "alex-modified.stan"))
me_model <- stan_model(file.path(model_folder, "mccaw-meas-error.stan"))
model_list <- list("mccaw-meas-error" = me_model, "alex" = alex_model)

# Data
data <- read_csv(file.path(data_folder, "alex-data.csv"), col_types = cols())
data_list <- group_split(data, mutation)
names(data_list) <- group_keys(data, mutation)$mutation

# Fit all models to all data (subsets)
fit_list <- map(model_list, fit_stan_model, data_list)

# Extract and save samples
samples_list <- map(fit_list, extract_samples_model)
iwalk(
  samples_list,
  ~ write_csv(.x, file.path(fit_folder, paste0(.y, ".csv")))
)

