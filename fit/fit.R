# Fit model to data
# Arseniy Khvorov
# Created 2019/10/24
# Last edit 2019/10/28

library(tools)
suppressPackageStartupMessages(library(rstan))
library(readr)
suppressPackageStartupMessages(library(dplyr))
library(purrr)

fit_folder <- "fit"
data_folder <- "data"
model_folder <- "model"

fit_stan_real <- function(data, model_compiled) {
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

extract_samples <- function(fit) {
  nchain <- length(get_inits(fit))
  pars <- names(fit)[!grepl("\\[", names(fit))]
  pars <- pars[pars != "lp__"]
  as.data.frame(fit, pars) %>%
    as_tibble() %>%
    mutate(chain = rep(1:nchain, each = n() / nchain))
}

nme_model <- stan_model(file.path(model_folder, "no-meas-error.stan"))

data_files <- list_files_with_exts(data_folder, "csv")
data_list <- map(data_files, ~read_csv(.x, col_types = cols()))
names(data_list) <- gsub(".csv", "", basename(data_files))

fit_list <- map(data_list, fit_stan_real, nme_model)

samples_list <- map(fit_list, extract_samples)

iwalk(samples_list, ~ write_csv(.x, file.path(fit_folder, paste0(.y, ".csv"))))
