# Plot results
# Arseniy Khvorov
# Created 2019/10/28
# Last edit 2019/10/29

library(tools)
library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(purrr))
library(ggdark) # devtools::install_github("khvorov45/ggdark")

fit_folder <- "fit"
fit_plot_folder <- "fit-plot"
data_folder <- "data"

# Functions ===================================================================

read_samples <- function(filepath) {
  read_csv(filepath, col_types = cols()) %>%
    mutate(model = gsub(".csv", "", basename(filepath)))
}

fit <- function(d, s) d / (d + (1 - d) * exp(s))

calc_prior <- function(summ, qprior) {
  summ %>%
    mutate(
      prior_med = fit(donor, qprior(0.5)),
      prior_lb = fit(donor, qprior(0.025)),
      prior_ub = fit(donor, qprior(0.975))
    )
}

calculate_fit <- function(samples, npoints = 101) {
  total_iter <- nrow(samples)
  samples %>%
    slice(rep(1:n(), each = npoints)) %>%
    mutate(
      donor = rep(seq(0, 1, length.out = npoints), times = total_iter),
      fitted_recipient = fit(donor, relative_fitness)
    ) %>%
    group_by(donor, data, model) %>%
    summarise(
      fit_med = median(fitted_recipient),
      fit_lb = quantile(fitted_recipient, 0.025),
      fit_ub = quantile(fitted_recipient, 0.975)
    ) %>%
    ungroup()
}

plot_curve <- function(fitted_vals, data_points) {
  prior_line <- function(mapping) geom_line(mapping, lty = "33")
  labeller <- function(breaks) as.character(breaks)
  ggplot(fitted_vals, aes(donor, fit_med)) +
    dark_theme_bw(verbose = FALSE) +
    theme(
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0, "null")
    ) +
    facet_grid(model ~ data) +
    scale_x_continuous(
      "Donor proportion", breaks = seq(0, 1, 0.1), labels = labeller
    ) +
    scale_y_continuous(
      "Recipient proportion", breaks = seq(0, 1, 0.1), labels = labeller
    ) +
    prior_line(aes(y = prior_lb)) +
    prior_line(aes(y = prior_ub)) +
    geom_point(data = data_points, aes(donor, recipient), shape = 18) +
    geom_ribbon(aes(ymin = fit_lb, ymax = fit_ub), alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, lty = "11") +
    geom_line()
}

save_plot <- function(pl, name, fit_plot_folder) {
  ggsave_dark(
    file.path(fit_plot_folder, paste0(name, ".pdf")),
    pl, dark = FALSE,
    width = 12, height = 12, units = "cm", device = "pdf"
  )
}

# Plot script =================================================================

samples_files <- list_files_with_exts(fit_folder, "csv")
samples <- map_dfr(samples_files, read_samples)

data <- read_csv(file.path(data_folder, "alex-data.csv"), col_types = cols())

fitted_vals <- calculate_fit(samples)

# Add prior distribution
prior_dict = list(
  alex = function(q) qnorm(q, 0, 1),
  "mccaw-meas-error" = function(q) qnorm(q, 0, 3)
)
fitted_vals <- fitted_vals %>%
  group_split(model) %>%
  map2_dfr(prior_dict, calc_prior)

pl <- plot_curve(fitted_vals, data = mutate(data, data = mutation))
save_plot(pl, "fit-plot", fit_plot_folder)
