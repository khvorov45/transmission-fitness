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

calculate_fit <- function(samples, qprior, npoints = 101) {
  total_iter <- nrow(samples)
  fit <- function(d, s) d / (d + (1 - d) * exp(s))
  samples %>%
    slice(rep(1:n(), each = npoints)) %>%
    mutate(
      donor = rep(seq(0, 1, length.out = npoints), times = total_iter),
      fitted_recipient = fit(donor, relative_fitness)
    ) %>%
    group_by(donor) %>%
    summarise(
      fit_med = median(fitted_recipient),
      fit_lb = quantile(fitted_recipient, 0.025),
      fit_ub = quantile(fitted_recipient, 0.975)
    ) %>%
    mutate(
      prior_med = fit(donor, qprior(0.5)),
      prior_lb = fit(donor, qprior(0.025)),
      prior_ub = fit(donor, qprior(0.975))
    )
}

plot_curve <- function(fitted_vals, data) {
  prior_line <- function(mapping) geom_line(mapping, lty = "33")
  ggplot(fitted_vals, aes(donor, fit_med)) +
    dark_theme_bw(verbose = FALSE) +
    theme(
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous("Donor proportion", breaks = seq(0, 1, 0.1)) +
    scale_y_continuous("Recipient proportion", breaks = seq(0, 1, 0.1)) +
    prior_line(aes(y = prior_lb)) +
    prior_line(aes(y = prior_ub)) +
    geom_point(data = data, aes(donor, recipient), shape = 18) +
    geom_ribbon(aes(ymin = fit_lb, ymax = fit_ub), alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, lty = "11") +
    geom_line()
}

save_plot <- function(pl, name, fit_plot_folder) {
  ggsave_dark(
    file.path(fit_plot_folder, paste0(name, ".pdf")),
    pl, dark = FALSE,
    width = 10, height = 10, units = "cm", device = "pdf"
  )
}

samples_files <- list_files_with_exts(fit_folder, "csv")
samples_list <- map(samples_files, ~ read_csv(.x, col_types = cols()))
names(samples_list) <- gsub(".csv", "", basename(samples_files))

data_list <- list_files_with_exts(data_folder, "csv") %>%
  map(~ read_csv(.x, col_types = cols()))

fitted_vals_list <- map(
  samples_list, calculate_fit,
  qprior = function(q) qnorm(q, 0, 3)
)

plot_list <- map2(fitted_vals_list, data_list, plot_curve)

iwalk(plot_list, save_plot, fit_plot_folder)
