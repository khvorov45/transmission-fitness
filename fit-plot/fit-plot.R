# Plot results
# Arseniy Khvorov
# Created 2019/10/28
# Last edit 2019/10/28

library(tools)
library(readr)
library(ggplot2)
library(purrr)
library(ggdark) # devtools::install_github("khvorov45/ggdark")

fit_folder <- "fit"
fit_plot_folder <- "fit-plot"
data_folder <- "data"

calculate_fit <- function(samples, npoints = 101) {
  total_iter <- nrow(samples)
  samples %>%
    slice(rep(1:n(), each = npoints)) %>%
    mutate(
      donor = rep(seq(0, 1, length.out = npoints), times = total_iter),
      fitted_recipient = donor / (donor + (1 - donor) * exp(relative_fitness))
    ) %>%
    group_by(donor) %>%
    summarise(
      fit_med = median(fitted_recipient),
      fit_lb = quantile(fitted_recipient, 0.025),
      fit_ub = quantile(fitted_recipient, 0.975)
    )
}

plot_curve <- function(fitted_vals) {
  ggplot(fitted_vals, aes(donor, fit_med)) +
    dark_theme_bw(verbose = FALSE) +
    theme(
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    geom_ribbon(aes(ymin = fit_lb, ymax = fit_ub), alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, lty = "11") +
    geom_line()
}

plot_curve(fitted_vals_list[[1]])

samples_files <- list_files_with_exts(fit_folder, "csv")
samples_list <- map(samples_files, ~ read_csv(.x, col_types = cols()))
names(samples_list) <- samples_files

data_list <- list_files_with_exts(data_folder, "csv") %>%
  map(~ read_csv(.x, col_types = cols()))

fitted_vals_list <- map(samples_list, calculate_fit)

plot_list <- map2(fitted_vals_list, data_list, plot_curve)

