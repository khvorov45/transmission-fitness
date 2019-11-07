# Diagnostic plots
# Arseniy Khvorov
# Created 2019/10/29
# Last edit 2019/10/29

library(tools)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyr))
library(ggdark) # devtools::install_github("khvorov45/ggdark")

fit_folder <- "fit"
fit_diag_folder <- "fit-diagnostic"
data_folder <- "data"

# Functions ===================================================================

lengthen_samples <- function(samples) {
  samples %>%
    group_by(chain, data) %>%
    mutate(iter = row_number()) %>%
    ungroup() %>%
    pivot_longer(
      cols = c(-chain, -iter, -data), names_to = "term", values_to = "sample"
    )
}

additional_theme <- function() {
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "lines"),
    panel.spacing = unit(0, "lines")
  )
}

plot_trace_one <- function(samples_long) {
  samples_long %>%
    ggplot(aes(sample, iter, col = as.factor(chain))) +
    dark_theme_bw(verbose = FALSE) +
    additional_theme() +
    theme(strip.text = element_blank()) +
    scale_y_continuous("Iteration") +
    scale_x_continuous("Value") +
    geom_path(alpha = 0.5) +
    facet_wrap(~term, scales = "free_x", nrow = 1)
}

plot_trace_model <- function(samples_model) {
  samples_split <- samples_model %>% lengthen_samples() %>% group_split(data)
  names(samples_split) <- group_keys(samples_model, data)$data
  map(samples_split, plot_trace_one)
}

plot_density_one <- function(long_samples, prior_dict) {
  priors <- list()
  for (prior_name in names(prior_dict)) {
    priors[[prior_name]] <- stat_function(
      data = filter(long_samples, term == prior_name),
      fun = prior_dict[[prior_name]], lty = "33", inherit.aes = FALSE
    )
  }
  long_samples %>%
    ggplot(aes(sample, col = as.factor(chain))) +
    dark_theme_bw(verbose = FALSE) +
    additional_theme() +
    theme(axis.ticks.length.x = unit(0, "null")) +
    scale_y_continuous("Density") +
    scale_x_continuous("Value") +
    facet_wrap(~term, scales = "free_x", nrow = 1) +
    priors +
    geom_freqpoly(alpha = 0.5, stat = "density")
}

plot_density_model <- function(samples_model, prior_dict) {
  samples_split <- samples_model %>% lengthen_samples() %>% group_split(data)
  names(samples_split) <- group_keys(samples_model, data)$data
  map(samples_split, plot_density_one, prior_dict)
}

arrange_itdens_one <- function(dens, iter) {
  ggarrange(
    dens + rremove("x.axis") + rremove("xlab") +
      rremove("x.text") + rremove("x.ticks"),
    iter, ncol = 1, align = "v"
  )
}

arrange_itdens_model <- function(dens_model, iter_model) {
  map2(dens_model, iter_model, arrange_itdens_one)
}

save_itdens_arranged_one <- function(plot, data_name,
                                     model_name, fit_diag_folder) {
  ggsave_dark(
    file.path(fit_diag_folder, paste0(data_name, "--", model_name, ".pdf")),
    plot, dark = FALSE, width = 15, height = 15, units = "cm", device = "pdf"
  )
}

save_itdens_arranged_model <- function(plot_model, model_name,
                                       fit_diag_folder) {
  iwalk(plot_model, save_itdens_arranged_one, model_name, fit_diag_folder)
}

# Plot script =================================================================

samples_files <- list_files_with_exts(fit_folder, "csv")
samples_list <- map(samples_files, ~ read_csv(.x, col_types = cols()))
names(samples_list) <- gsub(".csv", "", basename(samples_files))

iter_list <- map(samples_list, plot_trace_model)

prior_dict <- list(
  "alex" = list(
    relative_fitness = function(x) dnorm(x, mean = 0, sd = 1)
  ),
  "mccaw-meas-error" = list(
    kappa = function(x) dexp(x, rate = 0.1),
    relative_fitness = function(x) dnorm(x, mean = 0, sd = 3)
  )
)

dens_list <- map2(samples_list, prior_dict, plot_density_model)

it_dens_arranged <- map2(dens_list, iter_list, arrange_itdens_model)

iwalk(it_dens_arranged, save_itdens_arranged_model, fit_diag_folder)
