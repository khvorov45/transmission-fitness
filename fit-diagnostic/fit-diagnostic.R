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

lengthen_samples <- function(samples) {
  samples %>%
    group_by(chain) %>%
    mutate(iter = row_number()) %>%
    ungroup() %>%
    pivot_longer(
      cols = c(-chain, -iter), names_to = "term", values_to = "sample"
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

plot_trace <- function(samples) {
  samples %>%
    lengthen_samples() %>%
    ggplot(aes(sample, iter, col = as.factor(chain))) +
    dark_theme_bw(verbose = FALSE) +
    additional_theme() +
    theme(strip.text = element_blank()) +
    scale_y_continuous("Iteration") +
    scale_x_continuous("Value") +
    geom_path(alpha = 0.5) +
    facet_wrap(~term, scales = "free_x", nrow = 1)
}

plot_density <- function(samples, prior_dict) {
  long_samples <- lengthen_samples(samples)
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

arrange_itdens <- function(dens, iter) {
  ggarrange(
    dens + rremove("x.axis") + rremove("xlab") +
      rremove("x.text") + rremove("x.ticks"),
    iter, ncol = 1, align = "v"
  )
}

save_itdens_arranged <- function(plot, name, fit_diag_folder) {
  ggsave_dark(
    file.path(fit_diag_folder, paste0(name, ".pdf")),
    plot, dark = FALSE, width = 15, height = 15, units = "cm", device = "pdf"
  )
}

samples_files <- list_files_with_exts(fit_folder, "csv")
samples_list <- map(samples_files, ~ read_csv(.x, col_types = cols()))
names(samples_list) <- gsub(".csv", "", basename(samples_files))

iter_list <- map(samples_list, plot_trace)

prior_dict <- list(
  kappa = function(x) dexp(x, rate = 0.1),
  relative_fitness = function(x) dnorm(x, mean = 0, sd = 3)
)

dens_list <- map(samples_list, plot_density, prior_dict)

it_dens_arranged <- map2(dens_list, iter_list, arrange_itdens)

iwalk(it_dens_arranged, save_itdens_arranged, fit_diag_folder)
