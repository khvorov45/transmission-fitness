# Create plots of the data
# Arseniy Khvorov
# Created 2019/10/24
# Last edit 2019/10/24

library(tools)
library(readr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggdark) # devtools::install_github("khvorov45/ggdark")

data_folder <- "data"
data_plot_folder <- "data-plot"

read_data <- function(filepath) {
  filepath %>%
    read_csv(
      col_types = cols(recipient = col_double(), donor = col_double())
    )
}

extract_true_fitness <- function(.tbl) {
  if (!"relative_fitness" %in% names(.tbl)) return(NA_real_)
  unique(.tbl$relative_fitness)
}

plot_data <- function(data) {
  data %>%
    ggplot(aes(donor, recipient)) +
    dark_theme_bw(verbose = FALSE) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    xlab("Donor proportion") +
    ylab("Recipient proportion") +
    scale_x_continuous(expand = c(0, 0.02)) +
    scale_y_continuous(expand = c(0, 0.02)) +
    geom_point(shape = 18) +
    geom_abline(intercept = 0, slope = 1, lty = "33", col = "red")
}

add_true_line <- function(plot, true_rel_fitness) {
  if (is.na(true_rel_fitness)) return(plot)
  mccaw_curve <- function(x, s) x / (x + (1 - x) * exp(s))
  plot +
    stat_function(
      fun = mccaw_curve, args = list(s = true_rel_fitness), lwd = 1,
      col = "blue"
    )
}

save_plots <- function(plots, folder, ...) {
  save_one <- function(plot, name, folder, ...) {
    ggsave_dark(
      file.path(folder, paste0(name, ".pdf")), plot,
      units = "cm", device = "pdf", ...
    )
  }
  iwalk(plots, save_one, folder, ...)
}

data_files <- list_files_with_exts(data_folder, "csv")

data_tibbles <- map(data_files, read_data)
names(data_tibbles) <- basename(data_files) %>% str_replace("\\.csv", "")

true_relative_fitess <- map_dbl(data_tibbles, extract_true_fitness)

data_plots <- map(data_tibbles, plot_data) %>%
  map2(true_relative_fitess, add_true_line)

save_plots(data_plots, folder = data_plot_folder, width = 10, height = 10)
