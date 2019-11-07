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
library(rlang)

data_folder <- "data"
data_plot_folder <- "data-plot"

# Functions ===================================================================

plot_data <- function(data, facet_col) {
  data %>%
    ggplot(aes(donor, recipient)) +
    dark_theme_bw(verbose = FALSE) +
    theme(
      #panel.spacing = unit(0, "null"),
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    xlab("Donor proportion") +
    ylab("Recipient proportion") +
    scale_x_continuous(
      expand = c(0, 0.02), labels = function(breaks) as.character(breaks)
    ) +
    scale_y_continuous(
      expand = c(0, 0.02), labels = function(breaks) as.character(breaks)
    ) +
    facet_wrap(vars(!!sym(facet_col))) +
    geom_point(shape = 18) +
    geom_abline(intercept = 0, slope = 1, lty = "33")
}

save_plot <- function(plot, name, folder, ...) {
  ggsave_dark(
    file.path(folder, paste0(name, ".pdf")), plot,
    units = "cm", device = "pdf", ...
  )
}

# Script ======================================================================

data <- read_csv(file.path(data_folder, "alex-data.csv"), col_types = cols())

data_plot <- plot_data(data, "mutation")

save_plot(data_plot, "alex-data", data_plot_folder, width = 10, height = 7.5)
