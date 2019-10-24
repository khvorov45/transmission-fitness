# Summarise simulation results
# Arseniy Khvorov
# Created 2019/10/24
# Last edit 2019/10/24

library(readr)
library(stringr)
library(purrr)
library(tools)

simulation_folder <- "simulation"

read_results <- function(filepath) {
  if (str_detect(filepath, "summary")) return(NULL)
  read_csv(filepath, col_types = cols()) %>%
    mutate(filename = str_replace(basename(filepath), ".csv", ""))
}

sim_results <- list_files_with_exts(simulation_folder, "csv") %>%
  map_dfr(read_results)

sim_summary <- sim_results %>%
  group_by(term, model, data, true_value) %>%
  summarise(
    median_mean = mean(`50%`),
    median_sd = sd(`50%`),
    sd_mean = mean(sd),
    nsim = max(index)
  )

write_csv(sim_summary, file.path(simulation_folder, "summary.csv"))
