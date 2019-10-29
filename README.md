# transmission-fitness

The goal of transmission-fitness is to fit the model from [McCaw
(2011)](https://doi.org/10.1371/journal.pcbi.1002026) using the Bayesian
framework using [Stan](https://mc-stan.org/) and
[R](https://www.r-project.org/).

## Usage

Everything can be run interactively (launch the `.Rproj` file in
RStudio). Every script (except those that start with `test-`) 
can also be executed from a terminal as long as
the working directory is that of the `.Rproj` file, for example:

```
cd path/to/transmission-fitness.Rproj
Rscript data-plot/data-plot.R
```

## Directories

- `data` contains `.csv` files that serve as the data to be analysed.
Each `.csv` file contains two columns: `recipient` (outcome) and
`donor` (covariate). Both of these represent the proportion of a
virus (not a percentage). Other columns may be present and will be
ignored.

- `data-plot` contains a script to generate data plots. The script
looks for `.csv` files in the `data` folder.

- `data-raw` contains files that need to be cleaned before moving into
`data`. The script cleans the data in `data-raw` and moves it to
`data` keeping the filenames.

- `fit` fits a model from the `model` folder to the datasets in the
`data` folder.

- `fit-diagnostic` diagnostics of model fits found in `fit`.

- `fit-plot` plots of results. The key is in `key.txt` inside the folder.

- `model` contains Stan model files. `alex_modified` is the same as [`alex-unmodified`](https://github.com/aezarebski/competitive-mixtures/blob/master/src/between-host/mccaw.stan) except that variable names are changed to be the same as they are in `no-meas-error`.

- `paper` contains the paper describing the model.

- `simulation` contains a script that simulates ideal data for the
model and verifies the model by fitting it to the simulated data.
The summary of results is in `summary.csv`. The script also generates
datasets and moves them to `data` under the names of
`simulated-*`.

## Simulation results

When given a large sample with no measurement error, both models (`alex-modified` and `no-meas-error`) produce the correct estimates of relative fitness. `alex-modified` does not estimate variability (in a parameter), `no-meas-error` does.

When given a large sample with measurement error, `alex-modified` remains unbiased but tends to understimate the variance of the estimate (by around 30%). `no-meas-error` is biased (8% towards the null) and slightly underestimates the variance of the estimate.

When given a small sample with measurement error, Alex's model (`alex-modified`) is a bit better at finding the correct point estimate. My model (`no-meas-error`) is a bit better at estimating how much the estimate varies.

I used `no-meas-error` to fit real data.
