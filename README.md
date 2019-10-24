# transmission-fitness

The goal of transmission-fitness is to fit the model from [McCaw
(2011)](https://doi.org/10.1371/journal.pcbi.1002026) using the Bayesian
framework using [Stan](https://mc-stan.org/) and
[R](https://www.r-project.org/).

## Usage

Everything can be run interactively (launch the `.Rproj` file in
RStudio). Every script can also be executed from a terminal as long as
the working directory is that of the `.Rproj` file, for example:

```
cd path/to/transmission-fitness.Rproj
Rscript model/model.R
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

- `model` contains Stan model files.

- `paper` contains the paper describing the model.

- `simulation` contains a script that simulates ideal data for the
model and verifies the model by fitting it to the simulated data.
The summary of results is in `summary.csv`. The script also generate
datasets and moves them to `data` under the names of
`simulated-example`.
