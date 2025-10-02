# [Track-a-Cohort](https://www.cbr.washington.edu/sacramento/cohort/index.html#juvindelta)

A webpage that includes requested plots for winter run Chinook Salmon and Steelhead in Sacramento River.

To run the complete quarto webpage or individual R scripts, first you must install the dependencies and update the data supporting the R scripts. To do so, follow the instructions below.

``` r
# Install dependencies
source(file.path(getwd(), "R/load_dependencies.R"))

# Update data
source(file.path(getwd(), "R/update_data.R"))
```

Once dependencies are installed and data is updated, you can run the complete quarto webpage by running the following code `make` in the terminal, or clicking `Build` in the panel tabs within your R environment. For individual plots, select individual R scripts within R/ folder or run the following code in the terminal `Rscript R/SCRIPT_NAME.R`.


**Repo structure overview:** Each R script within the R/ folder is commented using Roxygen notation and the packages needed to run. The `R/` folder contains scripts and helper function that are needed to create the final plots. For data generation used in plots see the `data-raw/ `folder. Each build is set to update the `data-raw/` folder and output the data set into the `data/` folder that is later called within the `R/` folder plots and functions. 

**Plots not updating** If plots are not updating as expected, within your local R environment, run `Build` in the panel tabs or run `make` in the terminal and see console output to determine which file is causing an error. If that does not work, try running `source(file.path(getwd(), "R/update_data.R"))` to see if a certain dataset is not updating properly. Once determined, see specific file, or any downstream files to fix any errors.

``` r
source(file.path(getwd(), "R/update_data.R"))
```