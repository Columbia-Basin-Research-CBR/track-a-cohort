# [Track-a-Cohort](https://www.cbr.washington.edu/sacramento/cohort/index.html#juvindelta)

*product in development – please note the plots are pending new data inputs for WY2025*

A webpage that includes requested plots for winter run chinook salmon and steelhead in Sacramento River.

To run the complete quarto webpage or individual R scripts, first you must install the dependencies and update the data supporting the R scripts. To do so, follow the instructions below.

``` r
# Install dependencies
source(file.path(getwd(), "R/load_dependencies.R"))

# Update data
source(file.path(getwd(), "R/update_data.R"))
```

Once dependencies are installed and data is updated, you can run the complete quarto webpage by running the following code `make` in the terminal, or clicking `Build` in the panel tabs within your R environment. For individual plots, select individual R scripts within R/ folder or run the following code in the terminal `Rscript R/SCRIPT_NAME.R`.
