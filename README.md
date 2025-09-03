REBL Scale
================
Dr. Trisha Shrum, Chris Donovan

September 03, 2025

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tshrum/rebl/graph/badge.svg)](https://app.codecov.io/gh/tshrum/rebl)
<!-- badges: end -->

## Introduction

This is the repository for the Repeated Environmental Behavior Latent
(REBL) Scale project. It contains all the code to build the REBL scale
from raw data through visualizations.

Clean .rds files including REBL scores can be found in the
`2_clean/clean_dataframes/` folder. Clean .csv files are in the
`2_clean/clean_dataframes/csv/` folder.

## Dependencies

The project is set up with `renv`. To restore the packages used in the
project, use:

    # install.packages('renv')
    renv::restore()

This will install packages and versions as described in the `renv.lock`
file.

## Navigating the Project

After opening `reb_scale.Rproj`, start with `table_of_contents.R`. This
will list all the relevant scripts in the order they should be run. Run
the `housekeeping.R` script, which will load helper functions that are
used throughout the project. Additional required packages are then
included in the top of each script. Using `ctrl/cmd + left click` on the
text of a file path in the table of contents will open that script in a
new tab.

All the objects and packages required for each script are loaded at the
top of the page, and all important outputs are saved at the end of each
script. So, you can run the entire project from the table of contents,
but you can also just skip to any script and run it from there.

## Project File Structure

- `1_raw/` is for raw, untouched, datasets. Metadata is embedded into
  these files using \# as a comment symbol.
- `2_clean/` is for cleaned (wrangled, recoded, imputed, etc.) data.
- `3_functions/` is where custom functions are stored. The housekeeping
  script will load these.
- `4_scripts/` is for all R scripts.
- `5_objects/` is for intermediate .rds objects.
- `6_outputs/` is where tabular outputs are kept.
- `7_plots/` is for figures, diagrams, pngs, pdfs, etc.

## REBL Package

The project is halfway through transitioning into a package
(2025-09-03). The package has the usual structure:

- `R/`: Functions
- `data/`: Built-in datasets. Mostly used as an example and for testing.
- `man/`: Documentation
- `tests/`: Test suite using `testthat`
- `DESCRIPTION`: Package information, license, author
- `NAMESPACE`: Exports and imports

In the meantime, functions will be duplicated across `3_functions/` and
`R/` until things are settled, at which point I hope to clear this up
and create a more focused, functional, and portable workflow.
