REBL Scale
================
Dr. Trisha Shrum, Chris Donovan

September 08, 2025

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tshrum/rebl/graph/badge.svg)](https://app.codecov.io/gh/tshrum/rebl)
<!-- badges: end -->

## Introduction

This is the repository for the Repeated Environmental Behavior Latent
(REBL) Scale project. It contains all the code to build the REBL scale
from raw data through visualizations.

## REBL Package

### Installation

To install the `rebl` package:

``` r
if (!require('remotes')) install.packages('remotes')
remotes::install_github('tshrum/rebl')
```

### Using the Package

#### Recoding Survey

``` r
library(rebl)

# Example data frame
str(raw_example)

# Identify REBL items with regex. This just produces a character vector with all
# the item names. It is not necessary if you already have this handy. Here is
# an example with the food items:
food_items <- id_rebl_items(raw_example, '^food', perl = TRUE)

# Here we use regex to take everything that does not start with 'res', because
# 'respondent_id' is our only non-REBL column. If items all start with 'rebl',
# for example, pattern would just be '^rebl*'
rebl_items <- id_rebl_items(raw_example, '^(?!res).*', perl = TRUE)

# Check function documentation for reference
?id_rebl_items

# Recode REBL items from Yes/No to 1/0
df <- recode_rebl(raw_example, rebl_items)

# To reverse code REBL items (where 0 is the PEB), first identify them.
# Just using all food items as an example
reversed_items <- id_reversed_rebl_items(rebl_items, '^food')

# Now recode the reversed items in the df
df <- reverse_code_rebl_items(df, reversed_items)
```

#### Getting REBL Scores

``` r
# Run Rasch Model
model <- get_rasch_model(df, 'respondent_id', rebl_items)

# Throws warning if some items are not found in df
model2 <- get_rasch_model(df, 'respondent_id', c(rebl_items, 'ffff', 'zzzz'))

# Throws error if no items are found in df
model3 <- get_rasch_model(df, 'respondent_id', c('ffff', 'zzzz'))

# Get a df of REBL scores, including person fit statistics
rebl_scores <- get_rebl_scores(model)
str(rebl_scores)

# Only REBL scores, no fits
rebl_only <- get_rebl_scores(model, include_fits = FALSE)
str(rebl_only)
```

### File Structure

The project is halfway through transitioning into a package
(2025-09-03). The package has the usual structure:

- `R/`: Functions
- `data/`: Built-in datasets. Used in examples and testing.
- `data-raw/`: Scripts to create built-in datasets.
- `man/`: Documentation
- `tests/`: Test suite using `testthat`
- `DESCRIPTION`: Package information, license, author
- `NAMESPACE`: Exports and imports

In the meantime, functions will be duplicated across `3_functions/` and
`R/` until things are settled, at which point I hope to clear this up
and create a more focused, functional, and portable workflow.

## REBL Project

### Clone Repository

### Dependencies

The project is set up with `renv`. To restore the packages used in the
project, use:

    # install.packages('renv')
    renv::restore()

This will install packages and versions as described in the `renv.lock`
file.

### Navigating the Project

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

### Project File Structure

- `1_raw/` is for raw, untouched, datasets. Metadata is embedded into
  these files using \# as a comment symbol.
- `2_clean/` is for cleaned (wrangled, recoded, imputed, etc.) data.
- `3_functions/` is where custom functions are stored. The housekeeping
  script will load these.
- `4_scripts/` is for all R scripts.
- `5_objects/` is for intermediate .rds objects.
- `6_outputs/` is where tabular outputs are kept.
- `7_plots/` is for figures, diagrams, pngs, pdfs, etc.
