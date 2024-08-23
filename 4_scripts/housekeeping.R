# Install pacman if not already installed
suppressPackageStartupMessages(
  if (!requireNamespace('pacman')) {
    install.packages('pacman', dependencies = TRUE, quiet = TRUE)
  }
)

# Load essential packages
pacman::p_load(
  dplyr,
  conflicted
)

# Set dplyr as winner for common conflicts
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::summarize(),
  dplyr::all_of(),
  dplyr::any_of(),
  .quiet = TRUE
)

# Set options for Tibbles
options(tibble.print_min = Inf,
        max.print = 10000)

# Source basic custom functions
invisible(
  lapply(
    list.files(path = '3_functions/helper_functions', full.names = TRUE),
    source
  )
)
