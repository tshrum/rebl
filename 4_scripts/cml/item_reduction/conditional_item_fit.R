#' conditional item fit
#' 2024-04-24

#' First step in reducing the number of REBL items. Running iterations of 
#' `boot_fit()` over surveys 2a and 2b and removing items where the either the
#' in.pkorr or out.pkorr == 0. Running enough iterations that all significantly
#' misfitting items can be removed, or close to it?

#' NOTE: Might make more sense to keep eliminating items until neither infit or
#' outfit are significant at 0.05. As it stands now, it stops when there are 
#' still a couple of significantly misfitting items. Also, need to add a 
#' stopping mechanism so it doesn't keep doing new iterations after it is stable
#' NOTE: Only working with surveys 2a and 2b here for now because 1 is a mess.

#' Inputs:
#'    1. List of cml model outputs for all surveys
#'    2. All clean surveys

#' Outputs:
#'    1. Conditional Item Fit (currently 8x iterations)
#'    2. Reduced set of REBL items to be sent to stepwiseIt LR tests



# Load Data ---------------------------------------------------------------


# Packages
pacman::p_load(dplyr,
               purrr,     # map
               eRm,
               iarm,      # boot_fit
               tidyr,
               tictoc,    # timer
               furrr,     # parallel map
               profvis,   # profiling
               tibble)    

# Load just CML models for just surveys 2a and 2b
cml_models <- readRDS('5_objects/all_rasch_models.rds')[['cml']][2:3]

# Pull in survey 2 again so that we can recombine REBL scores with prolificID
surveys <- readRDS('2_clean/all_surveys_imputed.rds')[2:3]

# REBL item names to pull in the item fit section
all_rebl_item_names <- readRDS('5_objects/all_rebl_item_names.rds')

# Load get_next_cond_fit function fot iterative conditional fits
source('3_functions/get_next_cond_fit.R')
source('3_functions/get_cond_rebl_items.R')

# Initialize list of conditional fit results
cond_fits <- list()



# Conditional Fit ---------------------------------------------------------


#' First we take CML models and do a single boot fit run on each survey. After
#' that, for each iteration we use the `get_next_cond_fit()` function so we
#' can do it a bunch of times.



## Just First Round --------------------------------------------------------


# Set up backend with 1 session for each survey
tic()
config <- furrr_options(globals = 'cml_models',
                        packages = c('purrr', 'iarm'),
                        seed = 42)
plan(multisession, workers = 2)

# First round of conditional fit stats
cond_fits$fit_1 <- future_map(cml_models, ~ {
  boot_fit(.x, 1000)[[1]] %>%
    as.data.frame()
}, .options = config)

plan(sequential)
toc()
# 6.75 minutes



## Iterations --------------------------------------------------------------


# Keep running iterations until there are no in.pkorr or out.pkorr values == 0

# Prep while loop with iteration count, assign previous fit, print time
i <- 1
prev_fit <- cond_fits$fit_1
cat('\nStarting at',
    format(Sys.time(), '%X'))

# Set up parallel backend
tic()
config <- furrr_options(globals = 'cml_models',
                        packages = c('purrr', 'iarm', 'eRm', 'dplyr'),
                        seed = 42)
plan(multisession, workers = 2)

# While loop that continues until there are no in.pkorr or out.pkorr == 0
# Or stops at 10 iterations just in case!
while ((any(unlist(map(prev_fit, ~ .x$in.pkorr == 0))) |
       any(unlist(map(prev_fit, ~ .x$out.pkorr == 0)))) && 
       i <= 10) {
  
  # Get the next conditional fit iteration
  cond_fits[[paste0("fit_", i)]] <- get_next_cond_fit(prev_fit, 
                                                      surveys,
                                                      n_boot = 1000,
                                                      config = config,
                                                      seed = 42)
  
  # Update the previous fit for the next iteration
  prev_fit <- cond_fits[[paste0("fit_", i)]]
  
  # Increase iteration counter
  i <- i + 1
  
  cat('\nIteration', i, 'complete.')
}

plan(sequential)
toc()
# 409 sec (7 minutes)
# 30 minutes for 10 iterations. Note still having seed problems
# Also the while loop isn't stopping properly...



# Explore Conditional Fits ------------------------------------------------


names(cond_fits)
get_str(cond_fits)

# Looks like it stops changing at fit_5
identical(cond_fits$fit_5, cond_fits$fit_6)
# Yep, 5 is the last one

# Keep only the first 5
cond_fits <- cond_fits[1:5]

# Check out fit 5
get_str(cond_fits$fit_5)
# 44 and 49 items

# Look at infit and outfit
map(cond_fits$fit_5, ~ {
  .x %>% 
    select(in.pkorr, out.pkorr, everything()) %>% 
    arrange(in.pkorr)
})
# Still 4 infits at 0



# Remove All Significant Fits ---------------------------------------------


#' After iterations to remove items where in.pkorr or out.pkorr == 0, we will
#' just take one more round to remove anything that is significant in either.
cond_fits$fit_final <- map(cond_fits[[length(cond_fits)]], ~ {
  .x %>% 
    filter(in.pkorr > 0.05 & out.pkorr > 0.05) %>% 
    select(in.pkorr, out.pkorr, everything()) %>% 
    arrange(in.pkorr)
})

map(cond_fits$fit_final, dim)
# Down to 44 and 45

# See what was lost from each survey
map2(cond_fits$fit_5, cond_fits$fit_final, ~ {
  setdiff(row.names(.x), row.names(.y))
})
#' Survey 2a didn't change
#' Survey 2b lost: 
#'  "foodRefrainedDistance_" 
#'  "packTapWaterHome_"
#'  "socialReduceImpact_"   
#'  "transCarpool_"

# Don't love these questions anyway



# Keep only shared items --------------------------------------------------


# Just grab items from final fit that are in both surveys
final_item_fits <- get_cond_rebl_items(survey_2a = cond_fits$fit_final[[1]], 
                                       survey_2b = cond_fits$fit_final[[2]])
get_str(final_item_fits)
final_item_fits

# Just names of final items:
final_item_fits$rebl_item
# 38 items



# Save and Clear ----------------------------------------------------------


# All conditional fit iterations
saveRDS(cond_fits, '5_objects/cml/item_reduction/cond_fit_iters.rds')

# And keepers
saveRDS(final_item_fits$rebl_item,
        '5_objects/cml/item_reduction/rebl_items_cond_fit_38.rds')


clear_data()
