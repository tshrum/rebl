#' Item reduction helpers
#' 2024-06-14
#' 
#' Just a couple of functions to make item reduction by consistency and stewpise
#' LR tests a little easier, seeing as we are now trying out a bunch of 
#' iterations.



# Packages ----------------------------------------------------------------


pacman::p_load(eRm,
               tictoc,
               purrr,
               assertthat,
               stringr)



# stepwiseIt --------------------------------------------------------------


reduce_by_stepwise_lr <- function(models,
                                  criterion = list('LRtest'),
                                  verbose = TRUE,
                                  maxstep = NA) {
  # If given an individual model, turn it into a list
  if (!is.list(models)) {
    validate_that(any(str_detect(class(models), 'eRm')),
                  msg = 'Input must be either an eRm object of a list of eRm objects')
    models <- list(models)
  }
  
  
  # Now treat as list and apply to all
  tic()
  lr_outputs <- imap(models, ~ {
    cat('\n~~~ Starting', .y, 'at:', format(Sys.time(), '%X'), '~~~\n')
    
    stepwiseIt(object = .x,    
               criterion = criterion,
               verbose = verbose,
               maxstep = maxstep)
  })
  toc()
  
  # Now get shared items
  shared_items <- intersect(colnames(lr_outputs[[1]]$X), 
                            colnames(lr_outputs[[2]]$X))
  
  return(shared_items)
}




# Item Consistency --------------------------------------------------------


reduce_by_consistency <- function(rebl_items,
                                  clean_surveys,
                                  lower_bound = 0.75,
                                  upper_bound = 0.95) {
  
  # Filter down both surveys to only the people who were in both parts
  shared_ids <- intersect(clean_surveys[[1]]$prolificID, clean_surveys[[2]]$prolificID)
  filtered <- map(clean_surveys, ~ {
    .x %>% 
      filter(prolificID %in% shared_ids) %>% 
      arrange(prolificID)
    })
  
  # Make consistency data frame, where each cell is 1 if consistent, 0 if not
  consistency <- map(rebl_items, ~ {
    case_when(filtered[[1]][.x] == filtered[[2]][.x] ~ 1,
              .default = 0)
  }) %>% 
    as.data.frame() %>%
    setNames(rebl_items)
  
  # Get item consistency
  item_consistency <- consistency %>% 
    select(matches(rebl_items)) %>% 
    colSums() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('rebl_item', 'total_consistent')) %>% 
    mutate(prop_consistent = round(total_consistent / (nrow(filtered[[1]])), 3)) %>% 
    arrange(prop_consistent)
  
  # Filter by given bounds
  chosen_items <- item_consistency %>% 
      filter(prop_consistent >= lower_bound & prop_consistent < upper_bound)
  
  return(chosen_items)
}


# Get Consistency --------------------------------------------------------


get_consistency <- function(rebl_items,
                            clean_surveys) {
  # Filter down both surveys to only the people who were in both parts
  shared_ids <- intersect(clean_surveys[[1]]$prolificID, clean_surveys[[2]]$prolificID)
  filtered <- map(clean_surveys, ~ {
    .x %>%
      filter(prolificID %in% shared_ids) %>%
      arrange(prolificID)
  })
  
  # Make consistency data frame, where each cell is 1 if consistent, 0 if not
  consistency <- map(rebl_items, ~ {
    case_when(filtered[[1]][.x] == filtered[[2]][.x] ~ 1, .default = 0)
  }) %>%
    as.data.frame() %>%
    setNames(rebl_items)
  
  # Get item consistency
  item_consistency <- consistency %>%
    select(matches(rebl_items)) %>%
    colSums() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    setNames(c('rebl_item', 'total_consistent')) %>%
    mutate(prop_consistent = round(total_consistent / (nrow(filtered[[1]])), 3)) %>%
    arrange(prop_consistent) %>% 
    select(-total_consistent)
  
  return(item_consistency)
}


# Get LR Test -------------------------------------------------------------


get_lr_test <- function(rebl_items, clean_surveys) {
  map(clean_surveys, ~ {
    .x %>% 
      select(all_of(rebl_items)) %>%
      RM() %>% 
      LRtest()
  })
}


# Get RM -------------------------------------------------------------


get_rm <- function(rebl_items, clean_surveys) {
  map(clean_surveys, ~ {
    .x %>% 
      select(all_of(rebl_items)) %>%
      RM()
  })
}



# Get Uni Test ------------------------------------------------------------


# Single Survey
get_uni_test <- function(rebl_items, clean_surveys) {
  results <- clean_surveys %>% 
    select(all_of(rebl_items)) %>% 
    as.matrix() %>% 
    NPtest(method = 'MLoef')
  return(results)
}



# Get Uni Test Multiple ---------------------------------------------------


# Multiple Surveys
get_uni_test <- function(rebl_items, clean_surveys) {
  set <- deparse(substitute(rebl_items))
  cat('\n~~~ Starting', set, 'at', format(Sys.time(), '%X'), '~~~')
  tic()
  results <- imap(clean_surveys, ~ {
    cat('\nStarting', .y)
    .x %>%
      select(all_of(rebl_items)) %>%
      as.matrix() %>%
      NPtest(method = 'MLoef')
  })
  toc()
  return(results)
}
