#' item reduction consistency
#' 2024-08-03 update
#' 
#' Exploring consistency of items to make any more cuts. 
#' We decided to keep all the most consistent items, but ditch those under 0.75.
#' This is step 3 in item reduction, after conditional fit statistics and
#' stepwise elimination with LR tests. We also make some qualitative cuts at the
#' end here, including ditching all transportation questions, foodForage,
#' foodOwnLunch, and others.
#' 
#' Inputs:
#'    1. Vector of 36 items that came through from the stepwise lr tests
#'    2. Survey data for 2a and 2b
#' 
#' Outputs:
#'    1. Final set of 24 REBL Items



# Load Packages and Data --------------------------------------------------


pacman::p_load(dplyr,
               purrr,
               eRm,
               tibble)

# Pull surveys 2a and 2b
surveys <- readRDS('2_clean/all_surveys_imputed.rds')[2:3]

# List of all REBL names to pull out those vars only
rebl_items <- readRDS('5_objects/cml/item_reduction/rebl_items_step_lr_36.rds')



# Wrangle -----------------------------------------------------------------


#' Since we are comparing responses across both surveys, we first need to
#' reduce cases so that we only have respondents who participated in both 
#' surveys. 

# Start by reducing surveys to just the rebl items and prolific IDs. Also
# arrange by ID to make sure they are in the same order
dat <- map(surveys, ~ {
  .x %>% 
    select(prolificID, all_of(rebl_items)) %>% 
    arrange(prolificID)
})

get_str(dat, 3)

# Now reduce to only shared respondents
shared_ids <- intersect(surveys[[1]]$prolificID, surveys[[2]]$prolificID)

length(shared_ids)
# 810 shared

# Check against original cases
nrow(surveys[[1]])
# 924

nrow(surveys[[2]])
# 909

# Lost a fair number here! Probably excluded people in one survey but not other

# Now just filter both surveys to have the same people in them:
dat <- map(dat, ~ {
  .x %>% 
    filter(prolificID %in% shared_ids)
})

get_str(dat)
# 810 each now. Good!



# New RM ------------------------------------------------------------------


#' Run RM again with just those 36 items that passed the LR test. Until now, we
#' have been doing each survey independently. Just testing this out, not going
#' anywhere. 
new_models <- map(dat, ~ {
  .x %>% 
    select(-prolificID) %>% 
    RM()
})

get_str(new_models)

# Try another LRtest here
(lr <- map(new_models, LRtest))
map(lr, plotGOF)
# $survey_2a
# Andersen LR-test: 
# LR-value: 41.075 
# Chi-square df: 35 
# p-value:  0.222 
 
# $survey_2b
# Andersen LR-test: 
# LR-value: 32.236 
# Chi-square df: 35 
# p-value:  0.602



# Consistency DF ----------------------------------------------------------


# Make consistency data frame, where each cell is 1 if consistent, 0 if not
consistency <- map(rebl_items, ~ {
  case_when(dat[[1]][.x] == dat[[2]][.x] ~ 1,
            .default = 0)
}) %>% 
  as.data.frame() %>%
  setNames(rebl_items) %>%
  mutate(prolificID = dat[[1]]$prolificID)

get_str(consistency)

# Check a few manually to make sure it worked:
get_str(consistency)
get_str(dat, 3)



# Person Consistency ------------------------------------------------------


# Now make person consistency by adding totals for each row
person_consistency <- consistency %>% 
  mutate(total_consistent = rowSums(select(., -prolificID)),
         prop_consistent = total_consistent / (ncol(.) - 1))

# Check them out
get_str(person_consistency)
hist(person_consistency$prop_consistent)

# Save person consistency DF (just ID and prop consistent)
person_consistency %>% 
  select(prolificID, prop_consistent) %>% 
  saveRDS('5_objects/cml/item_reduction/person_consistency.rds')



# Item Consistency --------------------------------------------------------


# Use consistency DF, but get column sums for item consistency
item_consistency <- consistency %>% 
  select(matches(rebl_items)) %>% 
  colSums() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('rebl_item', 'total_consistent')) %>% 
  mutate(prop_consistent = round(total_consistent / (nrow(dat[[1]])), 3)) %>% 
  arrange(prop_consistent)

get_str(item_consistency)
range(item_consistency$prop_consistent)
hist(item_consistency$prop_consistent)

# Save this with just item name and prop consistent
item_consistency %>% 
  select(rebl_item, prop_consistent) %>% 
  saveRDS('5_objects/cml/item_reduction/item_consistency.rds')



# Reduce by Item Consistency ----------------------------------------------


# Check them out
item_consistency
hist(item_consistency$prop_consistent, breaks = 25)

# But let's try looking at quantiles and see if a more objective split emerges
quantile(item_consistency$prop_consistent, probs = c(0.05, 0.1, 0.9, 0.95))

# Dropping those less than 0.75, but keeping the most consistent
(consistent_items <- item_consistency %>%
  filter(prop_consistent > 0.75))



# Qualitative Reduction ---------------------------------------------------


#' After chat on 2024-05-16 decided to remove:
#'  1. foodGarden_
#'  2. purchSlowShipping_
#'  3. packDilutedSoap_

#' After chat on 2024-07-17, we are ditching more, including all trans questions
#'  1. packReusableBottle_
#'  2. foodOrganicVeg_
#'  3. transPublic_
#'  4. transWalk_

#' After emails on 2024-07-26, also removing forage (seasonal) and foodOwnLunch
#' (partly to be able to use validation set). Also keeping all the most
#' consistent items

# Filtering both sets
final_items <- consistent_items %>%
  filter(
    !rebl_item %in% c(
      'foodGarden_',
      'purchSlowShipping_',
      'packDilutedSoap_',
      'packReusableBottle_',
      'foodOrganicVeg_',
      'transPublic_',
      'transWalk_',
      'transBike_',
      'foodOwnLunch_',
      'foodForage_'
    )
  ) %>%
  pull(rebl_item) %>%
  sort()

final_items
# Final set of 24 here



# Save and Clear ----------------------------------------------------------


# Save this vector of final names for other analyses. Also in clean folder
saveRDS(final_items,'5_objects/cml/item_reduction/rebl_items_final.rds')
saveRDS(final_items, '2_clean/rebl_items_final.rds')

clear_data()