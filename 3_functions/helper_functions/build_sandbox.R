#' build sandbox
#' 2024-04-420

#' Function that creates a bunch of objects to play around with and test things.
#' also another function that removes those things.

# Packages
pacman::p_load(dplyr)

# build sandbox
build_sandbox <- function() {
  
  my_list <<- list(1, 'a', TRUE, 'bleh', 5.51)
  
  my_nlist <<- list(
    colors = list('green', 'blue', 'red', 'purple'),
    weather = 'sunny',
    numbers = list(31, 456, 412, 46, 897, 2),
    cat_names = list(
      food_names = c('Hummus', 'Falafel', 'Waffle', 'Pancake'),
      world_leader_puns = c('King George the Purred', 'Margaret Scratcher', 'Meow Zedong'),
      silly_names = c('Mittens', 'Flop', 'Kitty', 'Dog')
    )
  )
  
  my_vector <<- c(1, 5, 3, 67, 8, 32, 5, 2)
  
  my_df <<- data.frame(
    name = c('jeff', 'ali', 'fred', 'jan'),
    age = c(29, 41, 59, 55),
    fav_color = c('green', 'blue', 'red', 'purple'),
    height = c(44.1, 29.6, 97.1, 28.8)
  )
  
  # Print what is in the sandbox
  cat('Sandbox toys: my_list, my_nlist, my_vector, my_df')
}

# Remove sandbox
remove_sandbox <- function() {
  suppressWarnings(
    tryCatch(
      rm(list = c('my_list', 'my_nlist', 'my_vector', 'my_df'), 
         envir = .GlobalEnv)
    )
  )
}
