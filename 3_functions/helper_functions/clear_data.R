# remove_data_objects
# 2023.12.24

# Put this at end of scripts to remove non-function objects
remove_data_objects <- \() {
    rm(list = setdiff(ls(envir = .GlobalEnv),
                      lsf.str(envir = .GlobalEnv)),
       envir = .GlobalEnv)
}

# Same thign with a different name - phase the other one out because it's long
clear_data <- \() {
  rm(list = setdiff(ls(envir = .GlobalEnv),
                    lsf.str(envir = .GlobalEnv)),
     envir = .GlobalEnv)
}
