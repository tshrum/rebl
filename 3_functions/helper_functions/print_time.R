#' print_time
#' 2024-05-14

#' Convenience function to format output of Sys.time.

print_time <- function(msg = 'Starting at') {
  cat(
    '\n',
    msg, 
    ' ',
    format(Sys.time(), '%X'),
    '\n',
    sep = ''
  )
}

