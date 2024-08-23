#' get size
#' 2024-05-14
#' 
#' Wrapper around format(object.size(x)) to get it in Mb or Gb depending on size
#' 
#' No packages needed

get_size <- function(x) {
  
  size <- object.size(x)
  
  # if (size < 1e6) {
  #   format(size, units = 'Kb')
  # } else if (size < 1e9) {
  #   format(size, units = 'Mb')
  # } else if (size >= 1e9) {
  #   format(size, units = 'Gb')
  # }
  # 
  if (size < 1e6) {
    units <- 'Kb'
  } else if (size < 1e9) {
    units <- 'Mb'
  } else if (size >= 1e9) {
    units <- 'Gb'
  }
  
  format(size, units = units)
  
  
}
