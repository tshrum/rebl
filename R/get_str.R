#' Get Structure (Enhanced str() wrapper)
#'
#' This is a wrapper for str() to improve quality of life. Behaves slightly 
#' differently depending on the type of object.
#'
#' @param obj Object to examine structure of
#' @param level Maximum level of nesting to display (default: 2)
#' @param all_cols Whether to display all columns/elements (default: TRUE)
#' @param strict.width How to handle width constraints (default: 'cut')
#' @param ... Additional arguments passed to str()
#'
#' @return Prints the structure of the object (invisibly returns NULL)
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' get_str(mtcars)
#' get_str(list(a = 1:10, b = letters[1:5]), level = 1)
#' }
get_str <- function(obj,
                    level = 2,
                    all_cols = TRUE,
                    strict.width = 'cut',
                    ...) {
  
  if (is.data.frame(obj)) {
    
    if (all_cols == TRUE) {
      
      utils::str(object = obj,
          max.level = level,
          strict.width = strict.width,
          list.len = ncol(obj),
          vec.len = 100,
          ...)
      
    } else {
      
      utils::str(object = obj,
          max.level = level,
          strict.width = strict.width,
          vec.len = 100,
          ...)
    }
  
  } else if (is.list(obj) && all_cols == TRUE) {
    
    utils::str(object = obj,
        max.level = level,
        strict.width = strict.width,
        vec.len = 100,
        ...)
      
  } else if (is.list(obj) && all_cols == FALSE) {
    
    utils::str(object = obj,
        max.level = level,
        strict.width = strict.width,
        list.len = 250,
        vec.len = 100,
        ...)
  
  } else if (all_cols == TRUE) {
    
    utils::str(object = obj,
        max.level = level,
        strict.width = strict.width,
        vec.len = 100,
        list.len = ncol(obj),
        ...)
    
  } else {
    
    utils::str(object = obj,
        max.level = level,
        strict.width = strict.width,
        ...)
  }
}