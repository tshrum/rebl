#' Print Time with Message
#'
#' Convenience function to format output of Sys.time with a custom message.
#'
#' @param msg Character string message to display (default: 'Starting at')
#'
#' @return Prints formatted time message to console (invisibly returns NULL)
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' print_time()
#' print_time("Analysis completed at")
#' }
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