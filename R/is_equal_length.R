#' Are R objects of equal length?
#' 
#' @param ... any number of R object
#'
#' @export
is_equal_length <- function(...){
  dots <- list(...)
  for(i in seq_along(dots)){
    if(!identical(length(dots[[1]]), length(dots[[i]]))){
      return(FALSE)
    }
  }
  
  TRUE
}
