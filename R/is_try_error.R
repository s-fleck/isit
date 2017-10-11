#' Check If Object is a Try-Error
#'
#' @param x Any \r object
#'
#' @return A logical sclar
#' @export
#' @examples 
#'
#' x <- try(stop())
#' is_try_error(x)
#'
is_try_error <- function(x){
  inherits(x, "try-error")
}