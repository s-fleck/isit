#' Check for equality within a tolerance level
#'
#' @param x,y `numeric` vectors
#' @param tolerance `numeric` scalar. tolerance level (absolute value). Defaults
#'   to `.Machine$double.eps^0.5` which is a sensible default for comparing
#'   floating point numbers.
#'
#' @return `logical` vector.
#' @export
#' @seealso [.Machine]
#'
#' @examples
#' a - b == 0.5
#' equalish(a-b, 0.5)
#' 
equalish <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  assert_that(identical(length(tolerance), 1L) && is.numeric(tolerance))
  abs(x - y) < tolerance
}



#' @export
equalish_frac <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  (abs(x - y) / ((x+y) / 2)) < tolerance
}





#' Check for equality but treat NAs like normal values
#'
#' @param x Any R object that can be handled by `==`
#' @param y Any R object that can be handled by `==`
#'
#' @return a logical vector.
#'
#' @export
equal_or_na <- function(x, y){
  ((!is.na(x) & !is.na(y)) & (x == y)) | (is.na(x) &  is.na(y))
}