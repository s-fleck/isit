#' Check for equality within a tolerance level
#' 
#' 
#'
#' @param x,y `numeric` vectors
#' @param tolerance `numeric` scalar. tolerance level (absolute value). Defaults
#'   to `.Machine$double.eps^0.5` which is a sensible default for comparing
#'   floating point numbers.
#'
#' @return `equalish()` returns TRUE if the absolute difference between `x` and
#'   `y` is less than `tolerance`.
#' @export
#' @seealso [.Machine]
#'
#' @examples
#' a <- 0.7
#' b <- 0.2
#' a - b == 0.5
#' equalish(a - b, 0.5)
#' 
equalish <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  assert_that(identical(length(tolerance), 1L) && is.numeric(tolerance))
  abs(x - y) < tolerance
}




#' @return `equalish_frac()` returns `TRUE` if the relative difference between 
#'   `x` and `y` is smaller than `tolerance`. The relative difference is 
#'   calculated as `abs(x - y) / pmax(abs(x), abs(y))`. If both `x` and `y` are 
#'   `0` the relative difference is not defined, but this function will still 
#'   return `TRUE`.
#' 
#' @rdname equalish
#' @export
#' @examples 
#' 
#' equalish_frac(1000, 1010, tolerance = 0.01)
#' equalish_frac(1000, 1010, tolerance = 0.009)
#' equalish_frac(0, 0)
#'  
equalish_frac <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  res <- reldiff(x, y) < tolerance
  res[x == 0 & y == 0] <- TRUE
  res
}




reldiff <- function(x, y){
  abs(x - y) / pmax(abs(x), abs(y))
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
  ( (!is.na(x) & !is.na(y)) & (x == y) ) | (is.na(x) &  is.na(y))
}
