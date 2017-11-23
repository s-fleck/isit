#' Check for floating point equality
#'
#' @param x,y `numeric` vectors
#' @param tol `numeric` scalar. tolerance level (absolute numbers)
#'
#' @return `logical` vector.
#' @export
#'
#' @examples
#' a - b == 0.5
#' is_equal(a-b, 0.5)
#' 
is_equal <- function(x, y, tol = .Machine$double.eps^0.5){
  abs(x - y) < tol
}