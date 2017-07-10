#' Is an Object empty?
#'
#' for data.frames `is_empty()` returns true if it has either zero rows or
#' zero columns.
#'
#' @param x any R object
#'
#' @return logical. `TRUE` if `x` is emtpy. 
#' @export
is_empty <- function(x){
  UseMethod('is_empty')
} 
 



#' @export
is_empty.default <- function(x){
  identical(length(x), 0L)
}




#' @export
is_empty.data.frame <- function(x){
  identical(nrow(x), 0L) || identical(length(x), 0L)
}