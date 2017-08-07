# is_ascending ------------------------------------------------------------

#' Is an Object Sorted?
#' 
#' `is_ascending()` and `is_descending()` test if an object is sorted in 
#' ascending or descending order. Objects of length `<= 1` are always assumed 
#' sorted. 
#' 
#' @section Implementation:
#' 
#' `is_ascending()` and `is_descending()` are generics, so you can easily 
#' implement your own methods if you want to.
#'  
#' The default methods fall back to wrapping [base::is.unsorted()], and 
#' therefore support all the same classes (character, complex, raw and others)
#' at the same performance.
#' 
#' @param x any R object.
#' @param na.rm logical. Remove `NA` values before testing.
#' 
#' @return a logical sclar.
#' @seealso [base::is.unsorted()]
#'
#' @export
is_ascending <- function(x, na.rm = FALSE){
  UseMethod('is_ascending')
}




#' @export
is_ascending.default <- function(x, na.rm = FALSE){
  !is.unsorted(x, na.rm = na.rm, strictly = FALSE)
}




#' @rdname is_ascending
#' @export
is_strictly_ascending <- function(x, na.rm = FALSE){
  UseMethod('is_strictly_ascending')
}




#' @export
is_strictly_ascending.default <- function(x, na.rm = FALSE){
  !is.unsorted(x, na.rm = na.rm, strictly = TRUE)
}




# is descending -----------------------------------------------------------

#' @rdname is_ascending
#' @export
is_descending <- function(x, na.rm = FALSE){
  UseMethod('is_descending')
}




#' @export
is_descending.default <- function(x, na.rm = FALSE){
  !is.unsorted(rev(x), na.rm = na.rm, strictly = FALSE)
}




#' @rdname is_ascending
#' @export
is_strictly_descending <- function(x, na.rm = FALSE){
  UseMethod('is_strictly_descending')
}




#' @export
is_strictly_descending.default <- function(x, na.rm = FALSE){
  !is.unsorted(rev(x), na.rm = na.rm, strictly = TRUE)
}




# on failure --------------------------------------------------------------

on_failure(is_ascending) <- function(call, env){
  sprintf('Object "%s" is not in ascending order.', deparse(call[[2]]))
}




on_failure(is_ascending) <- function(call, env){
  sprintf('Object "%s" is not in strictly ascending order.', deparse(call[[2]]))
}




on_failure(is_strictly_descending) <- function(call, env){
  sprintf('Object "%s" is not in strictly descending order.', deparse(call[[2]]))
}




on_failure(is_descending) <- function(call, env){
  sprintf('Object "%s" is not in descending order.', deparse(call[[2]]))
}
