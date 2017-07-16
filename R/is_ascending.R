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
#' The methods for numeric and integer vectors are implemented
#' in C++. `is_ascending()` has about the same performance as
#' `!base::is.unsorted()`, but `is_descending(x)`is slighlty faster 
#' than `!base::is.unsorted(-x)` as it does not need to reverse / negate the
#' vector first. 
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




#' @export
is_ascending.integer <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_ascending_integer', PACKAGE = 'isit', x)
}




#' @export
is_ascending.numeric <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_ascending_numeric', PACKAGE = 'isit', x)
}




#' @rdname is_ascending
#' @export
is_strictly_ascending <- function(x, na.rm = FALSE){
  UseMethod('is_strictly_ascending')
}




#' @export
is_strictly_ascending.default <- function(x, na.rm = FALSE){
  is.unsorted(x, na.rm = na.rm, strictly = TRUE)
}




#' @export
is_strictly_ascending.integer <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_strictly_ascending_integer', PACKAGE = 'isit', x)
}




#' @export
is_strictly_ascending.numeric <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_strictly_ascending_numeric', PACKAGE = 'isit', x)
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




#' @export
is_descending.integer <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_descending_integer', PACKAGE = 'isit', x)
}




#' @export
is_descending.numeric <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_descending_numeric', PACKAGE = 'isit', x)
}




#' @rdname is_ascending
#' @export
is_strictly_descending <- function(x, na.rm = FALSE){
  UseMethod('is_strictly_descending')
}




#' @export
is_strictly_descending.default <- function(x, na.rm = FALSE){
  is.unsorted(rev(x), na.rm = na.rm, strictly = TRUE)
}




#' @export
is_strictly_descending.integer <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_strictly_descending_integer', PACKAGE = 'isit', x)
}




#' @export
is_strictly_descending.numeric <- function(x, na.rm = FALSE) {
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  .Call('_isit_is_strictly_descending_numeric', PACKAGE = 'isit', x)
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
