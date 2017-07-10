#' Test if Object is Sorted
#' 
#' `is_sorted()` tests if an object is sorted (either in ascending or 
#' descending order). Objects of length `<= 1` are assumed sorted. 
#' 
#' @section Implementation:
#' 
#' `is_sorted()`, `is_ascending()` and `is_descending()` are generics.
#'  
#' The methods for numeric and integer vectors are implemented
#' in C++. `is_ascending()` has about the same performance as
#' `!base::is.unsorted()`, but `is_descending(x)` will be slighlty faster 
#' than `!base::is.unsorted(-x)` as it does not need to reverse / negate the
#' vector first. 
#' 
#' The default methods fall back to wrapping [base::is.unsorted()], and 
#' therefore support all the same classes (character, complex, raw and others).
#' 
#' @param x any R object (see `methods(is_ascending)` and `methods(is_desscending)`).
#' @param na.rm logical. Remove `NA` values before testing.
#' @param strictly logical. Only allow *strictly* increasing/decreasing values.
#' 
#' @return logical sclar.
#' @seealso [base::is.unsorted()]
#'
#' @rdname is_sorted
#' @export
is_sorted <- function(x, na.rm = FALSE, strictly = FALSE){
  UseMethod(is_sorted(x, na.rm = na.rm, strictly = strictly))
}




#' @export
is_sorted.default <- function(
  x, 
  na.rm = FALSE, 
  strictly = FALSE
){
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  if(!strictly){
    is_ascending(x) || is_descending(x)
  } else {
    is_strictly_ascending(x) || is_strictly_descending(x)
  }
}




# is ascending ------------------------------------------------------------

#' @rdname is_sorted
#' @export
is_ascending <- function(x, na.rm = FALSE){
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  UseMethod('is_ascending')
}




#' @export
is_ascending.default <- function(x, na.rm = TRUE){
  is.unsorted(x, na.rm = FALSE, strictly = FALSE)
}




#' @rdname is_sorted
#' @export
is_strictly_ascending <- function(x, na.rm = FALSE){
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  UseMethod('is_strictly_ascending')
}




#' @export
is_strictly_ascending.default <- function(x, na.rm = TRUE){
  is.unsorted(x, na.rm = FALSE, strictly = TRUE)
}




# is descending -----------------------------------------------------------

#' @rdname is_sorted
#' @export
is_descending <- function(x, na.rm = FALSE){
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  UseMethod('is_descending')
}




#' @export
is_descending.default <- function(x, na.rm = TRUE){
  is.unsorted(rev(x), na.rm = FALSE, strictly = FALSE)
}




#' @rdname is_sorted
#' @export
is_strictly_descending <- function(x, na.rm = FALSE){
  if (anyNA(x)) {
    if (!na.rm) {
      return(NA)
    } else {
      x <- x[!is.na(x)]
    }
  } 
  
  UseMethod('is_strictly_descending')
}




#' @export
is_strictly_descending.default <- function(x, na.rm = TRUE){
  is.unsorted(rev(x), na.rm = FALSE, strictly = TRUE)
}




# on failure --------------------------------------------------------------

on_failure(is_ascending) <- function(call, env){
  sprintf('Object "%s" is not in ascending order.', deparse(call[[2]]))
}




on_failure(is_sorted) <- function(call, env){
  sprintf('Object "%s" is neither in ascending nor descending order.', deparse(call[[2]]))
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
