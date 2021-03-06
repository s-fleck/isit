#' Are all values TRUE? Warn if not.
#'
#' Checks if all values of a vector or list are `TRUE`, throws a an warning if
#' not.
#'
#' @param x anything that can be coerced to a list of only `logical`` values
#'   with [as.list()]
#' @param na_value value to replace NAs with (allowed values are `TRUE`,
#'   `FALSE`, `NA`)
#' @param silent `logical`. If `TRUE` no warning is thrown.
#'
#' @return `logical` scalar with attributes. If `all(x)` evaluates to `FALSE` or
#'   `NA`, a warning will be thrown containing the names (if `x` is
#'   named) or indices of the values that are not `TRUE`. In addition, the
#'   indices of those values will be attached as an attribute to the returned
#'   logical scalar (see examples)
#'
#' @md
#' @seealso [all()]
#' @export
#'
#' @examples
#'
#' x <- list(A = TRUE, B = FALSE)
#' all_with_warning(x)
#'
#' #  [1] FALSE
#' #  attr(,"failed")
#' #  [1] 2
#' #  Warning message:
#' #  In all_with_warning(x) : Not TRUE: B
#'
all_with_warning <- function(
  x,
  na_value = FALSE,
  silent = FALSE
){
  assert_that(is.scalar(na_value))
  assert_that(is.flag(na_value) || is.na(na_value))
  assert_that(is.flag(silent))

  x_lst <- as.list(x)
  assert_that(isTRUE(unique(vapply(x_lst, is.logical, logical(1)))))

  x_lst[is.na(x_lst)] <- na_value
  x_vec <- as.logical(x_lst)
  is_all_true <- all(x_vec)
  
  
  if (isTRUE(is_all_true)){
    return(TRUE)
    
  } else {
    failed_idx   <- which(as.logical(lapply(x_lst, function(x) !isTRUE(x))))
    failed_names <- names(x_lst)[failed_idx]
    
    # construct warning
    if (!silent){
      if (is.null(failed_names)){
        failed_msg <- failed_idx
      } else {
        failed_msg <- failed_names
      }
      failed_msg <- paste(failed_msg, collapse = ", ")
      
      warn        <- sprintf("Not TRUE: %s", failed_msg)
      warning(warn)
    }
    
    attr(is_all_true, "failed") <- failed_idx
    return(is_all_true)
  }
}




#' Test if all elements of a vector are identical
#'
#' @param x any object that can be handled by [unique()] (usually a vector or
#'   list)
#' @param empty_value Value to return if function is called on a vector of
#'   length 0 (e.g. `NULL`, `numeric()`, ...)
#'
#' @md
#' @family special equality checks
#' @return `TRUE/FALSE`
#' @export
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_identical <- function(x, empty_value = FALSE) {
  assert_that(length(empty_value) <= 1)
  
  if (length(x) > 0L) {
    return(identical(length(unique(x)), 1L))
    
  } else {
   
    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }
    
    return(empty_value)
  }
}




#' Test if all elements of a vector are unique
#'
#' @inheritParams all_are_identical
#'
#' @return TRUE/FALSE
#'
#' @md
#' @family special equality checks
#' @export
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_distinct <- function(
  x, 
  empty_value = FALSE
){
  assert_that(length(empty_value) <= 1)

  if (identical(length(x), 1L)) {
    return(TRUE)
  
  } else if (length(x) > 1L) {
    return(identical(length(unique(x)), length(x)))
    
  } else {
    
    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }
    
    return(empty_value)
  }
}




#' Convert vector if identical elements to scalar
#'
#' Returns `unique(x)` if all elements of `x` are identical, throws an error if
#' not.
#'
#' @inheritParams all_are_identical
#'
#' @md
#' @family special equality checks
#' @return A scalar of the same type as `x`
#' @export
as_scalar <- function(x){
  res <- unique(x)
  if (is.scalar(res)){
    return(res)
  } else {
    stop("Not all elements of x are identical")
  }
}




#' Test if a Vector or Combination of Vectors is a Candidate Key
#' 
#' Checks if all elements of the atomic vector `x`, or the combination of
#' all elements of `x` if `x` is a `list`, are unique and neither `NA` or
#' `infinite`.
#'
#' @param x a atomic vector or a list of atomic vectors
#'
#' @return `TRUE/FALSE`
#' @export
#'
#' @examples
#' 
#' is_candidate_key(c(1, 2, 3))
#' is_candidate_key(c(1, 2, NA))
#' is_candidate_key(c(1, 2, Inf))
#' 
#' td <- data.frame(
#'   x = 1:10,
#'   y = 1:2,
#'   z = 1:5
#' )
#' 
#' is_candidate_key(list(td$x, td$z))
#' # a data.frame is just a special list
#' is_candidate_key(td[, c("y", "z")])

is_candidate_key <- function(x){
  
  if (is.atomic(x)){
    # !is.infinite instead of is.finite because x can be a character vector
    length(x) > 1 &&
    all(!is.infinite(x)) &&
    !any(is.na(x)) && 
    identical(length(unique(x)), length(x))
  } else if (is.list(x)){
    length(x) > 0 &&
    length(x[[1]] > 0) &&
    do.call(is_equal_length, x) &&
    all(vapply(x, function(.x) all(!is.infinite(.x)), logical(1))) &&  
    all(vapply(x, function(.x) !any(is.na(.x)), logical(1))) &&
    !any(duplicated(as.data.frame(x)))
  }
}
