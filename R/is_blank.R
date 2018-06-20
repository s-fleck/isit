#' Do Elements of a Character Vector Contain Only Blanks?
#' 
#' Checks if elements of a character vector contain only blanks. All strings
#' that are composed only of zero or more spaces (`" "`) are considered blanks.
#'
#' @param x a `character` vector
#'
#' @return a `logical` vector
#' @export
#'
#' @examples
#' 
#' is_blank(c("", "    ", "a"))
#' 
is_blank <- function(x){
  assert_that(is.character(x))
  trimws(x) == ""
}
