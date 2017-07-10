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