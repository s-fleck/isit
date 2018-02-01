as_validation <- function(x){
  assert_that(is.list(x) || is.logical(x)) 

  chk <- unlist(x)
  res <- all(chk)
  attr(res, 'errors') <- names(chk)[-which(chk)]
  res
}