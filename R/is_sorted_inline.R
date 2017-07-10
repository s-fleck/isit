#' @useDynLib isit

code4 <- '
  int i;
  
  SEXP res = PROTECT(allocVector(LGLSXP, 1));
  memset(LOGICAL(res), 0, 1);   
  
  for(i = 0; i < length(x) - 1; i++){
  if(REAL(x)[i] > REAL(x)[i+1])
  {
  memset(LOGICAL(res), 1, 1);   
  UNPROTECT(1);
  return res;
  }
  }
  
  UNPROTECT(1);
  return res;
  '


#' @export is_sorted_inline

is_sorted_inline <- inline::cfunction(signature(x="numeric"), code4, language = "C", convention = ".Call", include = "#include <Rinternals.h>")
