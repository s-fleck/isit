#include <Rcpp.h> 

//' @export
// [[Rcpp::export(name = "is_ascending.numeric")]]
Rcpp::LogicalVector is_ascending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++) if(x[i] > x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = "is_strictly_ascending.numeric")]]
Rcpp::LogicalVector is_strictly_ascending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] >= x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = is_descending.numeric)]]
Rcpp::LogicalVector is_descending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] < x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = "is_strictly_descending.numeric")]]
Rcpp::LogicalVector is_strictly_descending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++)  if (x[i] <= x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = "is_ascending.integer")]]
Rcpp::LogicalVector is_ascending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++) if(x[i] > x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = is_strictly_ascending.integer)]]
Rcpp::LogicalVector is_strictly_ascending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] >= x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = "is_descending.integer")]]
Rcpp::LogicalVector is_descending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] < x[i+1]) return false;
  return true; 
}




//' @export
// [[Rcpp::export(name = is_strictly_descending.integer)]]
Rcpp::LogicalVector is_strictly_descending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++)  if (x[i] <= x[i+1]) return false;
  return true; 
}