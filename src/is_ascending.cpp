#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = "is_ascending_numeric")]]
bool is_ascending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++) if(x[i] > x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = "is_strictly_ascending_numeric")]]
bool is_strictly_ascending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] >= x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = is_descending_numeric)]]
bool is_descending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] < x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = "is_strictly_descending_numeric")]]
bool is_strictly_descending_numeric(Rcpp::NumericVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++)  if (x[i] <= x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = "is_ascending_integer")]]
bool is_ascending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++) if(x[i] > x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = is_strictly_ascending_integer)]]
bool is_strictly_ascending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] >= x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = "is_descending_integer")]]
bool is_descending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for(int i = 0; i < n; i++) if(x[i] < x[i+1]) return false;
  return true;
}




// [[Rcpp::export(name = is_strictly_descending_integer)]]
bool is_strictly_descending_integer(Rcpp::IntegerVector x){
  int n = x.length() - 1;
  for (int i = 0; i < n; i++)  if (x[i] <= x[i+1]) return false;
  return true;
}
