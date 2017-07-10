// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// is_ascending_numeric
Rcpp::LogicalVector is_ascending_numeric(Rcpp::NumericVector x);
RcppExport SEXP _isit_is_ascending_numeric(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_ascending_numeric(x));
    return rcpp_result_gen;
END_RCPP
}
// is_strictly_ascending_numeric
Rcpp::LogicalVector is_strictly_ascending_numeric(Rcpp::NumericVector x);
RcppExport SEXP _isit_is_strictly_ascending_numeric(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_strictly_ascending_numeric(x));
    return rcpp_result_gen;
END_RCPP
}
// is_descending_numeric
Rcpp::LogicalVector is_descending_numeric(Rcpp::NumericVector x);
RcppExport SEXP _isit_is_descending_numeric(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_descending_numeric(x));
    return rcpp_result_gen;
END_RCPP
}
// is_strictly_descending_numeric
Rcpp::LogicalVector is_strictly_descending_numeric(Rcpp::NumericVector x);
RcppExport SEXP _isit_is_strictly_descending_numeric(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_strictly_descending_numeric(x));
    return rcpp_result_gen;
END_RCPP
}
// is_ascending_integer
Rcpp::LogicalVector is_ascending_integer(Rcpp::IntegerVector x);
RcppExport SEXP _isit_is_ascending_integer(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_ascending_integer(x));
    return rcpp_result_gen;
END_RCPP
}
// is_strictly_ascending_integer
Rcpp::LogicalVector is_strictly_ascending_integer(Rcpp::IntegerVector x);
RcppExport SEXP _isit_is_strictly_ascending_integer(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_strictly_ascending_integer(x));
    return rcpp_result_gen;
END_RCPP
}
// is_descending_integer
Rcpp::LogicalVector is_descending_integer(Rcpp::IntegerVector x);
RcppExport SEXP _isit_is_descending_integer(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_descending_integer(x));
    return rcpp_result_gen;
END_RCPP
}
// is_strictly_descending_integer
Rcpp::LogicalVector is_strictly_descending_integer(Rcpp::IntegerVector x);
RcppExport SEXP _isit_is_strictly_descending_integer(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_strictly_descending_integer(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_isit_is_ascending_numeric", (DL_FUNC) &_isit_is_ascending_numeric, 1},
    {"_isit_is_strictly_ascending_numeric", (DL_FUNC) &_isit_is_strictly_ascending_numeric, 1},
    {"_isit_is_descending_numeric", (DL_FUNC) &_isit_is_descending_numeric, 1},
    {"_isit_is_strictly_descending_numeric", (DL_FUNC) &_isit_is_strictly_descending_numeric, 1},
    {"_isit_is_ascending_integer", (DL_FUNC) &_isit_is_ascending_integer, 1},
    {"_isit_is_strictly_ascending_integer", (DL_FUNC) &_isit_is_strictly_ascending_integer, 1},
    {"_isit_is_descending_integer", (DL_FUNC) &_isit_is_descending_integer, 1},
    {"_isit_is_strictly_descending_integer", (DL_FUNC) &_isit_is_strictly_descending_integer, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_isit(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
