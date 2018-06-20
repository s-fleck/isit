# isit 0.0.2.9000

* Added `is_candidate_key()`
* Added `is_blank()`




# isit 0.0.2

* Removed RCPP dependency for better compatibility
* added `is_free_memory()`
* added `duplicated_combinations()`
* added `is_try_error()`
* removed warning for `all_are_identical()` if `x` is of length 1
* removed warning for `all_are_distinct()` if `x` is of length 1. Also removed 
  "silent" argument of function.
* added `equalish()` to check for equality of floating point numbers
* added `equalish_frac()` to check for equality of floating point numbers based
  on the relative difference (expressed as a fraction) of both numbers.




# isit 0.0.1

* initial version
* added `is_sorted()`, `is_ascending()` and `is_descending()`
* migrated all predicates from hammr
