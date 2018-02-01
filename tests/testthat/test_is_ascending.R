context('is_ascending')

test_that('is_ascending and is_descending work for integer and numeric vectors', {
  
  tdat <- 1:1000
  
  
  test_sorted_predicates <- function(asc){
    desc <- rev(asc)
    
    # ascending
    expect_true(is_ascending(asc))
    expect_true(is_strictly_ascending(asc))
    
    expect_true(is.na(c(is_ascending(c(asc, NA)))))
    expect_true(is.na(c(is_strictly_ascending(c(asc, NA)))))
    
    expect_true(is_ascending(c(asc, asc[length(asc)])))
    expect_false(is_strictly_ascending(c(asc, asc[length(asc)])))
    
    expect_false(is_ascending(c(asc, asc[1])))
    expect_false(is_strictly_ascending(c(asc, asc[1])))
    
    expect_false(is_ascending(desc))
    expect_false(is_strictly_ascending(desc))
    
    # descending
    expect_true(is_descending(desc))
    expect_true(is_strictly_descending(desc))
    
    expect_true(is.na(c(is_descending(c(asc, NA)))))
    expect_true(is.na(c(is_strictly_descending(c(asc, NA)))))
    
    expect_true(is_descending(c(desc, desc[length(desc)])))
    expect_false(is_strictly_descending(c(desc, desc[length(desc)])))
    
    expect_false(is_descending(c(desc, desc[1])))
    expect_false(is_strictly_descending(c(desc, desc[1])))
    
    expect_false(is_descending(asc))
    expect_false(is_strictly_descending(asc))
  }
  
  
  test_sorted_predicates(as.integer(tdat))
  test_sorted_predicates(as.numeric(tdat))
})



test_that('is_sorted.character', {
  d1 <- c('A', "a", 'B', "b")
  d2 <- c('a', "A", 'b', "B")
  
  expect_identical(!is.unsorted(d1), is_ascending(d1))
  expect_identical(!is.unsorted(d2), is_ascending(d2))
})


