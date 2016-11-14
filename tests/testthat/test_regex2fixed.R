library(quanteda)
library(testthat)

context('test regex2fixed.R')

test_that("test that egex2fixed converts regex patterns properly", {
      
    regex <- list(c('^a$', '^b'), c('c'), c('d'))
    types <- c('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    expect_identical(
        regex2fixed(regex, types, 'fixed', case_insensitive=TRUE),
        list('C', 'c')
    )
    expect_identical(
        regex2fixed(regex, types, 'fixed', case_insensitive=FALSE),
        list('c')
    )
    expect_identical(
        regex2fixed(regex, types, 'regex', case_insensitive=TRUE),
        list(c("A", "B"), c("a", "B"), c("A", "BB"), c("a", "BB"), 
             c("A", "b"), c("a", "b"), c("A", "bb"), c("a", "bb"), 
             "C", "CC", "c", "cc")
    )
    expect_identical(
        regex2fixed(regex, types, 'regex', case_insensitive=FALSE),
        list(c("a", "b"), c("a", "bb"), "c", "cc")
    )
})
