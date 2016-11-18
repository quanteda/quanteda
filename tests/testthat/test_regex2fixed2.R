context('test regex2fixed2.R')

test_that("regex2fixed converts regex patterns correctly", {
      
    regex <- list(c('^a$', '^b'), c('c'), c('d'))
    types <- c('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    expect_identical(
        regex2fixed2(regex, types, 'fixed', case_insensitive=TRUE),
        list('C', 'c')
    )
    expect_identical(
        regex2fixed2(regex, types, 'fixed', case_insensitive=FALSE),
        list('c')
    )
    expect_identical(
        regex2fixed2(regex, types, 'regex', case_insensitive=TRUE),
        list(c("A", "B"), c("a", "B"), c("A", "BB"), c("a", "BB"), 
             c("A", "b"), c("a", "b"), c("A", "bb"), c("a", "bb"), 
             "C", "CC", "c", "cc")
    )
    expect_identical(
        regex2fixed2(regex, types, 'regex', case_insensitive=FALSE),
        list(c("a", "b"), c("a", "bb"), "c", "cc")
    )
})
