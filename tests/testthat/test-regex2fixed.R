context('test regex2fixed.R')

test_that("regex2fixed converts regex patterns correctly", {
      
    regex <- list(c('^a$', '^b'), c('c'), c('d'), c('b$'))
    glob <- list(c('a', 'b*'), c('*c*'), c('*d*'), c('*b'))
    types <- c('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(regex, types, 'fixed', case_insensitive = TRUE),
        list('C', 'c')
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(regex, types, 'fixed', case_insensitive = FALSE),
        list('c')
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(regex, types, 'regex', case_insensitive = TRUE),
        list(c("A", "B"), c("a", "B"), c("A", "BB"), c("a", "BB"), 
             c("A", "b"), c("a", "b"), c("A", "bb"), c("a", "bb"), 
             "C", "CC", "c", "cc", "B", "BB", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(glob, types, 'glob', case_insensitive = TRUE),
        list(c("A", "B"), c("a", "B"), c("A", "BB"), c("a", "BB"), 
             c("A", "b"), c("a", "b"), c("A", "bb"), c("a", "bb"), 
             "C", "CC", "c", "cc", "B", "BB", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(regex, types, 'regex', case_insensitive = FALSE),
        list(c("a", "b"), c("a", "bb"), "c", "cc", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(glob, types, 'glob', case_insensitive = FALSE),
        list(c("a", "b"), c("a", "bb"), "c", "cc", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('?', types, 'glob', case_insensitive = FALSE),
        list("A", "B", "C", "a", "b", "c")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('?', types, 'glob', case_insensitive = TRUE),
        list("A", "B", "C", "a", "b", "c")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('a?', types, 'glob', case_insensitive = FALSE),
        list("aa")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('a?', types, 'glob', case_insensitive = TRUE),
        list("AA", "aa")
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('??', types, 'glob', case_insensitive = TRUE),
        list("AA", "BB", "CC", "aa", "bb", 'cc')
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('*', types, 'glob', case_insensitive = FALSE),
        list('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    ), list())
    
    expect_identical(setdiff(
        quanteda:::regex2fixed('*', types, 'glob', case_insensitive = TRUE),
        list('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    ), list())
    
    
    
})

test_that("regex2fixed converts complex regex patterns correctly", {
    
    regex <- list(c('a...b'), c('c.*d'), c('e.+f'), c('^g[xyz]+h$'), c('z'), c('[0-9]'))
    types <- c('axxxb', 'cxxxd', 'exxxf', 'gyyyh', 'azzzb', 'a999b')
    
    expect_identical(setdiff(
        quanteda:::regex2fixed(regex, types, 'regex', case_insensitive = TRUE),
        list('axxxb', 'cxxxd', 'exxxf', 'gyyyh', 'azzzb', 'a999b')
    ), list())
})

test_that("regex2fixed works with character class", {
    
    types <- c('NATO', 'GM', '2000', 'G7')
    expect_equal(quanteda:::regex2fixed('\\d', types, 'regex', case_insensitive = TRUE),
                 list('2000', 'G7'))
    expect_equal(quanteda:::regex2fixed('\\D', types, 'regex', case_insensitive = TRUE),
                 list('NATO', 'GM', 'G7'))
    
})

test_that("regex2fixed converts emoji correctly", {
    
    regex <- ':)'
    types <- c(';)', ':(', ':)', ':/', '(;')
    expect_identical(
        unlist(quanteda:::regex2fixed(regex, types, 'glob', case_insensitive=TRUE)),
        ':)'
    )
    expect_identical(
        unlist(quanteda:::regex2fixed(regex, types, 'fixed', case_insensitive=TRUE)),
        ':)'
    )
})
