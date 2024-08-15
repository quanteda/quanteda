test_that("pattern2fixed converts regex patterns correctly", {
      
    regex <- list(c('^a$', '^b'), c('c'), c('d'), c('b$'))
    glob <- list(c('a', 'b*'), c('*c*'), c('*d*'), c('*b'))
    type <- c('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    
    expect_identical(setdiff(
        pattern2fixed(regex, type, 'fixed', case_insensitive = TRUE),
        list('C', 'c')
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed(regex, type, 'fixed', case_insensitive = FALSE),
        list('c')
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed(regex, type, 'regex', case_insensitive = TRUE),
        list(c("A", "B"), c("a", "B"), c("A", "BB"), c("a", "BB"), 
             c("A", "b"), c("a", "b"), c("A", "bb"), c("a", "bb"), 
             "C", "CC", "c", "cc", "B", "BB", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed(glob, type, 'glob', case_insensitive = TRUE),
        list(c("A", "B"), c("a", "B"), c("A", "BB"), c("a", "BB"), 
             c("A", "b"), c("a", "b"), c("A", "bb"), c("a", "bb"), 
             "C", "CC", "c", "cc", "B", "BB", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed(regex, type, 'regex', case_insensitive = FALSE),
        list(c("a", "b"), c("a", "bb"), "c", "cc", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed(glob, type, 'glob', case_insensitive = FALSE),
        list(c("a", "b"), c("a", "bb"), "c", "cc", "b", "bb")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('?', type, 'glob', case_insensitive = FALSE),
        list("A", "B", "C", "a", "b", "c")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('?', type, 'glob', case_insensitive = TRUE),
        list("A", "B", "C", "a", "b", "c")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('a?', type, 'glob', case_insensitive = FALSE),
        list("aa")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('?b', type, 'glob', case_insensitive = FALSE),
        list("bb")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('a?', type, 'glob', case_insensitive = TRUE),
        list("AA", "aa")
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('??', type, 'glob', case_insensitive = TRUE),
        list("AA", "BB", "CC", "aa", "bb", 'cc')
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('*', type, 'glob', case_insensitive = FALSE),
        list('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    ), list())
    
    expect_identical(setdiff(
        pattern2fixed('*', type, 'glob', case_insensitive = TRUE),
        list('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc')
    ), list())
    
})

test_that("pattern2fixed converts complex patterns correctly", {
    
    regex <- list(c('a...b'), c('c.*d'), c('e.+f'), c('^g[xyz]+h$'), c('z'), c('[0-9]'))
    glob <- list("<*>")
    type <- c('axxxb', 'cxxxd', 'exxxf', 'gyyyh', 'azzzb', 'a999b', 
              "<*>", "<xxx>", "<xx.yy>")
    
    expect_setequal(
        pattern2fixed(regex, type, 'regex', case_insensitive = TRUE),
        list('axxxb', 'cxxxd', 'exxxf', 'gyyyh', 'azzzb', 'a999b')
    )
    
    expect_setequal(
        pattern2fixed(glob, type, 'glob', case_insensitive = TRUE),
        list("<*>", "<xxx>", "<xx.yy>")
    )
})

test_that("pattern2fixed works with character class", {
    
    type <- c('NATO', 'GM', '2000', 'G7')
    expect_equal(pattern2fixed('\\d', type, 'regex', case_insensitive = TRUE),
                 list('2000', 'G7'))
    expect_equal(pattern2fixed('\\D', type, 'regex', case_insensitive = TRUE),
                 list('NATO', 'GM', 'G7'))
    
})

test_that("pattern2fixed converts emoji correctly", {
    
    regex <- ':)'
    type <- c(';)', ':(', ':)', ':/', '(;')
    expect_identical(
        unlist(pattern2fixed(regex, type, 'glob', case_insensitive=TRUE)),
        ':)'
    )
    expect_identical(
        unlist(pattern2fixed(regex, type, 'fixed', case_insensitive=TRUE)),
        ':)'
    )
})

test_that("index_types works fine with empty types", {

    expect_type(quanteda:::index_types("a*", character(), 'glob', FALSE),
                "list")
    expect_type(quanteda:::index_types("a*", character(), 'fixed', FALSE),
                "list")
    expect_type(quanteda:::index_types("a*", character(), 'regex', FALSE),
                "list")
})

test_that("index_types works fine with large vectors", {
    
    pat <- unique(stringi::stri_rand_strings(100000, 1:10))
    type <- unique(stringi::stri_rand_strings(1000000, 1:10))
    expect_type(quanteda:::index_types(pat, type, 'fixed', FALSE),
                "list")

})

test_that("glob patterns that contain regex special characters works", {
    
    expect_equal(pattern2fixed('*.aaa', 'bbb.aaa', 'glob', FALSE),
                 list('bbb.aaa'))
    expect_equal(pattern2fixed('*[aaa', 'bbb[aaa', 'glob', FALSE),
                 list('bbb[aaa'))
    expect_equal(pattern2fixed('?a(aa', 'ba(aa', 'glob', FALSE),
                 list('ba(aa'))
    expect_equal(pattern2fixed('.aaa*', '.aaabbb', 'glob', FALSE),
                 list('.aaabbb'))
    expect_equal(pattern2fixed('[aaa*', '[aaabbb', 'glob', FALSE),
                 list('[aaabbb'))
    expect_equal(pattern2fixed('a(aa?', 'a(aab', 'glob', FALSE),
                 list('a(aab'))
    
})

test_that("add value check for types (#1463)", {
    
    v1 <- list(c("a", "bb", "cc"))
    v2 <- letters
    v3 <- c(TRUE, FALSE)
    expect_error(pattern2fixed(v1, v1, 'regex', TRUE))
    expect_silent(pattern2fixed(v1, v2, 'regex', TRUE))
    expect_error(pattern2fixed(v1, v3, 'regex', TRUE))
    
})

test_that("keep_unmatched is working", {
  pattern <- list(c("a", "b"), c("a", "z"), "x")
  type <- c("a", "b", "c")
  expect_identical(quanteda:::pattern2id(pattern, type, "fixed", keep_nomatch = FALSE),
                   list(c(1L, 2L)))
  expect_identical(quanteda:::pattern2id(pattern, type, "fixed", keep_nomatch = TRUE),
                   list(c(1L, 2L), integer(), integer()))
})

test_that("unlist_integer() is working", {
  
  expect_equal(quanteda:::unlist_integer(list(c(1L, 6L), 6L, 0L)),
               c(1L, 6L, 6L, 0L))
  expect_equal(quanteda:::unlist_integer(list(c(1L, 6L), 6L, 0L), unique = TRUE),
               c(1L, 6L, 0L))
  expect_equal(quanteda:::unlist_integer(list()),
               integer())
  
})

test_that("unlist_character() is working", {
  
  expect_equal(quanteda:::unlist_character(list(c("a", "d"), "d", "e", "")),
               c("a", "d", "d", "e", ""))
  expect_equal(quanteda:::unlist_character(list(c("a", "d"), "d", "e", ""), unique = TRUE),
               c("a", "d", "e", ""))
  expect_equal(quanteda:::unlist_character(list()),
               character())
  
})

test_that("used of index do not change the result", {

    fixed <- list(c('a', 'BB'), c('CC'), c('*d*'), c('bb'), 'd*d')
    glob <- list(c('a', 'B**'), c('*c*'), c('*d*'), c('*B'), 'ddd')
    type <- c('A', 'AA', 'B', 'BB', 'C', 'CC', 'a', 'aa', 'b', 'bb', 'c', 'cc', 'ddd')
    
    expect_identical(
        pattern2fixed(fixed, type, 'fixed', case_insensitive = FALSE, use_index = TRUE),
        pattern2fixed(fixed, type, 'fixed', case_insensitive = FALSE, use_index = FALSE)
    )
    expect_identical(
        pattern2fixed(glob, type, 'glob', case_insensitive = TRUE, use_index = TRUE),
        pattern2fixed(glob, type, 'glob', case_insensitive = TRUE, use_index = FALSE)
    )

})

test_that("index_types works correctly", {
    
    type <- c("abcd", "abc", "ab", "ABCD", "ABC", "AB")
    
    index1 <- quanteda:::index_types("a*", type)
    expect_null(names(index1))
    expect_equal(
        attributes(index1),
        list(types_search = c("abcd", "abc", "ab", "abcd", "abc", "ab"),
             types = type, valuetype = "glob", 
             case_insensitive = TRUE, key = "a*")
    )
    
    index2 <- quanteda:::index_types("a*", type, case_insensitive = FALSE)
    expect_null(names(index2))
    expect_equal(
        attributes(index2),
        list(types_search = type, types = type, valuetype = "glob", 
             case_insensitive = FALSE, key = "a*")
    )
    
    # do not index for regex
    index3 <- quanteda:::index_types("a*", type, valuetype = "regex")
    expect_null(names(index3))
    expect_equivalent(index3, list())
    expect_equal(
        attributes(index3),
        list(types_search = c("abcd", "abc", "ab", "ABCD", "ABC", "AB"),
             types = type, valuetype = "regex", 
             case_insensitive = TRUE, key = character())
    )
})

test_that("cpp_index_types works correctly", {

    type <- c("abcd", "abc", "ab", "ABCD", "ABC", "AB")
    type_lower <- stringi::stri_trans_tolower(type)
    
    expect_equal(quanteda:::cpp_index_types("abc", type, FALSE),
                 list("abc" = 2))
    expect_equal(quanteda:::cpp_index_types("abc", type_lower, FALSE),
                 list("abc" = c(2, 5)))
    expect_equal(quanteda:::cpp_index_types("ab*", type, FALSE),
                 list("ab*" = integer()))
    expect_equal(quanteda:::cpp_index_types("*ab", type, FALSE),
                 list("*ab" = integer()))
    expect_equal(quanteda:::cpp_index_types("*", type, FALSE),
                 list("*" = integer()))
    
    expect_equal(quanteda:::cpp_index_types("ab*", type),
                 list("ab*" = c(1, 2, 3)))
    expect_equal(quanteda:::cpp_index_types("abc*", type_lower),
                 list("abc*" = c(1, 2, 4, 5)))
    expect_equal(quanteda:::cpp_index_types("*bc", type),
                 list("*bc" = c(2)))
    expect_equal(quanteda:::cpp_index_types("*bc", type_lower),
                 list("*bc" = c(2, 5)))
    
    
    # no wildcard
    expect_equal(quanteda:::cpp_index_types("ab", type),
                 list("ab" = 3))
    expect_equal(quanteda:::cpp_index_types("ab", type_lower),
                 list("ab" = c(3, 6)))
    expect_equal(quanteda:::cpp_index_types("bc", type),
                 list("bc" = integer()))
    expect_equal(quanteda:::cpp_index_types("bc", type_lower),
                 list("bc" = integer()))

    # both sides
    expect_equal(quanteda:::cpp_index_types("*b*", type),
                 list("*b*" = integer()))
    expect_equal(quanteda:::cpp_index_types("?b?", type),
                 list("?b?" = integer()))
    
})
