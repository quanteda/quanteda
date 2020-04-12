context("test char_select functions")

test_that("test character selection", {
    txt <- c("natural", "National", "denatured", "other")
    
    expect_identical(
        char_select(txt, "nat*", selection = "keep", valuetype = "glob",
                    case_insensitive = TRUE),
        c("natural", "National")
    )
    expect_identical(
        char_select(txt, "nat*", selection = "keep", valuetype = "glob",
                    case_insensitive = FALSE),
        c("natural")
    )
    expect_identical(
        char_select(txt, "nat*", selection = "remove", valuetype = "glob",
                    case_insensitive = TRUE),
        c("denatured", "other")
    )
    expect_identical(
        char_select(txt, "nat*", selection = "remove", valuetype = "glob",
                    case_insensitive = FALSE),
        c("National", "denatured", "other")
    )
    expect_identical(
        char_select(txt, "nat", selection = "keep", valuetype = "regex",
                    case_insensitive = TRUE),
        c("natural", "National", "denatured")
    )
    expect_identical(
        char_select(txt, "nat", selection = "keep", valuetype = "regex",
                    case_insensitive = FALSE),
        c("natural", "denatured")
    )
})

test_that("test character selection with named elements", {
    txt <- c(c1 = "aa", c2 = "ab", c3 = "aa", c4 = "bcd", c5 = "bcd")
    
    expect_identical(
        char_select(txt, "*b*", selection = "keep"),
        c(c2 = "ab", c4 = "bcd", c5 = "bcd")
    )
    expect_identical(
        char_select(txt, "a*", selection = "keep"),
        c(c1 = "aa", c2 = "ab", c3 = "aa")
    )
    
    expect_identical(
        char_select(txt, "*b*", selection = "remove"),
        c(c1 = "aa", c3 = "aa")
    )
    expect_identical(
        char_select(txt, "a*", selection = "remove"),
        c(c4 = "bcd", c5 = "bcd")
    )
})

test_that("test that char_remove and char_select work as shortcuts", {
    txt <- c("natural", "natural", "National", "denatured", "other")
  
    expect_identical(
        char_select(txt, "nat*", selection = "keep", valuetype = "glob",
                    case_insensitive = TRUE),
        char_keep(txt, "nat*", valuetype = "glob", case_insensitive = TRUE)
    )
    expect_identical(
        char_select(txt, "nat*", selection = "keep", valuetype = "glob",
                    case_insensitive = FALSE),
        char_keep(txt, "nat*", valuetype = "glob", case_insensitive = FALSE)
    )
    expect_identical(
        char_select(txt, "nat*", selection = "remove", valuetype = "glob",
                    case_insensitive = TRUE),
        char_remove(txt, "nat*", valuetype = "glob", case_insensitive = TRUE)
    )
    expect_identical(
        char_select(txt, "nat*", selection = "remove", valuetype = "glob",
                    case_insensitive = FALSE),
        char_remove(txt, "nat*", valuetype = "glob", case_insensitive = FALSE)
    )
    expect_identical(
        char_select(txt, "nat", selection = "keep", valuetype = "regex",
                    case_insensitive = TRUE),
        char_keep(txt, "nat", valuetype = "regex", case_insensitive = TRUE)
    )
    expect_identical(
        char_select(txt, "nat", selection = "keep", valuetype = "regex",
                    case_insensitive = FALSE),
        char_keep(txt, "nat", valuetype = "regex", case_insensitive = FALSE)
    )
})

test_that("char_select works with edge cases", {
    txt <- c(c1 = "aa", c2 = "ab", c3 = "aa", c4 = "bcd", c5 = "bcd")

    expect_identical(char_select(txt, "x*"), NULL)
    expect_identical(char_select(txt, "x*", selection = "remove"), txt)
    
    expect_identical(char_select(txt, c("aa*", "x*")), c(c1 = "aa", c3 = "aa"))
    expect_identical(char_remove(txt, c("aa*", "x*")), 
                     c(c2 = "ab", c4 = "bcd", c5 = "bcd"))
    
    expect_identical(char_remove(txt, "*"), character())
    expect_identical(char_remove(unname(txt), "*"), character())
    
    expect_identical(char_keep(txt, "*"), txt)
})

test_that("char_select works with list pattern", {
    txt <- c(c1 = "aa", c2 = "ab", c3 = "aa", c4 = "bcd", c5 = "bcd")
    
    expect_identical(
        char_keep(txt, list("x*", "b*")), 
        c(c4 = "bcd", c5 = "bcd")
    )
    expect_identical(
        char_remove(txt, list("x*", "b*")), 
        c(c1 = "aa", c2 = "ab", c3 = "aa")
    )
})

test_that("char_select works with dictionary pattern", {
    txt <- c(c1 = "aa", c2 = "ab", c3 = "aa", c4 = "bcd", c5 = "bcd")
    patt <- dictionary(list(one = "x*", two = "b*"))
    expect_identical(
        char_keep(txt, patt),
        c(c4 = "bcd", c5 = "bcd")
    )
    expect_identical(
        char_remove(txt, patt), 
        c(c1 = "aa", c2 = "ab", c3 = "aa")
    )
})

test_that("char_select works with collocations pattern", {
    txt <- c(c1 = "aa bb", c2 = "ab", c3 = "aa bc", c4 = "bcd", c5 = "bcd")
    patt <- textstat_collocations("aa bb aa bb aa bc bcd", min_count = 1)
    expect_identical(
        char_keep(txt, patt),
        c(c1 = "aa bb", c3 = "aa bc")
    )
    expect_identical(
        char_remove(txt, patt), 
        c(c2 = "ab", c4 = "bcd", c5 = "bcd")
    )
})
