context("test tokens_replace")

txt <- c(doc1 = "aa bb BB cc DD ee",
         doc2 = "aa bb cc DD ee")
toks_test <- tokens(txt)

test_that("tokens_replace works with regular pattern and replacement", {
    
    # equivalent to tokens conversion method
    expect_equal(tokens_replace(toks_test, types(toks_test), char_toupper(types(toks_test)), "fixed", case_insensitive = FALSE),
                 tokens_toupper(toks_test))
    
    # fixed, case-insensitive
    expect_equal(as.list(tokens_replace(toks_test, c('aa', 'bb'), c('a', 'b'), "fixed", case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "b", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # fixed, case-sensitive
    expect_equal(as.list(tokens_replace(toks_test, c('aa', 'bb'), c('a', 'b'), "fixed", case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # fixed, duplicated types in from
    expect_equal(as.list(tokens_replace(toks_test, c('aa', 'aa'), c('a', 'aaa'), "fixed", case_insensitive = FALSE)),
                 list(doc1 = c("a", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "bb", "cc", "DD", "ee")))
    
    # fixed, no match
    expect_equal(as.list(tokens_replace(toks_test, "x", "y", "fixed", case_insensitive = FALSE)),
                 as.list(toks_test))
    
    # glob, case-insensitive
    expect_equal(as.list(tokens_replace(toks_test, c('a*', 'b*'), c('a', 'b'), "glob", case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "b", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # glob, case-sensitive
    expect_equal(as.list(tokens_replace(toks_test, c('a*', 'b*'), c('a', 'b'), "glob", case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # glob, duplicated types in from
    expect_equal(as.list(tokens_replace(toks_test, c('a*', 'a*'), c('a', 'aaa'), "glob", case_insensitive = FALSE)),
                 list(doc1 = c("a", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "bb", "cc", "DD", "ee")))
    
    # glob, no match
    expect_equal(as.list(tokens_replace(toks_test, "x*", "y", "glob", case_insensitive = FALSE)),
                 as.list(toks_test))
    
    # regex, case-insensitive
    expect_equal(as.list(tokens_replace(toks_test, c('a.', 'b.'), c('a', 'b'), "regex", case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "b", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # regex, case-sensitive
    expect_equal(as.list(tokens_replace(toks_test, c('a.', 'b.'), c('a', 'b'), "regex", case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # regex, duplicated types in from
    expect_equal(as.list(tokens_replace(toks_test, c('a.', 'a.'), c('a', 'aaa'), "regex", case_insensitive = FALSE)),
                 list(doc1 = c("a", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "bb", "cc", "DD", "ee")))
    
    # regex, no match
    expect_equal(as.list(tokens_replace(toks_test, "x.*", "y", "regex", case_insensitive = FALSE)),
                 as.list(toks_test))
    
    
})

test_that("tokens_replace works with pharsal pattern and replacement", {

    # fixed, case-insensitive
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('aa bb')), phrase(c('a b')), "fixed", 
                                        case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # fixed, single, case-sensitive
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('AA BB')), phrase(c('a b')), "fixed", 
                                        case_insensitive = FALSE)),
                 list(doc1 = c("aa", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("aa", "bb", "cc", "DD", "ee")))
    
    # fixed, single, duplicated types in from
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('aa bb', 'aa bb')), phrase(c('a b', 'aaa bbb')), "fixed", 
                                        case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # glob, case-insensitive
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('aa *')), phrase(c('a b')), "glob", case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # glob, case-sensitive
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('AA *')), phrase(c('a b')), "glob", case_insensitive = FALSE)),
                 list(doc1 = c("aa", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("aa", "bb", "cc", "DD", "ee")))
    
    # glob, duplicated types in from
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('aa *', 'aa bb')), phrase(c('a b', 'aaa bbb')), "glob", 
                                        case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # regex, case-insensitive
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('aa .*')), phrase(c('a b')), "regex", case_insensitive = TRUE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
    
    # regex, case-sensitive
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('AA .*')), phrase(c('a b')), "regex", case_insensitive = FALSE)),
                 list(doc1 = c("aa", "bb", "BB", "cc", "DD", "ee"), 
                      doc2 = c("aa", "bb", "cc", "DD", "ee")))
    
    # regex, duplicated types in from
    expect_equal(as.list(tokens_replace(toks_test, phrase(c('aa .*', 'aa bb')), phrase(c('a b', 'aaa bbb')), "regex", 
                                        case_insensitive = FALSE)),
                 list(doc1 = c("a", "b", "BB", "cc", "DD", "ee"), 
                      doc2 = c("a", "b", "cc", "DD", "ee")))
})

test_that("tokens_replace can reconstruct original by re-replacement", {
    toks <- tokens(data_corpus_inaugural)
    
    toks_temp1 <- tokens_replace(toks, "it", "ZZZZZZZ", case_insensitive = FALSE)
    expect_equal(as.list(tokens_replace(toks_temp1, "ZZZZZZZ", "it")),
                 as.list(toks))
    
    toks_temp2 <- tokens_replace(toks, phrase("it is"), phrase("XXXXXXX YYYYYYY ZZZZZZZ"), case_insensitive = FALSE)
    expect_equal(as.list(tokens_replace(toks_temp2, phrase("XXXXXXX YYYYYYY ZZZZZZZ"), phrase("it is"))),
                 as.list(toks))
})


test_that("tokens_replace raises error when input values are invalid", {
    # error when lenfths of from and to are different
    expect_error(tokens_replace(toks_test, c('aa', 'bb'), c('a')),
                 "Lengths of 'pattern' and 'replacement' must be the same")
    
    expect_error(tokens_replace(toks_test, c(1, 2), c(10, 20), valuetype = "fixed"),
                 "'pattern' and 'replacement' must be characters")
    
    # does nothing when input vector is zero length
    expect_equal(tokens_replace(toks_test, character(), character()),
                 toks_test)
})

test_that("tokens_replace remove tokens when replacement is empty", {
    
    pat1 <- list(c("aa", "bb"), c("dd", "ee"))
    rep1 <- list(character(), c("zz", "zz"))
    pat2 <- list(c("aa", "bb"), c("bb", "cc"))
    rep2 <- list(c("zz", "zz"), character())
    
    expect_equal(as.list(tokens_replace(toks_test, pat1, rep1)),
                 list(doc1 = c("BB", "cc", "zz", "zz"), 
                      doc2 = c("cc","zz", "zz")))
    expect_equal(as.list(tokens_replace(toks_test, pat2, rep2)),
                 list(doc1 = c("zz", "zz", "DD", "ee"), 
                      doc2 = c("zz","zz", "DD", "ee")))
                 
})

test_that("tokens_replace works with same replacement characters (#1765)", {
    
    exp1 <- list(doc1 = c("bb", "bb", "BB", "bb", "DD", "ee"), 
                 doc2 = c("bb", "bb", "bb", "DD", "ee"))
    expect_equal(
        as.list(tokens_replace(toks_test, c("aa", "cc"), c("bb", "bb"), valuetype = "fixed")),
        exp1
    )
    expect_equal(
        as.list(tokens_replace(toks_test, c("aa", "cc"), c("bb", "bb"), valuetype = "glob")),
        exp1
    )
    expect_equal(
        as.list(tokens_replace(toks_test, c("a.*", "cc"), c("bb", "bb"), valuetype = "regex")),
        exp1
    )
    
    exp2 <- list(doc1 = c("bb", "BB", "bb", "ee"), 
                 doc2 = c("bb", "bb", "ee"))
    
    expect_equal(
        as.list(tokens_replace(toks_test, list(c("aa", "bb"), c("cc", "DD")), 
                   c("bb", "bb"), valuetype = "fixed")),
        exp2
    )
    expect_equal(
        as.list(tokens_replace(toks_test, list(c("aa", "bb"), c("cc", "DD")), 
                   c("bb", "bb"), valuetype = "glob")),
        exp2
    )
    expect_equal(
        as.list(tokens_replace(toks_test, list(c("a.*", "bb"), c("cc", "DD")), 
                   c("bb", "bb"), valuetype = "regex")),
        exp2
    )
    
    exp3 <- list(doc1 = c("zz", "bb", "BB", "zz", "DD", "ee"), 
                 doc2 = c("zz", "bb", "zz", "DD", "ee"))
    
    expect_equal(
        as.list(tokens_replace(toks_test, c("aa", "cc"), c("zz", "zz"), valuetype = "fixed")),
        exp3
    )
    expect_equal(
        as.list(tokens_replace(toks_test, c("aa", "cc"), c("zz", "zz"), valuetype = "glob")),
        exp3
    )
    expect_equal(
        as.list(tokens_replace(toks_test, c("a.*", "cc"), c("zz", "zz"), valuetype = "regex")),
        exp3
    )
    
    exp4 <- list(doc1 = c("zz", "BB", "zz", "ee"), 
                 doc2 = c("zz", "zz", "ee"))

    expect_equal(
        as.list(tokens_replace(toks_test, list(c("aa", "bb"), c("cc", "DD")), 
                       c("zz", "zz"), valuetype = "glob")),
        exp4
    )
    expect_equal(
        as.list(tokens_replace(toks_test, list(c("aa", "bb"), c("cc", "DD")), 
                       c("zz", "zz"), valuetype = "glob")),
        exp4
    )
    expect_equal(
        as.list(tokens_replace(toks_test, list(c("a.*", "bb"), c("cc", "DD")), 
                       c("zz", "zz"), valuetype = "regex")),
        exp4
    )
})
