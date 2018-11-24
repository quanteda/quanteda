context("test tokens_replace")

txt <- c(doc1 = "aa bb BB cc DD ee",
         doc2 = "aa bb cc DD ee")
toks_test <- tokens(txt)
type_test <- types(toks)

test_that("tokens_replace works with regular pattern and replacement", {
    
    # equivalent to tokens conversion method
    expect_equal(tokens_replace(toks_test, type_test, char_toupper(type_test), "fixed", case_insensitive = FALSE),
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
    
    expect_error(tokens_replace(toks_test, c(1, 2), c(10, 20)), 
                 "'pattern' and 'replacement' must be characters")
    
    # does nothing when input vector is zero length
    expect_equal(tokens_replace(toks_test, character(), character()),
                 toks_test)
})

test_that("test tokens_replace works with dictionary", {
    
    txt <- c(doc1 = "aa bb a BB cc DD ee",
             doc2 = "AA bb cc b DD ee")
    toks <- tokens(txt)
    dict1 <- dictionary(list(A = c('a', 'aa'), B = c('b', 'bb')))
    dict2 <- dictionary(list(AB = c('a* b*'), BC = c('b* c*')))
    
    expect_equal(as.list(tokens_replace(toks, dict1, case_insensitive = TRUE)),
                 list(doc1 = c("A", "B", "A", "B", "cc", "DD", "ee"), 
                      doc2 = c("A", "B", "cc", "B", "DD", "ee")))
    
    expect_equal(as.list(tokens_replace(toks, dict1, case_insensitive = FALSE)),
                 list(doc1 = c("A", "B", "A", "BB", "cc", "DD", "ee"), 
                      doc2 = c("AA", "B", "cc", "B", "DD", "ee")))
    
    expect_error(tokens_replace(toks, dict1, c('a')),
                 "'replacement' must be NULL when 'pattern' is a dictionary")
    
    expect_equal(as.list(tokens_replace(toks, dict2, case_insensitive = TRUE)),
                 list(doc1 = c("AB", "AB", "cc", "DD", "ee"), 
                      doc2 = c("AB", "cc", "b", "DD", "ee")))
    
    expect_equal(as.list(tokens_replace(toks, dict1, case_insensitive = FALSE)),
                 list(doc1 = c("A", "B", "A", "BB", "cc", "DD", "ee"), 
                      doc2 = c("AA", "B", "cc", "B", "DD", "ee")))
    
    expect_error(tokens_replace(toks, dict1, c('a')),
                 "'replacement' must be NULL when 'pattern' is a dictionary")
    
})

