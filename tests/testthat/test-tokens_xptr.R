require(quanteda)
require(testthat)

toks <- tokens(data_corpus_inaugural)
xtoks <- as.tokens_xptr(toks)

test_that("Basic functions work", {
    expect_true(is.tokens_xptr(tokens("a b c d", xptr = TRUE)))
})

test_that("Basic functions work", {
    
    expect_false(is.tokens_xptr(toks))
    expect_true(is.tokens_xptr(xtoks))
    
    expect_identical(docnames(toks), docnames(xtoks))
    expect_identical(docid(toks), docid(xtoks))
    expect_identical(segid(toks), segid(xtoks))
    
    expect_identical(ndoc(toks), ndoc(xtoks))
    expect_identical(ntoken(toks), ntoken(xtoks))
    expect_identical(types(toks), types(xtoks))
})

test_that("attributes are the same", {
    expect_identical(attr(toks, "docvars"), attr(xtoks, "docvars"))
    expect_identical(attr(toks, "meta"), attr(xtoks, "meta"))
})

test_that("subsetting work", {
    expect_identical(docnames(as.tokens_xptr(toks)[2:6]),
                     docnames(toks)[2:6])
    expect_identical(docnames(as.tokens_xptr(toks)[2:6 * -1]),
                     docnames(toks)[2:6 * -1])
    
    expect_identical(docnames(head(as.tokens_xptr(toks))), 
                     docnames(head(toks)))
    expect_identical(docnames(head(as.tokens_xptr(toks), 0)), 
                     docnames(head(toks, 0)))
    expect_identical(docnames(head(as.tokens_xptr(toks), 100)), 
                     docnames(head(toks, 100)))
    expect_identical(docnames(tail(as.tokens_xptr(toks))), 
                     docnames(tail(toks)))
    expect_identical(docnames(tail(as.tokens_xptr(toks), -10)), 
                     docnames(tail(toks, -10)))
})

test_that("deep copy xtokens", {
    xtoks_copy <- as.tokens_xptr(xtoks)
    expect_identical(as.tokens(xtoks_copy), 
                     as.tokens(xtoks))
})

test_that("c words on xtokens", {
    xtoks_pad <- tokens_remove(as.tokens_xptr(toks), stopwords(), padding = TRUE)
    xtoks1 <- as.tokens_xptr(xtoks_pad)[1:10]
    xtoks2 <- as.tokens_xptr(xtoks_pad)[11:20]
    
    expect_identical(as.list(c(xtoks1, xtoks2)), 
                     as.list(as.tokens_xptr(xtoks_pad)[1:20]))
    
    expect_error(
        c(xtoks_pad, list()),
        "Cannot combine different types of objects"
    )
})

test_that("operations on copied xtokens do not affect the original xtokens", {
    expect_identical(
        as.tokens(tokens_subset(as.tokens_xptr(toks), Party == "Republican")),
        tokens_subset(toks, Party == "Republican")
    )
    expect_identical(
        as.tokens(tokens_select(as.tokens_xptr(toks), stopwords("en"))),
        tokens_select(toks, stopwords("en"))
    )
    expect_identical(
        as.list(as.tokens(tokens_ngrams(as.tokens_xptr(toks)))),
        as.list(tokens_ngrams(toks))
    )
})

test_that("operations on copied xtokens do not affect the original xtokens", {
    xtoks <- as.tokens_xptr(toks)
    xtoks_copy <- as.tokens_xptr(xtoks)
    xtoks_copy <- tokens_remove(xtoks_copy, stopwords(), padding = TRUE)
    expect_false(identical(as.list(xtoks_copy), 
                           as.list(xtoks)))
    expect_identical(as.list(toks), 
                     as.list(xtoks))
})

test_that("tokens_select and tokens_remove work", {
    
    toks2 <- tokens_remove(toks, stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    xtoks2 <- as.tokens_xptr(toks) %>% 
        tokens_remove(stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    expect_identical(as.list(xtoks2), as.list(toks2))
    
})

test_that("tokens_tolower and tokens_toupper work", {
    expect_identical(as.list(as.tokens(tokens_tolower(as.tokens_xptr(toks)))),
                     as.list(as.tokens(tokens_tolower(xtoks))))
    expect_identical(as.list(as.tokens(tokens_toupper(as.tokens_xptr(toks)))),
                     as.list(as.tokens(tokens_toupper(xtoks))))
})

test_that("tokens_tolower and tokens_toupper work", {
    dict <- data_dictionary_LSD2015[1:2]
    expect_identical(as.tokens(tokens_lookup(as.tokens_xptr(toks), dict)),
                     tokens_lookup(toks, dict))
    
    xtoks1 <- tokens_lookup(as.tokens_xptr(toks), dict, exclusive = FALSE)
    expect_identical(quanteda:::cpp_get_attributes(xtoks1),
                     list(recompiled = FALSE))
    
    xtoks2 <- tokens_lookup(as.tokens_xptr(toks), dict, nomatch = "nomatch")
    expect_identical(quanteda:::cpp_get_attributes(xtoks2),
                     list(recompiled = TRUE))
    
    # attributes are copied
    xtoks3 <- as.tokens_xptr(xtoks2)
    expect_identical(quanteda:::cpp_get_attributes(xtoks3),
                     list(recompiled = TRUE))
})

test_that("all the meta fields are copied", {
    
    toks_dict <- tokens_lookup(toks, data_dictionary_LSD2015[1:2])
    xtoks_dict <- as.tokens_xptr(toks_dict)
    expect_identical(attr(toks_dict, "meta"), attr(xtoks_dict, "meta"))
    
    toks_ngram <- tokens_ngrams(toks)
    xtoks_ngram <- as.tokens_xptr(toks_ngram)
    expect_identical(attr(toks_ngram, "meta"), attr(xtoks_ngram, "meta"))
    
})

test_that("recompile flag is correct", {
    dict <- data_dictionary_LSD2015[1:2]
    xtoks_dict1 <- tokens_lookup(as.tokens_xptr(toks), 
                                 dict, exclusive = TRUE)
    expect_true(quanteda:::cpp_get_attributes(xtoks_dict1)$recompiled)

    xtoks_dict2 <- tokens_lookup(as.tokens_xptr(toks), 
                                 dict, exclusive = FALSE)
    expect_false(quanteda:::cpp_get_attributes(xtoks_dict2)$recompiled)
})

test_that("tokens_compound works", {
    dict <- data_dictionary_LSD2015[3:4]
    expect_identical(as.list(tokens_compound(as.tokens_xptr(toks), dict)),
                     as.list(tokens_compound(toks, dict)))
    expect_identical(as.list(tokens_compound(as.tokens_xptr(toks), phrase("of the"))),
                     as.list(tokens_compound(toks, phrase("of the"))))
})

test_that("tokens_chunk() works", {
    
    expect_identical(as.tokens(tokens_chunk(as.tokens_xptr(toks), 10)),
                     tokens_chunk(toks, 10))
})

test_that("tokens_replace() and tokens_split() work", {
    
    pat <- phrase(c("Supreme Court"))
    rep <- phrase(c("Supreme Court of the United States"))
    
    expect_identical(as.tokens(tokens_replace(as.tokens_xptr(toks), pat, rep)),
                     tokens_replace(toks, pat, rep))
    
    expect_identical(as.tokens(tokens_split(as.tokens_xptr(toks), "-")),
                     tokens_split(toks, "-"))
})

test_that("tokens_sample() works", {
    
    set.seed(1234)
    toks1 <- as.tokens(tokens_sample(as.tokens_xptr(toks), 10))
    set.seed(1234)
    toks2 <- tokens_sample(toks, 10)
    expect_identical(toks1, toks2)
})


test_that("dfm works", {
    
    xtoks <- as.tokens_xptr(toks)
    expect_identical(dfm(xtoks, tolower = TRUE), 
                     dfm(toks, tolower = TRUE))
    expect_identical(dfm(xtoks, tolower = FALSE), 
                     dfm(toks, tolower = FALSE))
    expect_false(any(duplicated(colnames(dfm(xtoks)))))
    
    # with padding
    toks_pad <- tokens_remove(toks, stopwords(), padding = TRUE)
    expect_identical(dfm(as.tokens_xptr(toks_pad)), dfm(toks_pad))
    expect_identical(dfm(as.tokens_xptr(toks_pad), tolower = FALSE), 
                     dfm(toks_pad, tolower = FALSE))
    
    # with dictionary keys
    dict <- data_dictionary_LSD2015
    expect_equal(
        featnames(dfm(tokens_lookup(as.tokens_xptr(toks), dict))),
        names(dict)
    )
    expect_equal(
        featnames(dfm(tokens_lookup(as.tokens_xptr(toks), rev(dict)))),
        rev(names(dict))
    )
})

test_that("order of the feature is unique", {
    
    mat <- replicate(5, featnames(dfm(tokens_ngrams(as.tokens_xptr(toks)))))
    expect_true(all(mat[,1] == mat))
    
})

test_that("fcm works", {
    expect_identical(fcm(as.tokens_xptr(toks[1:10])), fcm(toks[1:10]))
    expect_identical(fcm(as.tokens_xptr(toks), window = 5), 
                     fcm(toks, window = 5))
})


