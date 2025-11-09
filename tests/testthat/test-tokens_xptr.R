skip_on_cran()

toks <- tokens(data_corpus_inaugural)
xtoks <- as.tokens_xptr(toks)

test_that("Basic functions work", {
    expect_true(is.tokens_xptr(tokens("a b c d", xptr = TRUE)))
})

test_that("Basic functions work", {
    
    expect_false(is.tokens_xptr(toks))
    expect_true(is.tokens_xptr(xtoks))
    
    expect_identical(docnames(xtoks), docnames(toks))
    expect_identical(docid(xtoks), docid(toks))
    expect_identical(segid(xtoks), segid(toks))
    
    expect_identical(ndoc(xtoks), ndoc(toks))
    expect_identical(ntoken(xtoks), ntoken(toks))

    expect_identical(types(xtoks), types(toks))
    expect_identical(concatenator(xtoks), concatenator(toks))
    expect_identical(concat(xtoks), concat(toks))
    expect_identical(ntype(xtoks), ntype(toks))
    expect_warning(
        ntoken(xtoks, xxx = TRUE),
        "xxx argument is not used"
    )
    xtoks2 <- tokens_remove(as.tokens_xptr(xtoks), 
                            min_nchar = 2, padding = TRUE)
    toks2 <- tokens_remove(toks, min_nchar = 2, padding = TRUE)
    expect_identical(ntype(xtoks2), ntype(toks2))
    expect_identical(ntoken(xtoks2, remove_padding = TRUE), 
                     ntoken(tokens_remove(toks2, "")))
})

test_that("attributes are the same", {
    expect_identical(attr(toks, "docvars"), attr(xtoks, "docvars"))
    expect_identical(attr(toks, "meta"), attr(xtoks, "meta"))
})

test_that("subsetting works", {
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

test_that("extractor works", {
    
    expect_identical(xtoks[[integer()]], toks[[integer()]])
    expect_identical(xtoks[[10]], toks[[10]])
    expect_identical(xtoks[[10:20]], toks[[10:20]])
    
})

test_that("deep copy xtokens", {
    expect_identical(
        as.tokens(as.tokens_xptr(xtoks)),
        as.tokens(xtoks)
    )
})

test_that("c works on xtokens", {
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
        as.list(as.tokens(tokens_subset(as.tokens_xptr(toks), Party == "Republican"))),
        as.list(tokens_subset(toks, Party == "Republican"))
    )
    expect_identical(
        as.list(as.tokens(tokens_select(as.tokens_xptr(toks), stopwords("en")))),
        as.list(tokens_select(toks, stopwords("en")))
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
    
    toks2 <- tokens_remove(toks, stopwords(), padding = TRUE) |> 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    xtoks2 <- as.tokens_xptr(toks) |> 
        tokens_remove(stopwords(), padding = TRUE) |> 
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
                     list(recompiled = FALSE, padded = FALSE))
    
    xtoks2 <- tokens_lookup(as.tokens_xptr(toks), dict, nomatch = "nomatch")
    expect_identical(quanteda:::cpp_get_attributes(xtoks2),
                     list(recompiled = TRUE, padded = FALSE))
    
    # attributes are copied
    xtoks3 <- as.tokens_xptr(xtoks2)
    expect_identical(quanteda:::cpp_get_attributes(xtoks3),
                     list(recompiled = TRUE, padded = FALSE))
})

test_that("tokens_subset works", {

    expect_equal(
        tokens_subset(xtoks, 1000 <= ntoken(xtoks)),
        tokens_subset(xtoks, min_ntoken = 1000)
    )
    
    expect_equal(
        tokens_subset(xtoks, ntoken(xtoks) <= 3000),
        tokens_subset(xtoks, max_ntoken = 3000)
    )
    
    expect_equal(
        tokens_subset(xtoks, Year > 2000 & 1000 <= ntoken(xtoks) & ntoken(xtoks) >= 1000),
        tokens_subset(xtoks, Year > 2000, min_ntoken = 1000, max_ntoken = 3000)
    )

})

test_that("all the meta fields are copied", {
    
    toks_dict <- tokens_lookup(toks, data_dictionary_LSD2015[1:2])
    xtoks_dict <- as.tokens_xptr(toks_dict)
    expect_identical(attr(toks_dict, "meta"), attr(xtoks_dict, "meta"))
    
    toks_ngram <- tokens_ngrams(toks)
    xtoks_ngram <- as.tokens_xptr(toks_ngram)
    expect_identical(attr(toks_ngram, "meta"), attr(xtoks_ngram, "meta"))
    
})

test_that("attributes are correct", {
    dict <- data_dictionary_LSD2015[1:2]
    
    xtoks2 <- tokens_remove(as.tokens_xptr(toks), stopwords(), padding = TRUE)
    expect_false(quanteda:::cpp_get_attributes(xtoks2)$recompiled)
    expect_true(quanteda:::cpp_get_attributes(xtoks2)$padded)
    
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

test_that("cpp_serialize is working", {
    
    lis <- as.list(toks)
    out1 <- quanteda:::cpp_as_list(quanteda:::cpp_serialize(lis))
    out2 <- quanteda:::serialize_tokens(lis)
    
    expect_equal(
        lapply(unclass(out1), function(x) attr(out1, "types")[x]),
        lapply(unname(out2), function(x) attr(out2, "types")[x])
    )
})

test_that("cpp_get_freq is working", {
    
    txt <- c("a b c d !", "a ? b c a b . c")
    
    xtoks <- tokens(txt, remove_punct = TRUE, padding = TRUE, xptr = TRUE)
    expect_equal(
        quanteda:::cpp_get_freq(xtoks),
        structure(c(3L, 3L, 3L, 3L, 1L),
                  names = c("", "a", "b", "c", "d"))
    )
    
    expect_equal(
        quanteda:::cpp_get_freq(xtoks, no_padding = TRUE),
        structure(c(3L, 3L, 3L, 1L),
                  names = c("a", "b", "c", "d"))
    )
    
    expect_equal(
        quanteda:::cpp_get_freq(xtoks, boolean = TRUE),
        structure(c(2L, 2L, 2L, 2L, 1L),
                  names = c("", "a", "b", "c", "d"))
    )
    
    expect_equal(
        quanteda:::cpp_get_freq(xtoks, boolean = TRUE, no_padding = TRUE),
        structure(c(2L, 2L, 2L, 1L),
                  names = c("a", "b", "c", "d"))
    )

})

test_that("returns shallow or deep copy x", {
  
  # shallow copy
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks10 <- tokens_select(xtoks, stopwords("en"))
  expect_true(identical(quanteda:::address(xtoks), 
                        quanteda:::address(xtoks10)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks11 <- tokens_compound(xtoks, "and", window = 1)
  expect_true(identical(quanteda:::address(xtoks), 
                        quanteda:::address(xtoks11)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks12 <- tokens_lookup(xtoks, data_dictionary_LSD2015)
  expect_true(identical(quanteda:::address(xtoks), 
                        quanteda:::address(xtoks12)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks13 <- tokens_ngrams(xtoks)
  expect_true(identical(quanteda:::address(xtoks), 
                        quanteda:::address(xtoks13)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks14 <- tokens_replace(xtoks, phrase("fellow citizens"), phrase("fellow Americans"))
  expect_true(identical(quanteda:::address(xtoks), 
                        quanteda:::address(xtoks14)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks15 <- tokens_restore(xtoks)
  expect_true(identical(quanteda:::address(xtoks), 
                        quanteda:::address(xtoks15)))
  
  # deep copy
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks20 <- tokens_group(xtoks)
  expect_false(identical(quanteda:::address(xtoks), 
                         quanteda:::address(xtoks20)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks21 <- tokens_subset(xtoks)
  expect_false(identical(quanteda:::address(xtoks), 
                         quanteda:::address(xtoks21)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks22 <- xtoks[]
  expect_false(identical(quanteda:::address(xtoks), 
                         quanteda:::address(xtoks22)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks23 <- tokens_segment(xtoks, "\\p{P}", valuetype = "regex")
  expect_false(identical(quanteda:::address(xtoks), 
                         quanteda:::address(xtoks23)))
  
  xtoks <- as.tokens_xptr(toks[1:10])
  xtoks24 <- tokens_chunk(xtoks, size = 1000)
  expect_false(identical(quanteda:::address(xtoks), 
                         quanteda:::address(xtoks24)))
})

test_that("lengths works on tokens xptr objects", {
    expect_identical(
        lengths(toks),
        lengths(xtoks)
    )
    expect_identical(
        lengths(toks, use.names = FALSE),
        lengths(xtoks, use.names = FALSE)
    )
    expect_identical(
        names(lengths(xtoks, use.names = FALSE)),
        names(1:5),
        NULL
    )
})

test_that("test low-level validation", {
    
    xtoks <- tokens("a b c", xptr = TRUE)
    
    # integer patterns
    dict <- list(c(1L, 2L))
    expect_error(
        quanteda:::cpp_tokens_select(as.tokens_xptr(xtoks), 
                                     dict, 2, TRUE, 0, 0, c(1, 1), 3, FALSE),
        "Invalid pos_from"
    )
    expect_error(
        quanteda:::cpp_tokens_select(as.tokens_xptr(xtoks), 
                                     dict, 2, TRUE, 0, 0, 1, c(3, 3), FALSE),
        "Invalid pos_to"
    )
    expect_error(
        quanteda:::cpp_tokens_select(as.tokens_xptr(xtoks), 
                                     dict, 2, TRUE, 0, 0, 1, 3, c(FALSE, TRUE)),
        "Invalid bypass"
    )
    expect_error(
        quanteda:::cpp_tokens_compound(as.tokens_xptr(xtoks), 
                                      dict, "-", TRUE, FALSE, 0, 0, c(FALSE, TRUE)),
        "Invalid bypass"
    )
    expect_error(
        quanteda:::cpp_tokens_replace(as.tokens_xptr(xtoks), 
                                      dict, list(c(2, 3)), c(FALSE, TRUE)),
        "Invalid bypass"
    )
    expect_error(
        quanteda:::cpp_tokens_lookup(as.tokens_xptr(xtoks), 
                                     dict, 1, "A", 1, 1, c(FALSE, TRUE)),
        "Invalid bypass"
    )
    expect_error(
        quanteda:::cpp_tokens_lookup(as.tokens_xptr(xtoks), 
                                     dict, c(1, 2), "A", 1, 1, c(FALSE)),
        "Invalid words and keys"
    )
    
    expect_error(
        quanteda:::cpp_subset(as.tokens_xptr(xtoks), c(TRUE, FALSE)),
        "Invalid document index"
    )
    
    expect_error(
        quanteda:::cpp_tokens_group(as.tokens_xptr(xtoks), 2),
        "Invalid groups"
    )
    
    expect_error(
        quanteda:::cpp_kwic(as.tokens_xptr(xtoks), c(1, 2), c(1, 1), c(1, 1), 2),
        "Invalid documents"
    )
    
    expect_error(
        quanteda:::cpp_kwic(as.tokens_xptr(xtoks), 1, c(1, 1), 2, 2),
        "Invalid pos_from"
    )
    
    expect_error(
        quanteda:::cpp_kwic(as.tokens_xptr(xtoks), 1, 1, c(2, 2), 2),
        "Invalid pos_to"
    )
    
    expect_error(
        quanteda:::cpp_kwic(as.tokens_xptr(xtoks), 1, 3, 1, 2),
        "Invalid index"
    )
    
    # not-integer patterns
    expect_error(
        quanteda:::cpp_tokens_select(as.tokens_xptr(xtoks), 
                                     list(c(1, 2)), 
                                     2, TRUE, 0, 0, 1, 3, FALSE),
        "Invalid patterns"
    )
    
    # negative patterns
    expect_silent(
        quanteda:::cpp_tokens_select(as.tokens_xptr(xtoks), 
                                     list(c(1L, -1L)), 
                                     2, TRUE, 0, 0, 1, 3, FALSE)
    )
    
    # NA in patterns
    expect_silent(
        quanteda:::cpp_tokens_select(as.tokens_xptr(xtoks), 
                                     list(c(1L, NA_integer_)), 
                                     2, TRUE, 0, 0, 1, 3, FALSE)
    )
    
})

