test_that("tokens_match works", {
    txt <- c(doc1 = "aa bb BB cc DD ee",
             doc2 = "aa bb cc DD ee")
    toks <- tokens(txt) %>% 
        tokens_tolower()

    toks_conf1 <- tokens_match(toks, c("aa", "zz", "xx", "bb"))
    expect_identical(
        types(toks_conf1),
        c("aa", "zz", "xx", "bb")
    )
    expect_identical(
        docnames(toks_conf1),
        c("doc1", "doc2")
    )

    toks_conf2 <- tokens_match(toks, types(tokens("aa zz xx bb")))
    expect_identical(
        types(toks_conf2),
        c("aa", "zz", "xx", "bb")
    )
    expect_identical(
        docnames(toks_conf2),
        c("doc1", "doc2")
    )
    
    toks_conf3 <- tokens_match(toks, character())
    expect_identical(
        types(toks_conf3), character()
    )
    expect_identical(
        docnames(toks_conf3),
        c("doc1", "doc2")
    )
})

test_that("tokens_match works with padding", {
    
    toks <- tokens("aa bb !", padding = TRUE, remove_punct = TRUE)
    expect_identical(
        types(tokens_match(toks, c("aa", "bb", "cc", ""))),
        c("aa", "bb", "cc", "")
    )
})

test_that("tokens_match coerce non-character feature", {
    txt <- c(doc1 = "TRUE TRUE FALSE",
             doc2 = "1 2 100")
    toks <- tokens(txt) %>% 
        tokens_tolower()
    expect_equal(types(tokens_match(toks, c(TRUE, FALSE))),
                 c("TRUE", "FALSE"))
    expect_equal(types(tokens_match(toks, c(100, 1))),
                 c("100", 1))

})

test_that("tokens_match verbose works", {
    
    toks <- tokens(head(data_corpus_inaugural))
    expect_message(
        tokens_match(toks, c("this", "the", "all", "be"), verbose = TRUE),
        "tokens_match() changed from 9,825 tokens (6 documents) to 865 tokens (6 documents)",
        fixed = TRUE
    )
})
