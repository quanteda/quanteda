context("test wordstem")

test_that("character wordstem test to test testing.", {
    expect_equal(char_wordstem("testing", "porter"), "test")
    expect_equal(char_wordstem("testing", "english"), "test")
})

test_that("can wordstem dfms with zero features and zero docs", {
    # zero feature documents
    dfmt1 <- dfm(c("one", "0"), stem = TRUE, remove_numbers = TRUE)
    dfmt2 <- dfm(c("one", "!!"), stem = TRUE, remove_punct = TRUE)
    expect_equal(ndoc(dfmt1), ndoc(dfmt2), 2)
    
    # features with zero docfreq
    dfmt3 <- dfm(c("stemming porter three", "stemming four five"))
    dfmt3[2, 4] <- 0
    dfmt3 <- as.dfm(dfmt3)
    dfm_wordstem(dfmt3, language = "english")
    expect_equal(nfeat(dfm_wordstem(dfmt3)), 5)
})

test_that("can wordstem tokens", {
    txt <- c(d1 = "stemming plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem", "plural", "perfect"),
                      d2 = c("one", "two", "three")))
})

test_that("can wordstem token ngrams", {
    txt <- c(d1 = "stemming plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt) %>% tokens_ngrams(n = 2)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem_plural", "plural_perfect"),
                      d2 = c("one_two", "two_three")))
})

test_that("can wordstem dfm with unigrams", {
    txt <- c(d1 = "stemming stems plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt)
    dfm <- dfm(toks)
    expect_equal(featnames(dfm_wordstem(dfm, language = "porter")),
                 c("stem", "plural", "perfectli", "on", "two", "three"))
})

test_that("can wordstem dfm with ngrams", {
    txt <- c(d1 = "stemming stems stemmed plurals perfectly",
             d2 = "one two three")
    dfmt <- dfm(txt, ngrams = 2)
    dfmt_stemmed <- dfm_wordstem(dfmt, language = "english")
    expect_equal(sort(featnames(dfmt_stemmed)),
                 c("one_two", "plural_perfect", "stem_plural", "stem_stem","two_three"))
    expect_equal(dfmt@meta$object$ngram, 
                 dfmt_stemmed@meta$object$ngram)
    expect_equal(dfmt@meta$object$concatenator, 
                 dfmt_stemmed@meta$object$concatenator)
})

test_that("wordstem works with tokens with padding = TRUE", {
    txt <- c(d1 = "stemming plurals perfectly",
             d2 = "one two three")
    toks <- tokens_remove(tokens(txt), c("one", "three"), padding = TRUE)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem", "plural", "perfect"),
                      d2 = c("", "two", "")))
})

test_that("wordstem works on tokens that include separators (#909)", {
    txt <- "Tests for developers."
    toks <- tokens(txt, remove_punct = TRUE)
    expect_equal(
        as.list(tokens_wordstem(toks, language = "english")),
        list(text1 = c("Test", "for", "develop"))
    )
})
