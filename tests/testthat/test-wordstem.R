library(quanteda)

test_that("character wordstem test to test testing.", {
    expect_equal(char_wordstem('testing'), 'test')
})


test_that("can wordstem dfms with zero features and zero docs", {
    
    # zero feature documents
    dfm1 <- dfm(c("one", "0"), stem = TRUE, removeNumbers = TRUE)
    dfm2 <- dfm(c("one", "!!"), stem = TRUE, removePunct = TRUE)
    expect_equal(ndoc(dfm1), ndoc(dfm2), 2)
    
    # features with zero docfreq
    mydfm <- dfm(c("stemming porter three", "stemming four five"))
    mydfm[2, 4] <- 0
    mydfm <- new("dfmSparse", mydfm)
    dfm_wordstem(mydfm)
    expect_equal(nfeature(dfm_wordstem(mydfm)), 5)
    
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
    toks <- tokens(txt, ngrams = 2)
    expect_equal(as.list(tokens_wordstem(toks, "english")),
                 list(d1 = c("stem_plural", "plural_perfect"),
                      d2 = c("one_two", "two_three")))
})

test_that("can wordstem dfm with unigrams", {
    txt <- c(d1 = "stemming stems plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt)
    dfmtoks <- dfm(toks)
    expect_equal(featnames(dfm_wordstem(dfmtoks)),
                 c("stem", "plural", "perfectli", "on", "two", "three"))
})

test_that("can wordstem dfm with ngrams", {
    txt <- c(d1 = "stemming stems stemmed plurals perfectly",
             d2 = "one two three")
    dfmtxt <- dfm(txt, ngrams = 2)
    dfmtxt_stemmed <- dfm_wordstem(dfmtxt, language = "english")
    expect_equal(sort(featnames(dfmtxt_stemmed)),
                 c("one_two","plural_perfect","stem_plural", "stem_stem","two_three"))
    expect_equal(dfmtxt@ngrams, dfmtxt_stemmed@ngrams)
    expect_equal(dfmtxt@concatenator, dfmtxt_stemmed@concatenator)
})


