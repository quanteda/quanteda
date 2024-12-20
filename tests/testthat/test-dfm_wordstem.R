
test_that("dfm_wordstem works with zero features and zero docs", {
    # zero feature documents
    dfmt1 <- dfm(tokens(c("one", "0"), remove_numbers = TRUE)) |>
        dfm_wordstem()
    dfmt2 <- dfm(tokens(c("one", "!!"), remove_punct = TRUE)) |>
        dfm_wordstem()
    expect_equal(ndoc(dfmt1), ndoc(dfmt2), 2)

    # features with zero docfreq
    dfmt3 <- dfm(tokens(c("stemming porter three", "stemming four five")))
    dfmt3[2, 4] <- 0
    dfmt3 <- as.dfm(dfmt3)
    dfm_wordstem(dfmt3, language = "english")
    expect_equal(nfeat(dfm_wordstem(dfmt3)), 5)
})


test_that("dfm_wordstem works with unigrams", {
    txt <- c(d1 = "stemming stems plurals perfectly",
             d2 = "one two three")
    toks <- tokens(txt)
    dfmat <- dfm(toks)
    expect_equal(featnames(dfm_wordstem(dfmat, language = "porter")),
                 c("stem", "plural", "perfectli", "on", "two", "three"))
})

test_that("dfm_wordstem works with ngrams", {
    txt <- c(d1 = "stemming stems stemmed plurals perfectly",
             d2 = "one two three")
    dfmat <- tokens(txt) |> tokens_ngrams(n = 2) |> dfm()
    dfmat_stemmed <- dfm_wordstem(dfmat, language = "english")
    expect_equal(sort(featnames(dfmat_stemmed)),
                 c("one_two", "plural_perfect", "stem_plural", "stem_stem", "two_three"))
    expect_identical(
        meta(dfmat, "ngram", "object"),
        meta(dfmat_stemmed, "ngram", "object")
    )
    expect_identical(
        meta(dfmat, "concatenator", "object"),
        meta(dfmat_stemmed, "concatenator", "object")
    )
})

test_that("dfm_wordstem() works with verbose", {
    dfmat <- dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE)
    expect_message(
        dfm_tolower(dfmat, verbose = TRUE),
        "",
        fixed = TRUE
    )
    expect_message(
        dfm_toupper(dfmat, verbose = TRUE),
        "",
        fixed = TRUE
    )
})

test_that("dfm_wordstem() works with verbose", {
    dfmat <- dfm(tokens(c("win", "winning", "wins", "won", "winner")), tolower = FALSE)
    expect_message(
        dfm_wordstem(dfmat, verbose = TRUE),
        "dfm_wordstem() changed from 5 features (5 documents) to 3 features (5 documents)",
        fixed = TRUE
    )
})
