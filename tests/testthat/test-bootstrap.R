context("test bootstrap_dfm")

test_that("bootstrap_dfm works with character and corpus objects", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.",
             texttwo = "Premiere phrase.  Deuxieme phrase.",
             textthree = "Sentence three is really short.")
    corp <- corpus(txt,
                   docvars = data.frame(country = c("UK", "USA", "UK"), 
                                        year = c(1990, 2000, 2005)))
    set.seed(10)
    bs1 <- bootstrap_dfm(corp, n = 10, verbose = TRUE)
    # print(bs1[[1]], -1, -1)
    # print(dfm(corp), -1, -1)
    expect_equal(bs1[[1]], dfm(corp))
    
    bs2 <- bootstrap_dfm(txt, n = 10, verbose = TRUE)
    expect_identical(bs2[[1]],
                     dfm(corp, include_docvars = FALSE))
    
    # are feature names of resamples identical?
    expect_identical(
        featnames(bs2[[1]]),
        featnames(bs2[[2]])
    )

    # are document names of resamples identical?
    expect_identical(
        docnames(bs2[[1]]),
        docnames(bs2[[2]])
    )
})

test_that("bootstrap_dfm works as planned with dfm", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.",
             texttwo = "Premiere phrase.  Deuxieme phrase.")
    corp <- corpus(txt, 
                   docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)))
    dfmt <- dfm(corpus_reshape(corp, to = "sentences"))
    
    set.seed(10)
    bs1 <- bootstrap_dfm(dfmt, n = 3, verbose = FALSE)
    expect_equivalent(bs1[[1]], 
                      dfm(corp))
    
    bs2 <- bootstrap_dfm(txt, n = 3, verbose = FALSE)
    expect_identical(bs2[[1]], 
                     dfm(corp, include_docvars = FALSE))

    # are feature names of resamples identical?
    expect_identical(
        featnames(bs2[[1]]),
        featnames(bs2[[2]])
    )
    # are document names of resamples identical?
    expect_identical(
        docnames(bs2[[1]]),
        docnames(bs2[[2]])
    )
})

test_that("verbose messages work", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.",
             texttwo = "Premiere phrase.  Deuxieme phrase.",
             textthree = "Sentence three is really short.")
    expect_message(
        bootstrap_dfm(txt, n = 1, verbose = TRUE),
        "Segmenting the .+ into sentences"
    )
    expect_message(
        bootstrap_dfm(txt, n = 1, verbose = TRUE),
        "resampling and forming dfms: 0"
    )
})
