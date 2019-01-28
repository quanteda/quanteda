context("test bootstrapping functions")

test_that("bootstrap_dfm works with character and corpus objects", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.",
             texttwo = "Premiere phrase.  Deuxieme phrase.",
             textthree = "Sentence three is really short.")
    corp <- corpus(txt,
                   docvars = data.frame(country = c("UK", "USA", "UK"),
                                        year = c(1990, 2000, 2005)),
                   metacorpus = list(notes = "Example showing how corpus_reshape() works."))
    set.seed(10)

    dfmatresamp1 <- bootstrap_dfm(corp, n = 10)
    expect_equal(dfmatresamp1[[1]], dfm(corp))

    dfmatresamp2 <- bootstrap_dfm(txt, n = 10)
    expect_identical(dfmatresamp2[[1]],
                     dfm(corp, include_docvars = FALSE))


    # are feature names of resamples identical?
    expect_identical(
        featnames(dfmatresamp2[[1]]),
        featnames(dfmatresamp2[[2]])
    )

    # check that all documents have at least one sentence
    L <- lapply(dfmatresamp1, as.matrix)
    arrayL <- array(unlist(L), dim = c(nrow(L[[1]]), ncol(L[[1]]), length(L)))
    docsums <- apply(arrayL, c(1, 3), sum)
    expect_true(all(docsums[c(2, 3), ] >= 6))
})

test_that("bootstrap_dfm works as planned with dfm", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.",
             texttwo = "Premiere phrase.  Deuxieme phrase.")
    corp <- corpus(txt,
                       docvars = data.frame(country = c("UK", "USA"),
                                            year = c(1990, 2000)),
                       metacorpus = list(notes = "Example showing how corpus_reshape() works."))
    dfmat <- dfm(corpus_reshape(corp, to = "sentences"))

    set.seed(10)
    dfmatresamp1 <- bootstrap_dfm(dfmat, n = 3, verbose = FALSE)
    expect_equivalent(dfmatresamp1[[1]],
                      dfm(corp))

    dfmatresamp2 <- bootstrap_dfm(txt, n = 3, verbose = FALSE)
    expect_identical(dfmatresamp2[[1]],
                     dfm(corp, include_docvars = FALSE))

    # are feature names of resamples identical?
    expect_identical(
        featnames(dfmatresamp2[[1]]),
        featnames(dfmatresamp2[[2]])
    )
    # are document names of resamples identical?
    expect_identical(
        docnames(dfmatresamp2[[1]]),
        docnames(dfmatresamp2[[2]])
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