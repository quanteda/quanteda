context("test as.dfm")

set.seed(19)
elements <- rpois(20, 1)

test_that("as.dfm adds document and feature names when a matrix has none", {
    m <- matrix(elements, nrow = 4)
    expect_equal(
        docnames(as.dfm(m)),
        paste0("text", seq_len(nrow(m)))
    )
    expect_equal(
        featnames(as.dfm(m)),
        paste0("feat", seq_len(ncol(m)))
    )
    expect_equal(
        names(dimnames(as.dfm(m))),
        c("docs", "features")
    )
})

test_that("as.dfm adds names of dimnames when a matrix has none", {
    m <- matrix(elements, nrow = 4)
    dimnames(m) <- list(paste0("text", seq_len(nrow(m))),
                        letters[seq_len(ncol(m))])
    expect_equal(
        docnames(as.dfm(m)),
        paste0("text", seq_len(nrow(m)))
    )
    expect_equal(
        featnames(as.dfm(m)),
        letters[seq_len(ncol(m))]
    )
    expect_equal(
        names(dimnames(as.dfm(m))),
        c("docs", "features")
    )
})

test_that("as.dfm keeps document and feature names from a data.frame", {
    m <- data.frame(matrix(elements, nrow = 4))
    expect_equal(
        docnames(as.dfm(m)),
        as.character(seq_len(nrow(m)))
    )
    expect_equal(
        featnames(as.dfm(m)),
        paste0("X", seq_len(ncol(m)))
    )
    expect_equal(
        names(dimnames(as.dfm(m))),
        c("docs", "features")
    )
})

test_that("as.dfm adds names of dimnames when a data.frame has none", {
    m <- data.frame(matrix(elements, nrow = 4))
    dimnames(m) <- list(paste0("text", seq_len(nrow(m))),
                        letters[seq_len(ncol(m))])
    expect_equal(
        docnames(as.dfm(m)),
        paste0("text", seq_len(nrow(m)))
    )
    expect_equal(
        featnames(as.dfm(m)),
        letters[seq_len(ncol(m))]
    )
    expect_equal(
        names(dimnames(as.dfm(m))),
        c("docs", "features")
    )
})

test_that("is.dfm works as expected", {
    m <- data.frame(matrix(elements, nrow = 4))
    expect_true(is.dfm(as.dfm(m)))
    expect_false(is.dfm(m))
})

test_that("as.dfm for tm matrix objects", {
    txt <- c(docA = "a a a b c c f",
             docB = "a b b b c d",
             docC = "c c c f f")
    skip_if_not_installed("tm")
    dtm <- tm::DocumentTermMatrix(tm::Corpus(tm::VectorSource(txt)),
                                  control = list(wordLengths = c(1, Inf)))
    expect_equal(
        as.dfm(dtm),
        dfm(txt)
    )
    
    tdm <- tm::TermDocumentMatrix(tm::Corpus(tm::VectorSource(txt)),
                                  control = list(wordLengths = c(1, Inf)))
    expect_equal(
        as.dfm(tdm),
        dfm(txt)
    )
})

