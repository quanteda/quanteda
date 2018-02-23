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

test_that("as.data.frame for dfm objects", {
    d <- data_dfm_lbgexample[, 1:5]
    expect_equal(
        as.data.frame(d),
        data.frame(document = docnames(d), as.matrix(d), stringsAsFactors = FALSE, row.names = NULL)
    )
    expect_equal(
        as.data.frame(d, document = NULL),
        data.frame(as.matrix(d), stringsAsFactors = FALSE, row.names = NULL)
    )
    expect_equal(
        as.data.frame(d, row.names = docnames(d)),
        data.frame(document = docnames(d), as.matrix(d), stringsAsFactors = FALSE, row.names = docnames(d))
    )
    expect_error(
        as.data.frame(d, document = TRUE),
        "document must be character or NULL"
    )
})

test_that("as.data.frame.dfm handles irregular feature names correctly", {
    skip_on_os("windows")
    skip_on_appveyor()
    mydfm <- dfm(data_char_sampletext, 
                 dictionary = dictionary(list("字" = "a", "spe cial" = "the", 
                                              "飛機" = "if", "spec+ial" = "of")))
    expect_equal(
        names(as.data.frame(mydfm)),
        c("document", "字", "spe cial", "飛機", "spec+ial")
    )
    expect_equal(
        names(as.data.frame(mydfm, check.names = TRUE)),
        c("document", "字", "spe.cial", "飛機", "spec.ial")
    )
})

test_that("as.matrix for dfm objects", {
    d <- data_dfm_lbgexample[1:2, 1:5]
    expect_equal(
        as.matrix(d),
        matrix(c(2, 0, 3, 0, 10, 0, 22, 0, 45, 0), nrow = ndoc(d), 
               dimnames = list(docs = c("R1", "R2"), features = LETTERS[1:5]))
    )
    expect_equal(
        as.matrix(d[1, ]),
        matrix(c(2, 3, 10, 22, 45), nrow = 1,
               dimnames = list(docs = c("R1"), features = LETTERS[1:5]))
    )
})
