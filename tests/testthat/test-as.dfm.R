context("test as.dfm")

set.seed(19)
elements <- rpois(20, 1)

test_that("as.dfm adds document and feature names when a matrix has none", {
    m <- matrix(elements, nrow = 4)
    expect_equal(
        docnames(as.dfm(m)),
        paste0("doc", 1:nrow(m))
    )
    expect_equal(
        featnames(as.dfm(m)),
        paste0("feat", 1:ncol(m))
    )
    expect_equal(
        names(dimnames(as.dfm(m))),
        c("docs", "features")
    )
})

test_that("as.dfm adds names of dimnames when a matrix has none", {
    m <- matrix(elements, nrow = 4)
    dimnames(m) <- list(paste0("doc", 1:nrow(m)),
                        letters[1:ncol(m)])
    expect_equal(
        docnames(as.dfm(m)),
        paste0("doc", 1:nrow(m))
    )
    expect_equal(
        featnames(as.dfm(m)),
        letters[1:ncol(m)]
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
        as.character(1:nrow(m))
    )
    expect_equal(
        featnames(as.dfm(m)),
        paste0("X", 1:ncol(m))
    )
    expect_equal(
        names(dimnames(as.dfm(m))),
        c("docs", "features")
    )
})

test_that("as.dfm adds names of dimnames when a data.frame has none", {
    m <- data.frame(matrix(elements, nrow = 4))
    dimnames(m) <- list(paste0("doc", 1:nrow(m)),
                        letters[1:ncol(m)])
    expect_equal(
        docnames(as.dfm(m)),
        paste0("doc", 1:nrow(m))
    )
    expect_equal(
        featnames(as.dfm(m)),
        letters[1:ncol(m)]
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
