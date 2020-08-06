context("test textstat_proxy.R")

test_mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), remove = stopwords("english"),
               stem = TRUE, verbose = FALSE)
test_mt <- dfm_trim(test_mt, min_termfreq = 5)

test_simil <- function(x, method, margin, ignore_upper = FALSE, ...) {

    if (margin == "documents") {
        by_rows <- TRUE
        selection <- "1985-Reagan"
        y <- x[selection, ]
    } else {
        by_rows <- FALSE
        selection <- "soviet"
        y <- x[, selection]
    }

    s1 <- as.matrix(textstat_proxy(x, method = method, margin = margin, ...))
    s2 <- as.matrix(proxy::simil(as.matrix(x),
                                method = method, by_rows = by_rows, diag = TRUE, ...))
    diag(s1) <- NA
    diag(s2) <- NA

    if (ignore_upper)
        s1[upper.tri(s1, TRUE)] <- s2[upper.tri(s2, TRUE)] <- 0
    expect_equal(s1, s2, tolerance = 0.001)

    s3 <- as.matrix(textstat_proxy(x, y, method = method, margin = margin, ...))
    s4 <- as.matrix(proxy::simil(as.matrix(x), as.matrix(y),
                                method = method, by_rows = by_rows, diag = TRUE, ...))
    if (ignore_upper)
        s3[upper.tri(s3, TRUE)] <- s4[upper.tri(s4, TRUE)] <- 0
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
}

test_dist <- function(x, method, margin, ignore_upper = FALSE, ...) {

    if (margin == "documents") {
        by_rows <- TRUE
        selection <- "1985-Reagan"
        y <- x[selection, ]
    } else {
        by_rows <- FALSE
        selection <- "soviet"
        y <- x[, selection]
    }

    s1 <- as.matrix(textstat_proxy(x, method = method, margin = margin, ...))
    s2 <- as.matrix(proxy::dist(as.matrix(x),
                                method = method, by_rows = by_rows, diag = TRUE, ...))

    if (ignore_upper)
        s1[upper.tri(s1, TRUE)] <- s2[upper.tri(s2, TRUE)] <- 0
    expect_equal(s1, s2, tolerance = 0.001)

    s3 <- as.matrix(textstat_proxy(x, y, method = method, margin = margin, ...))
    s4 <- as.matrix(proxy::dist(as.matrix(x), as.matrix(y),
                                method = method, by_rows = by_rows, diag = TRUE, ...))
    if (ignore_upper)
        s3[upper.tri(s3, TRUE)] <- s4[upper.tri(s4, TRUE)] <- 0
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
}

# Similarity measures -------------------------------------------

test_that("test textstat_proxy cosine similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "cosine", "documents")
    test_simil(test_mt, "cosine", "features")
})

test_that("test textstat_proxy correlation similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "correlation", "documents")
    test_simil(test_mt, "correlation", "features")
})

test_that("test textstat_proxy jaccard similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "jaccard", "documents")
    test_simil(test_mt, "jaccard", "features")
})

test_that("test textstat_proxy ejaccard similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "ejaccard", "documents")
    test_simil(test_mt, "ejaccard", "features")
})

test_that("test textstat_proxy dice similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "dice", "documents")
    test_simil(test_mt, "dice", "features")
})

test_that("test textstat_proxy edice similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "edice", "documents")
    test_simil(test_mt, "edice", "features")
})

test_that("test textstat_proxy simple matching similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "simple matching", "documents")
    test_simil(test_mt, "simple matching", "features")
})

test_that("test textstat_proxy hamman similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "hamman", "documents")
    test_simil(test_mt, "hamman", "features")
})


# Distance measures -------------------------------------------

test_that("test textstat_proxy euclidean distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "euclidean", "documents")
    test_dist(test_mt, "euclidean", "features")
})

# test_that("test textstat_proxy chisquared distance on documents", {
#     skip_if_not_installed("ExPosition")
#     s1 <- as.matrix(textstat_proxy(test_mt, method = "chisquared", margin = "documents"))
#     s2 <- as.matrix(ExPosition::chi2Dist(as.matrix(test_mt))$D)
#     names(dimnames(s2)) <- NULL
#     expect_equal(s1, s2, tolerance = 0.001)
#
#     s3 <- as.matrix(textstat_proxy(test_mt, "1985-Reagan", method = "chisquared", margin = "documents"))
#     s4 <- as.matrix(ExPosition::chi2Dist(as.matrix(test_mt))$D[,"1985-Reagan"])
#     names(dimnames(s4)) <- NULL
#     expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
# })
#
# test_that("test textstat_proxy chisquared distance on features", {
#     skip_if_not_installed("ExPosition")
#     s1 <- as.matrix(textstat_proxy(test_mt, method = "chisquared", margin = "features"))
#     s2 <- as.matrix(ExPosition::chi2Dist(t(as.matrix(test_mt)))$D)
#     names(dimnames(s2)) <- NULL
#     expect_equal(s1, s2, tolerance = 0.001)
#
#     s3 <- as.matrix(textstat_proxy(test_mt, "soviet", method = "chisquared", margin = "features"))
#     s4 <- as.matrix(ExPosition::chi2Dist(t(as.matrix(test_mt)))$D[,"soviet"])
#     names(dimnames(s4)) <- NULL
#     expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
# })


test_that("test kullback kullback similarity", {
    skip_if_not_installed("proxy")
    # make dense matrix to avoide Inf in proxy::dist
    test_mt_dense <- test_mt + 1
    # proxy::dist() also incorrectly produces symmetric matrix
    test_dist(test_mt_dense, "kullback", "documents", ignore_upper = TRUE)
    test_dist(test_mt_dense, "kullback", "features", ignore_upper = TRUE)
})

test_that("test textstat_proxy manhattan distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "manhattan", "documents")
    test_dist(test_mt, "manhattan", "features")
})

test_that("test textstat_proxy maximum distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "maximum", "documents")
    test_dist(test_mt, "maximum", "features")
})

test_that("test textstat_proxy canberra distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "canberra", "documents")
    test_dist(test_mt, "canberra", "features")
})

test_that("test textstat_proxy canberra distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "canberra", "documents")
    test_dist(test_mt, "canberra", "features")
})

test_that("test textstat_proxy minkowski distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "minkowski", "documents", p = 0.1)
    test_dist(test_mt, "minkowski", "features", p = 0.1)
    test_dist(test_mt, "minkowski", "documents", p = 2)
    test_dist(test_mt, "minkowski", "features", p = 2)
    test_dist(test_mt, "minkowski", "documents", p = 10)
    test_dist(test_mt, "minkowski", "features", p = 10)
})

test_that("as.matrix works as expected", {
    txt <- c("Bacon ipsum dolor amet tenderloin hamburger bacon t-bone,",
             "Tenderloin turducken corned beef bacon.",
             "Burgdoggen venison tail, hamburger filet mignon capicola meatloaf pig pork belly.")
    mt <- dfm(txt)
    expect_equivalent(diag(as.matrix(textstat_proxy(mt))),
                      rep(1, 3))
})

test_that("textstat_proxy stops as expected for methods not supported", {
    expect_error(textstat_proxy(test_mt, method = "Yule"))
})

test_that("textstat_proxy works on zero-frequency features", {
    d1 <- dfm(c("a b c", "a b c d"))
    d2 <- dfm(letters[1:6])
    dtest <- dfm_match(d1, featnames(d2))

    expect_equal(
        textstat_proxy(dtest, method = "cosine")[2, 1], 0.866,
        tolerance = 0.001
    )
    expect_equal(
        textstat_proxy(dtest, method = "correlation")[2, 1], 0.707,
        tolerance = 0.001
    )
})

test_that("textstat_proxy works on zero-feature documents (#952)", {
    corp <- corpus(c("a b c c", "b c d", "a"),
                   docvars = data.frame(grp = factor(c("A", "A", "B"), levels = LETTERS[1:3])))
    mt <- dfm(corp)
    mt <- dfm_group(mt, groups = "grp", fill = TRUE)

    expect_equal(
        as.numeric(textstat_proxy(mt, method = "cosine")[1, ]),
        c(1, 0.2581, 0),
        tolerance = 0.001
    )
    expect_equal(
        as.numeric(textstat_proxy(mt, method = "correlation")[1, ]),
        c(1, -0.5222, 0),
        tolerance = 0.001
    )
})

test_that("textstat_proxy works with non-intersecting documents or features", {

    toks <- tokens(c(doc1 = "a b c d e", doc2 = "b c f e", doc3 = "c d f", doc4 = "f g h"), remove_punct = TRUE)
    mt <- dfm(toks)
    sim1 <- textstat_proxy(mt, margin = "features")
    expect_equal(as.matrix(textstat_proxy(mt[, c("a", "b")], mt[, c("c", "d", "e")], margin = "features")),
                 as.matrix(sim1[c("a", "b"), c("c", "d", "e"), drop = FALSE]))
    sim2 <- textstat_proxy(mt, margin = "documents")
    expect_equal(as.matrix(textstat_proxy(mt[c("doc1", "doc2"), ], mt[c("doc4"), ], margin = "documents")),
                 as.matrix(sim2[c("doc1", "doc2"), c("doc4"), drop = FALSE]))
})

test_that("raises error when dfm is empty (#1419)", {
    mt <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_silent(textstat_proxy(mt))
    expect_silent(textstat_proxy(mt, mt))
})

test_that("raises error when p is smaller than 1", {
    expect_error(textstat_proxy(test_mt, method = "minkowski", p = 0))
    expect_error(textstat_proxy(test_mt, method = "minkowski", p = -1))
})

test_that("sparse objects are of expected class and occur when expected", {

    expect_is(textstat_proxy(test_mt),
              "dsTMatrix")
    expect_is(textstat_proxy(test_mt, min_proxy = 10),
              "dsTMatrix")
    expect_is(textstat_proxy(test_mt, rank = 2),
              "dgTMatrix")
    expect_is(textstat_proxy(test_mt, method = "kullback"),
              "dgTMatrix")

})

test_that("rank argument is working", {

    expect_error(textstat_proxy(test_mt, rank = 0),
                 "rank must be great than or equal to 1")

    expect_equal(as.matrix(textstat_proxy(test_mt)),
                 as.matrix(textstat_proxy(test_mt, rank = 100)))

    expect_equal(as.matrix(textstat_proxy(test_mt, rank = 3)),
                 apply(as.matrix(textstat_proxy(test_mt)), 2,
                       function(x) ifelse(x >= sort(x, decreasing = TRUE)[3], x, 0)))

})

test_that("record zeros even in the sparse matrix", {
    toks <- tokens(c(doc1 = "a b c", doc2 = "d e f"), remove_punct = TRUE)
    mt <- dfm(toks)
    expect_true(any(textstat_proxy(mt)@x == 0))
    expect_true(any(textstat_proxy(mt, method = "cosine")@x == 0))
    expect_true(any(textstat_proxy(mt, method = "cosine", min_proxy = -0.5)@x == 0))
    expect_true(any(textstat_proxy(mt, method = "cosine", rank = 2)@x == 0))
    expect_true(any(textstat_proxy(mt, method = "dice")@x == 0))
})

test_that("textstat_proxy raises error when documents are different for feature similarity", {
    expect_silent(
        textstat_proxy(test_mt[1:5, ], test_mt[1:5, ], margin = "features")
    )
    expect_error(textstat_proxy(test_mt[1:5, ], test_mt[6:10, ], margin = "features"),
                 "x and y must contain the same documents")
})

test_that("textstat_proxy raises error when y is not a dfm", {
    expect_error(textstat_proxy(test_mt[1:5, ], 6:10, margin = "features"),
                 "y must be a dfm")
})

test_that("use_na is working", {
    mt <- as.dfm(matrix(c(rep(0, 4),
                          rep(1, 4),
                          c(1, 3, 2, 0)), ncol = 3))

    cos1 <- textstat_proxy(mt, margin = "features", method = "cosine", use_na = TRUE)
    cor1 <- textstat_proxy(mt, margin = "features", method = "correlation", use_na = TRUE)
    euc1 <- textstat_proxy(mt, margin = "features", method = "euclidean", use_na = TRUE)
    expect_equal(sum(is.na(cos1)), 5)
    expect_equal(sum(is.na(cor1)), 8)
    expect_equal(sum(is.na(euc1)), 5)

    cos2 <- textstat_proxy(mt, mt[, 3], margin = "features", method = "cosine", use_na = TRUE)
    cor2 <- textstat_proxy(mt, mt[, 3], margin = "features", method = "correlation", use_na = TRUE)
    euc2 <- textstat_proxy(mt, mt[, 3], margin = "features", method = "euclidean", use_na = TRUE)
    expect_equal(sum(is.na(cos2)), 1)
    expect_equal(sum(is.na(cor2)), 2)
    expect_equal(sum(is.na(euc2)), 1)
})

test_that("no value is greater than 1.0 (#1543)", {
    cos1 <- textstat_proxy(test_mt[1:5, ], test_mt[1:5, ], method = "cosine")
    expect_equal(sum(cos1 > 1), 0)
    cor1 <- textstat_proxy(test_mt[1:5, ], test_mt[1:5, ], method = "correlation")
    expect_true(all(cor1 <= 1.000000001))
})
