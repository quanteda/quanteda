context("test textstat_simil")

mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
mt <- dfm_trim(mt, min_termfreq = 10)

test_that("y errors if not a dfm", {
    expect_error(
        textstat_simil(mt, y = c("mr", "president"), margin = "features"),
        "y must be a dfm matching x in the margin specified"
    )
})

test_that("selection takes integer or logical vector", {
    expect_equivalent(textstat_simil(mt, y = mt[, c(2, 5)], margin = "features"),
                      textstat_simil(mt, y = mt[, c("mr", "president")], margin = "features"))
    suppressWarnings(expect_equivalent(textstat_simil(mt, y = mt[, c(2, 5)], margin = "features"),
                      textstat_simil(mt, y = mt[, c("mr", "president")], margin = "features")))

    l1 <- featnames(mt) %in% c("mr", "president")
    expect_equivalent(textstat_simil(mt, y = mt[, l1], margin = "features"),
                      textstat_simil(mt, y = mt[, c("mr", "president")], margin = "features"))

    expect_error(textstat_simil(mt, "xxxx", margin = "features"))
    expect_error(textstat_simil(mt, 1000, margin = "features"))

    expect_equivalent(textstat_simil(mt, y = mt[c(2, 4), ], margin = "documents"),
                      textstat_simil(mt, y = mt[c("1985-Reagan", "1993-Clinton"), ], margin = "documents"))
    l2 <- docnames(mt) %in% c("1985-Reagan", "1993-Clinton")
    expect_equivalent(textstat_simil(mt, y = mt[l2, ], margin = "documents"),
                      textstat_simil(mt, y = mt[c("1985-Reagan", "1993-Clinton"), ], margin = "documents"))

    expect_error(textstat_simil(mt, y = "nothing", margin = "documents"))
    expect_error(textstat_simil(mt, y = 100, margin = "documents"))
})

test_that("textstat_simil() returns NA for empty dfm", {
    skip("Skip until textstat_simil() has been corrected for empty dfms")
    skip_if_not_installed("proxy")
    mt <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_equivalent(
        as.matrix(textstat_simil(mt, method = "correlation")),
        cor(t(as.matrix(mt)), method = "pearson")
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "correlation")),
        proxy::simil(as.matrix(mt), method = "correlation")
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "cosine")),
        proxy::simil(as.matrix(mt), method = "cosine")
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "jaccard")),
        as.dist(proxy::simil(as.matrix(mt), method = "jaccard"))
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "ejaccard")),
        as.dist(proxy::simil(as.matrix(mt), method = "ejaccard"))
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "dice")),
        as.dist(proxy::simil(as.matrix(mt), method = "dice"))
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "edice")),
        as.dist(proxy::simil(as.matrix(mt), method = "edice"))
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "hamman")),
        proxy::dist(as.matrix(mt), method = "hamman")
    )
    expect_equivalent(
        as.dist(textstat_simil(mt, method = "simple matching")),
        as.dist(proxy::simil(as.matrix(mt), method = "simple matching"))
    )
})

test_that("textstat_simil() returns NA for zero-variance documents", {
    mt <- data_dfm_lbgexample[1:5, 1:20]
    mt[1:2, ] <- 0
    mt[3:4, ] <- 1
    mt <- as.dfm(mt)
    mt_na_all <- matrix(NA, nrow = 5, ncol = 5,
                        dimnames = list(paste0("R", 1:5), paste0("R", 1:5)))
    mt_na_some <- mt_na_all
    mt_na_some[3:4, 3:4] <- 1

    expect_equivalent(
        as.matrix(textstat_simil(mt, method = "correlation")),
        mt_na_all
    )
    expect_equal(
        as.matrix(textstat_simil(mt, method = "cosine")),
        mt_na_some
    )

    expect_equivalent(
         as.matrix(textstat_simil(mt, method = "jaccard")),
         mt_na_some
    )

    expect_equivalent(
        as.matrix(textstat_simil(mt, method = "ejaccard")),
        mt_na_some
    )

    expect_equivalent(
        as.matrix(textstat_simil(mt, method = "dice")),
        mt_na_some
    )

    expect_equal(
        as.matrix(textstat_simil(mt, method = "edice")),
        mt_na_some
    )

    expect_equal(
        as.matrix(textstat_simil(mt, method = "hamman")),
        mt_na_some
    )

    expect_equal(
        as.matrix(textstat_simil(mt, method = "simple matching")),
        mt_na_some
    )
})

test_that("selection is always on columns (#1549)", {
    mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
    suppressWarnings(expect_equal(
        textstat_simil(mt, margin = "documents", selection = c("1985-Reagan", "1989-Bush")) %>%
            as.matrix() %>%
            colnames(),
        c("1985-Reagan", "1989-Bush")
    ))
    suppressWarnings(expect_equal(
        textstat_simil(mt, margin = "documents", selection = c(2, 3)) %>%
            as.matrix() %>%
            colnames(),
        c("1985-Reagan", "1989-Bush")
    ))
    suppressWarnings(expect_equal(
        textstat_simil(mt, margin = "features", selection = c("justice", "and")) %>%
            as.matrix() %>%
            colnames(),
        c("justice", "and")
    ))
    suppressWarnings(expect_equal(
        textstat_simil(mt, margin = "features", selection = c(4, 6)) %>%
            as.matrix() %>%
            colnames(),
        c("mr", "chief")
    ))
})

test_that("all similarities are between 0 and 1", {
    methods <- c("correlation", "cosine", "jaccard", "ejaccard",
                 "dice", "edice", "hamman", "simple matching")
    for (m in methods) {
        minmax <- range(textstat_simil(mt, method = m, margin = "documents"))
        tol <- .000001
        expect_gte(minmax[1], 0)
        expect_lte(minmax[2], 1.0 + tol)
    }
})

test_that("textstat_simil is stable across repetitions", {
    res <- textstat_simil(mt, y = mt[c(2, 4), ],
                          margin = "documents")
    set.seed(10)
    resv <- list()
    for (i in 1:100) {
        resv[[i]] <- as.matrix(textstat_simil(mt, y = mt[2, ],
                                              margin = "documents"))
    }
    rescols <- do.call(cbind, resv)
    expect_true(all(apply(rescols, 1, sd) == 0))
})

test_that("textstat_simil coercion methods work with options", {
    mt2 <- mt[6:10, ]

    # upper = TRUE, diag = TRUE
    tstat <- textstat_simil(mt2, margin = "documents")
    expect_equal(
        nrow(as.data.frame(tstat, diag = TRUE, upper = TRUE)),
        nrow(mt2) ^ 2
    )
    mat <- as.matrix(tstat)
    expect_equal(dim(mat), c(ndoc(mt2), ndoc(mt2)))
    # in matrix, diagonal is 1.0
    iden <- rep(1, ndoc(mt2)); names(iden) <- docnames(mt2)
    expect_equal(diag(mat), iden)
    lis <- as.list(tstat, sort = TRUE, diag = TRUE)
    lislen <- rep(ndoc(mt2), 5); names(lislen) <- docnames(mt2)
    expect_equivalent(lengths(lis), rep(ndoc(mt2), ndoc(mt2)))
    # in list, sorted first item is comparison to itself
    expect_identical(names(lis), names(sapply(lis, "[[", 1)))
    expect_equal(iden, sapply(lis, "[[", 1))

    # upper = TRUE, diag = FALSE
    tstat <- textstat_simil(mt2, margin = "documents")
    expect_equal(
        nrow(as.data.frame(tstat, upper = TRUE, diag = FALSE)),
        nrow(mt2) ^ 2 - ndoc(mt2)
    )
    mat <- as.matrix(tstat)
    expect_equal(dim(mat), c(ndoc(mt2), ndoc(mt2)))
    # # in matrix, diagonal is NA
    # iden <- rep(as.numeric(NA), ndoc(mt2)); names(iden) <- docnames(mt2)
    # expect_equal(diag(as.matrix(tstat)), iden)
    # in matrix, diagonal is 1.0
    iden <- rep(1, ndoc(mt2))
    names(iden) <- docnames(mt2)
    expect_equal(diag(mat), iden)
    lis <- as.list(tstat, sort = TRUE, diag = FALSE)
    expect_equivalent(lengths(lis), rep(ndoc(mt2) - 1, ndoc(mt2)))
    expect_identical(names(lis), names(sapply(lis, "[[", 1)))
    # in list, item not compared to itself
    expect_true(all(sapply(seq_along(lis), function(y) ! names(lis[y]) %in% names(y))))

    # upper = FALSE, diag = TRUE
    tstat <- textstat_simil(mt2, margin = "documents")
    expect_equal(
        nrow(as.data.frame(tstat, upper = FALSE, diag = TRUE)),
        (nrow(mt2) ^ 2 - ndoc(mt2)) / 2 + ndoc(mt2)
    )
    mat <- as.matrix(tstat)
    # expect_true(all(is.na(mat[upper.tri(mat)])))
    # in matrix, diagonal is 1.0
    iden <- rep(1, ndoc(mt2)); names(iden) <- docnames(mt2)
    expect_equal(diag(as.matrix(tstat)), iden)
    # in matrix, lower is NA
    lis <- as.list(tstat, sort = TRUE, diag = TRUE)
    lislen <- rep(ndoc(mt2), ndoc(mt2)); names(lislen) <- docnames(mt2)
    expect_equivalent(lengths(lis), rep(ndoc(mt2), ndoc(mt2)))
    # in list, sorted first item is comparison to itself
    expect_identical(names(lis), names(sapply(lis, "[[", 1)))
    expect_equal(iden, sapply(lis, "[[", 1))

    # upper = FALSE, diag = FALSE
    tstat <- textstat_simil(mt2, margin = "documents")
    expect_equal(
        nrow(as.data.frame(tstat, upper = FALSE, diag = FALSE)),
        (nrow(mt2) ^ 2 - ndoc(mt2)) / 2
    )
    mat <- as.matrix(tstat)
    loweranddiag <- upper.tri(mat)
    diag(loweranddiag) <- TRUE
    # expect_true(all(is.na(mat[upper.tri(mat)])))
    # in matrix, diagonal is 1.0
    iden <- rep(1, ndoc(mt2))
    names(iden) <- docnames(mt2)
    expect_equal(diag(mat), iden)
    lis <- as.list(tstat, sort = TRUE, diag = FALSE)
    expect_equivalent(lengths(lis), rep(ndoc(mt2) - 1, ndoc(mt2)))
    # in list, item not compared to itself
    expect_true(all(sapply(seq_along(lis), function(y) ! names(lis[y]) %in% names(y))))
})

test_that("as.list.texstat_simil() is robust", {
    expect_error(
        as.list(textstat_simil(mt), n = 0),
        "n must be 1 or greater"
    )
    expect_equivalent(
        lengths(as.list(textstat_simil(mt), n = 2)),
        rep(2, ndoc(mt))
    )
    expect_equivalent(
        lengths(as.list(textstat_simil(mt), n = ndoc(mt) + 20, diag = TRUE)),
        rep(ndoc(mt), ndoc(mt))
    )
    expect_warning(
        as.list(textstat_simil(mt), n = 2, sort = FALSE),
        "ignoring n when sorted = FALSE"
    )
})

test_that("as.list.textstat_simil works with features margin", {
    tstat <- textstat_simil(mt, y = mt[, c("world", "freedom")],
                            method = "cosine", margin = "features")
    lis <- as.list(tstat, n = 5, diag = FALSE)
    expect_equal(
        sapply(lis, head, 1),
        c("world.today" = 0.952, "freedom.independence" = 0.937),
        tol = .01
    )
    expect_identical(names(lis), c("world", "freedom"))

    tstat <- textstat_simil(mt, y = mt[, "freedom"],
                            method = "cosine", margin = "features")
    lis <- as.list(tstat, n = 5, diag = TRUE)
    expect_equal(
        sapply(lis, head, 1),
        c("freedom.freedom" = 1)
    )
})

test_that("as.data.frame.textstat_simildist works with selection", {
    mt2 <- mt[6:10, ]
    tstat <- textstat_simil(mt2, y = mt[c("2017-Trump", "2001-Bush"), ], method = "cosine")
    expect_equal(
        as.character(as.data.frame(tstat, diag = FALSE, upper = FALSE)$document2),
        c(rep("2017-Trump", 4), rep("2001-Bush", 4))
    )
    expect_equal(
        as.character(as.data.frame(tstat, diag = TRUE, upper = FALSE)$document2),
        c(rep("2017-Trump", 5), rep("2001-Bush", 5))
    )
    suppressWarnings(expect_equal(
        as.character(as.data.frame(tstat, diag = FALSE, upper = TRUE)$document2),
        c(rep("2017-Trump", 4), rep("2001-Bush", 4))
    ))
    suppressWarnings(expect_equal(
        as.character(as.data.frame(tstat, diag = TRUE, upper = TRUE)$document2),
        c(rep("2017-Trump", 5), rep("2001-Bush", 5))
    ))

    expect_warning(
        as.data.frame(tstat, upper = TRUE),
        "upper = TRUE has no effect when columns have been selected"
    )

    expect_identical(
        names(as.data.frame(textstat_simil(mt2, method = "cosine")))[3],
        "cosine"
    )
    expect_identical(
        names(as.data.frame(textstat_simil(mt2, method = "correlation")))[3],
        "correlation"
    )
    expect_identical(
        names(as.data.frame(textstat_dist(mt2, method = "euclidean")))[3],
        "euclidean"
    )
})

test_that("textstat_simil validator works", {
    expect_error(
        textstat_simil(data_dfm_lbgexample, min_simil = -1.1),
        "min_simil must range from -1.0 to 1.0"
    )
})

test_that("textstat_simil show/head/tail methods work", {
    expect_output(
        show(textstat_simil(data_dfm_lbgexample, method = "cosine")),
        "textstat_simil object;"
    )
    expect_equal(
        as.matrix(head(textstat_simil(data_dfm_lbgexample, method = "cosine"), n = 3)),
        as.matrix(textstat_simil(data_dfm_lbgexample, method = "cosine"))[1:3, ]
    )
    expect_equal(
        as.matrix(tail(textstat_simil(data_dfm_lbgexample, method = "cosine"), n = 3)),
        as.matrix(textstat_simil(data_dfm_lbgexample, method = "cosine"))[4:6, ]
    )
})

test_that("min_simil argument works", {
    tstat <- textstat_simil(mt, method = "cosine", min_simil = 0.98)
    expect_output(
        show(tstat),
        "1.000       0.982         .            .         ",
        fixed = TRUE
    )

    expect_equal(
        as.data.frame(tstat, diag = FALSE, upper = FALSE),
        data.frame(document1 = factor(c("1981-Reagan"),
                                      levels = rownames(tstat)),
                   document2 = factor(c("1985-Reagan"),
                                      levels = rownames(tstat)),
                   cosine = c(0.9817)),
        tol = .0001
    )

    expect_equal(
        as.data.frame(tstat, diag = FALSE, upper = TRUE),
        data.frame(document1 = factor(c("1985-Reagan", "1981-Reagan"),
                                      levels = rownames(tstat)),
                   document2 = factor(c("1981-Reagan", "1985-Reagan"),
                                      levels = rownames(tstat)),
                   cosine = c(0.9817, 0.9817)),
        tol = .0001
    )

    expect_equal(
        as.list(tstat, diag = FALSE),
        list("1981-Reagan" = c("1985-Reagan" = 0.981771),
             "1985-Reagan" = c("1981-Reagan" = 0.981771)),
        tol = .001
    )
    expect_equal(
        sapply(as.list(tstat, diag = TRUE), "[", 1),
        structure(rep(1, ndoc(mt)),
                      names = paste(docnames(mt), docnames(mt), sep = "."))
    )
})

test_that("test that min_simil coercion to matrix works as expected", {
    dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year > 2000),
                 remove_punct = TRUE, remove = stopwords("english"))

    tstat1 <- textstat_simil(dfmat, method = "cosine", margin = "documents", min_simil = 0.6)
    expect_equal(
        as.matrix(tstat1)[3, 4:5],
        c("2013-Obama" = 0.6373, "2017-Trump" = NA),
        tol = .0001
    )
    expect_equal(
        as.matrix(tstat1, omitted = 0)[3, 4:5],
        c("2013-Obama" = 0.6373, "2017-Trump" = 0),
        tol = .0001
    )

    tstat2 <- textstat_simil(dfmat, y = dfmat[c("2009-Obama", "2013-Obama"), ],
                             method = "cosine", margin = "documents", min_simil = 0.6)
    expect_equal(
        as.matrix(tstat2)[3:5, 1],
        c("2009-Obama" = 1, "2013-Obama" = 0.6373, "2017-Trump" = NA),
        tol = .0001
    )
    expect_equal(
        as.matrix(tstat2, omitted = 0)[3:5, 1],
        c("2009-Obama" = 1, "2013-Obama" = 0.6373, "2017-Trump" = 0),
        tol = .0001
    )
})

test_that("y is working in the same way as selection (#1714)", {
    suppressWarnings({
        expect_identical(textstat_simil(mt, selection = c("2009-Obama", "2013-Obama"),
                                    margin = "documents"),
                     textstat_simil(mt, mt[c("2009-Obama", "2013-Obama"), ],
                                    margin = "documents"))

    expect_identical(textstat_simil(mt, selection = c("world", "freedom"),
                                    margin = "features"),
                     textstat_simil(mt, mt[, c("world", "freedom")],
                                    margin = "features"))

    expect_identical(textstat_dist(mt, selection = c("2009-Obama", "2013-Obama"),
                                    margin = "documents"),
                     textstat_dist(mt, mt[c("2009-Obama", "2013-Obama"), ],
                                    margin = "documents"))

    expect_identical(textstat_dist(mt, selection = c("world", "freedom"),
                                    margin = "features"),
                     textstat_dist(mt, mt[, c("world", "freedom")],
                                    margin = "features"))
    })
})

test_that("diag2na is working", {

    mat1 <- Matrix::Matrix(1:9, nrow = 3,
                           dimnames = list(c("a", "b", "c"), c("b", "c", "d")))
    expect_equal(as.matrix(quanteda:::diag2na(as(mat1, "dgTMatrix"))),
                 matrix(c(1, NA, 3, 4, 5, NA, 7, 8, 9), nrow = 3,
                        dimnames = list(c("a", "b", "c"), c("b", "c", "d"))))

    mat2 <- Matrix::Matrix(1:9, nrow = 3,
                           dimnames = list(c("a", "b", "c"), c("d", "c", "b")))
    expect_equal(as.matrix(quanteda:::diag2na(as(mat2, "dgTMatrix"))),
                 matrix(c(1, 2, 3, 4, 5, NA, 7, NA, 9), nrow = 3,
                        dimnames = list(c("a", "b", "c"), c("d", "c", "b"))))

    mat3 <- Matrix::Matrix(1:6, nrow = 3,
                           dimnames = list(c("a", "b", "c"), c("c", "b")))
    expect_equal(as.matrix(quanteda:::diag2na(as(mat3, "dgTMatrix"))),
                 matrix(c(1, 2, NA, 4, NA, 6), nrow = 3,
                        dimnames = list(c("a", "b", "c"), c("c", "b"))))

    mat4 <- Matrix::forceSymmetric(mat1)
    expect_equal(as.matrix(quanteda:::diag2na(as(mat4, "dsTMatrix"))),
                 matrix(c(NA, 4, 7, 4, NA, 8, 7, 8, NA), nrow = 3,
                        dimnames = list(c("b", "c", "d"), c("b", "c", "d"))))

    mat5 <- Matrix::Matrix(rep(0, 9), nrow = 3,
                           dimnames = list(c("a", "b", "c"), c("b", "c", "d")))
    expect_equal(as.matrix(quanteda:::diag2na(as(mat5, "dgTMatrix"))),
                 matrix(c(0, NA, 0, 0, 0, NA, 0, 0, 0), nrow = 3,
                        dimnames = list(c("a", "b", "c"), c("b", "c", "d"))))

})

test_that("make_na_matrix is working", {

    expect_equal(
        as.matrix(quanteda:::make_na_matrix(c(5, 4), row = 2L:3L)),
        matrix(c(c(0, NA, NA, 0, 0),
                 c(0, NA, NA, 0, 0),
                 c(0, NA, NA, 0, 0),
                 c(0, NA, NA, 0, 0)), nrow = 5)
    )

    expect_equal(
        as.matrix(quanteda:::make_na_matrix(c(5, 4), col = 3L)),
        matrix(c(c(0, 0, 0, 0, 0),
                 c(0, 0, 0, 0, 0),
                 rep(NA, 5),
                 c(0, 0, 0, 0, 0)), nrow = 5)
    )

    expect_equal(
        as.matrix(quanteda:::make_na_matrix(c(5, 4), col = 1L:2L, row = 2L:3L)),
        matrix(c(rep(NA, 5), rep(NA, 5),
                 c(0, NA, NA, 0, 0),
                 c(0, NA, NA, 0, 0)), nrow = 5)
    )

    expect_equal(
        as.matrix(quanteda:::make_na_matrix(c(5, 4), 2L:3L, c(1L:2L))),
        matrix(c(rep(NA, 5), rep(NA, 5),
               c(0, NA, NA, 0, 0),
               c(0, NA, NA, 0, 0)), nrow = 5)
    )

    expect_equal(
        as.matrix(quanteda:::make_na_matrix(c(5, 4), 1L, 3L)),
        matrix(c(c(NA, 0, 0, 0, 0),
                 c(NA, 0, 0, 0, 0),
                 rep(NA, 5),
                 c(NA, 0, 0, 0, 0)), nrow = 5)
    )

})

test_that("symmetric class is correctly given", {

    dist1 <- textstat_dist(mt)
    expect_identical(
        Matrix::tril(dist1),
        t(Matrix::triu(dist1))
    )
    dist2 <- textstat_dist(mt, mt)
    expect_identical(
        Matrix::tril(dist2),
        t(Matrix::triu(dist2))
    )
    siml1 <- textstat_simil(mt)
    expect_identical(
        Matrix::tril(siml1),
        t(Matrix::triu(siml1))
    )
    siml2 <- textstat_simil(mt, mt)
    expect_identical(
        Matrix::tril(siml2),
        t(Matrix::triu(siml2))
    )
})

test_that("as.data.frame works with subsetted object", {
    levs <- c(paste0("R", 1:5), "V1")
    simildf <- textstat_simil(data_dfm_lbgexample[-1, ], data_dfm_lbgexample[1, ]) %>%
        as.data.frame()
    expect_equal(
        simildf,
        data.frame(document1 = factor(levs[-1], levels = levs),
                   document2 = factor(rep("R1", 5), levels = levs),
                   correlation = c(0.18, -0.29, -0.32, -0.32, -0.12)),
        tol = .01
    )
    expect_identical(levels(simildf$document1), levels(simildf$document1))
    
    simildf <- textstat_simil(data_dfm_lbgexample, data_dfm_lbgexample[c(1, 3), ]) %>%
        as.data.frame()
    levs2 <- levs[c(1, 3, 2, 4:6)]
    expect_identical(
        simildf[, -3],
        data.frame(document1 = factor(c(levs[-1], levs[-3]), levels = levs2),
                   document2 = factor(c(rep("R1", 5), c(rep("R3", 5))), levels = levs2))
    )
    expect_identical(levels(simildf$document1), levels(simildf$document1))
})
    