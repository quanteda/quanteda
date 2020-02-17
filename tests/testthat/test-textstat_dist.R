context("test textstat_dist")

mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
mt <- dfm_trim(mt, min_termfreq = 10)

test_that("dist object has all the attributes", {
    d1 <- textstat_dist(mt) %>% as.matrix %>% as.dist()
    expect_equal(class(attr(d1, "Labels")), "character")
    expect_equal(class(attr(d1, "Size")), "integer")
    expect_equal(class(attr(d1, "call")), "call")
    expect_equal(class(attr(d1, "Diag")), "logical")
    expect_equal(class(attr(d1, "Upper")), "logical")
})

test_that("y errors if not a dfm", {
    expect_error(
        textstat_dist(mt, y = c("mr", "president"), margin = "features"),
        "y must be a dfm matching x in the margin specified"
    )
})

test_that("selection takes integer or logical vector", {
    suppressWarnings({
    expect_equivalent(textstat_dist(mt, selection = c(2, 5), margin = "features"),
                      textstat_dist(mt, selection = c("mr", "president"), margin = "features"))
    l3 <- featnames(mt) %in% c("mr", "president")
    expect_equivalent(textstat_dist(mt, selection = l3, margin = "features"),
                      textstat_dist(mt, selection = c("mr", "president"), margin = "features"))
    })

    expect_error(textstat_dist(mt, y = "xxxx", margin = "features"))
    expect_error(textstat_dist(mt, y = 1000, margin = "features"))

    expect_equivalent(textstat_dist(mt, y = mt[c(2, 4), ], margin = "documents"),
                      textstat_dist(mt, y = mt[c("1985-Reagan", "1993-Clinton"), ], margin = "documents"))
    l4 <- docnames(mt) %in% c("1985-Reagan", "1993-Clinton")
    expect_equivalent(textstat_dist(mt, y = mt[l4, ], margin = "documents"),
                      textstat_dist(mt, y = mt[c("1985-Reagan", "1993-Clinton"), ], margin = "documents"))

    expect_error(textstat_dist(mt, y = "nothing", margin = "documents"))
    expect_error(textstat_dist(mt, y = 100, margin = "documents"))
})

test_that("textstat_dist() returns NA for empty dfm", {
    skip("until textstat_dist() works correctly for empty dfms")
    mt <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_equivalent(
        textstat_dist(mt, method = "euclidean") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "euclidean")
    )
    # expect_equivalent(
    #     textstat_dist(mt, method = "kullback") %>% as.matrix() %>% as.dist(),
    #     proxy::dist(as.matrix(mt), method = "kullback")
    # )
    expect_equivalent(
        textstat_dist(mt, method = "manhattan") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "manhattan")
    )
    # FAILS
    # expect_equivalent(
    #     textstat_dist(mt, method = "maximum") %>% as.matrix() %>% as.dist(),
    #     stats::dist(as.matrix(mt), method = "maximum")
    # )
    expect_equivalent(
        textstat_dist(mt, method = "canberra") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "canberra")
    )
    expect_equivalent(
        textstat_dist(mt, method = "minkowski") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "minkowski", p = 2)
    )
})

test_that("textstat_dist() returns NA for zero-variance documents", {
    skip("skip until textstat_dist() works correctly for zero-variance documents")
    mt <- data_dfm_lbgexample[1:5, 1:20]
    mt[1:2, ] <- 0
    mt[3:4, ] <- 1
    mt <- as.dfm(mt)

    # fails
    expect_equivalent(
        textstat_dist(mt, method = "euclidean") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "euclidean")
    )
    # expect_equivalent(
    #     textstat_dist(mt, method = "kullback") %>% as.matrix() %>% as.dist(),
    #     proxy::dist(as.matrix(mt), method = "kullback")
    # )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "manhattan") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "manhattan")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "maximum") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "maximum")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "canberra") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "canberra")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "minkowski") %>% as.matrix() %>% as.dist(),
        stats::dist(as.matrix(mt), method = "minkowski", p = 2)
    )
})

test_that("selection is always on columns (#1549)", {
    mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
    suppressWarnings({
        expect_equal(
        colnames(textstat_dist(mt, margin = "documents",
                               selection = c("1985-Reagan", "1989-Bush")) %>% as.matrix()),
        c("1985-Reagan", "1989-Bush")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "documents", selection = c(2, 3)) %>% as.matrix()),
        c("1985-Reagan", "1989-Bush")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "features", selection = c("justice", "and")) %>% as.matrix()),
        c("justice", "and")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "features", selection = c(4, 6)) %>% as.matrix()),
        c("mr", "chief")
    )
    })
})

test_that("all distances are bounded at 0", {
    methods <- c("euclidean", "manhattan", "maximum", "canberra", "minkowski")
    for (m in methods) {
        expect_gte(min(textstat_dist(mt, method = m, margin = "features")), 0)
    }
})

test_that("textstat_dist coercion methods work with options", {
    mt2 <- mt[6:10, ]

    # upper = TRUE, diag = TRUE
    tstat <- textstat_dist(mt2, margin = "documents")
    # expect_equal(nrow(tstat), nrow(mt2)^2)
    mat <- as.matrix(tstat)
    expect_equal(dim(mat), c(ndoc(mt2), ndoc(mt2)))
    # in matrix, diagonal is 0
    iden <- rep(0, ndoc(mt2)); names(iden) <- docnames(mt2)
    expect_equal(diag(mat), iden)

    # upper = TRUE, diag = FALSE
    tstat <- textstat_dist(mt2, margin = "documents")
    # expect_equal(nrow(tstat), nrow(mt2)^2 - ndoc(mt2))
    mat <- as.matrix(tstat)
    expect_equal(dim(mat), c(ndoc(mt2), ndoc(mt2)))
    iden <- rep(0, ndoc(mt2))
    names(iden) <- docnames(mt2)
    expect_equal(diag(mat), iden)

    # upper = FALSE, diag = TRUE
    tstat <- textstat_dist(mt2, margin = "documents")
    # expect_equal(nrow(tstat), (nrow(mt2)^2 - ndoc(mt2)) / 2 + ndoc(mt2))
    mat <- as.matrix(tstat)
    # expect_true(all(is.na(mat[upper.tri(mat)])))
    # in matrix, diagonal is 0
    iden <- rep(0, ndoc(mt2))
    names(iden) <- docnames(mt2)
    expect_equal(diag(as.matrix(tstat)), iden)

    # upper = FALSE, diag = FALSE
    tstat <- textstat_dist(mt2, margin = "documents")
    # expect_equal(nrow(tstat), (nrow(mt2)^2 - ndoc(mt2)) / 2)
    mat <- as.matrix(tstat)
    loweranddiag <- upper.tri(mat)
    diag(loweranddiag) <- TRUE
    # expect_true(all(is.na(mat[upper.tri(mat)])))
    # in matrix, diagonal is 0
    iden <- rep(0, ndoc(mt2)); names(iden) <- docnames(mt2)
    expect_equal(diag(mat), iden)
})
