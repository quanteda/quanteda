context("test quanteda:::textstat_proxy.R")

test_mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), remove = stopwords("english"),
               stem = TRUE, verbose = FALSE)
test_mt <- dfm_trim(test_mt, min_termfreq = 5)

test_simil <- function(x, method, margin, ignore_upper = FALSE, ...) {
    
    if (margin == "documents") {
        by_rows <- TRUE
        selection <- "1985-Reagan"
        y <- x[selection,]
    } else {
        by_rows <- FALSE
        selection <- "soviet"
        y <- x[,selection]
    }
    
    s1 <- as.matrix(quanteda:::textstat_proxy(x, method = method, margin = margin, ...))
    s2 <- as.matrix(proxy::simil(as.matrix(x), 
                                method = method, by_rows = by_rows, diag = TRUE, ...))

    if (ignore_upper)
        s1[upper.tri(s1, TRUE)] <- s2[upper.tri(s2, TRUE)] <- 0
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(quanteda:::textstat_proxy(x, selection, method = method, margin = margin, ...))
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
        y <- x[selection,]
    } else {
        by_rows <- FALSE
        selection <- "soviet"
        y <- x[,selection]
    }
    
    s1 <- as.matrix(quanteda:::textstat_proxy(x, method = method, margin = margin, ...))
    s2 <- as.matrix(proxy::dist(as.matrix(x), 
                                method = method, by_rows = by_rows, diag = TRUE, ...))
    
    if (ignore_upper)
        s1[upper.tri(s1, TRUE)] <- s2[upper.tri(s2, TRUE)] <- 0
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(quanteda:::textstat_proxy(x, selection, method = method, margin = margin, ...))
    s4 <- as.matrix(proxy::dist(as.matrix(x), as.matrix(y), 
                                method = method, by_rows = by_rows, diag = TRUE, ...))
    if (ignore_upper)
        s3[upper.tri(s3, TRUE)] <- s4[upper.tri(s4, TRUE)] <- 0
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
}

# Similarity measures -------------------------------------------

test_that("test quanteda:::textstat_proxy cosine similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "cosine", "documents")
    test_simil(test_mt, "cosine", "features")
})

test_that("test quanteda:::textstat_proxy correlation similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "correlation", "documents")
    test_simil(test_mt, "correlation", "features")
})

test_that("test quanteda:::textstat_proxy jaccard similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "jaccard", "documents")
    test_simil(test_mt, "jaccard", "features")
})

test_that("test quanteda:::textstat_proxy ejaccard similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "ejaccard", "documents")
    test_simil(test_mt, "ejaccard", "features")
})

test_that("test quanteda:::textstat_proxy dice similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "dice", "documents")
    test_simil(test_mt, "dice", "features")
})

test_that("test quanteda:::textstat_proxy edice similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "edice", "documents")
    test_simil(test_mt, "edice", "features")
})

test_that("test quanteda:::textstat_proxy simple matching similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "simple matching", "documents")
    test_simil(test_mt, "simple matching", "features")
})

test_that("test quanteda:::textstat_proxy hamman similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "hamman", "documents")
    test_simil(test_mt, "hamman", "features")
})

test_that("test quanteda:::textstat_proxy faith similarity", {
    skip_if_not_installed("proxy")
    test_simil(test_mt, "faith", "documents", ignore_upper = TRUE)
    test_simil(test_mt, "faith", "features", ignore_upper = TRUE)
})

# Distance measures -------------------------------------------

test_that("test quanteda:::textstat_proxy euclidean distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "euclidean", "documents")
    test_dist(test_mt, "euclidean", "features")
})

# test_that("test quanteda:::textstat_proxy chisquared distance on documents", {
#     skip_if_not_installed("ExPosition")
#     s1 <- as.matrix(quanteda:::textstat_proxy(test_mt, method = "chisquared", margin = "documents"))
#     s2 <- as.matrix(ExPosition::chi2Dist(as.matrix(test_mt))$D)
#     names(dimnames(s2)) <- NULL
#     expect_equal(s1, s2, tolerance = 0.001)
#     
#     s3 <- as.matrix(quanteda:::textstat_proxy(test_mt, "1985-Reagan", method = "chisquared", margin = "documents"))
#     s4 <- as.matrix(ExPosition::chi2Dist(as.matrix(test_mt))$D[,"1985-Reagan"])
#     names(dimnames(s4)) <- NULL
#     expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
# })
# 
# test_that("test quanteda:::textstat_proxy chisquared distance on features", {
#     skip_if_not_installed("ExPosition")
#     s1 <- as.matrix(quanteda:::textstat_proxy(test_mt, method = "chisquared", margin = "features"))
#     s2 <- as.matrix(ExPosition::chi2Dist(t(as.matrix(test_mt)))$D)
#     names(dimnames(s2)) <- NULL
#     expect_equal(s1, s2, tolerance = 0.001)
#     
#     s3 <- as.matrix(quanteda:::textstat_proxy(test_mt, "soviet", method = "chisquared", margin = "features"))
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

test_that("test quanteda:::textstat_proxy manhattan distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "manhattan", "documents")
    test_dist(test_mt, "manhattan", "features")
})

test_that("test quanteda:::textstat_proxy maximum distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "maximum", "documents")
    test_dist(test_mt, "maximum", "features")
})

test_that("test quanteda:::textstat_proxy canberra distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "canberra", "documents")
    test_dist(test_mt, "canberra", "features")
})

test_that("test quanteda:::textstat_proxy canberra distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "canberra", "documents")
    test_dist(test_mt, "canberra", "features")
})

test_that("test quanteda:::textstat_proxy minkowski distance", {
    skip_if_not_installed("proxy")
    test_dist(test_mt, "minkowski", "documents", p = 0.1)
    test_dist(test_mt, "minkowski", "features", p = 0.1)
    test_dist(test_mt, "minkowski", "documents", p = 2)
    test_dist(test_mt, "minkowski", "features", p = 2)
    test_dist(test_mt, "minkowski", "documents", p = 10)
    test_dist(test_mt, "minkowski", "features", p = 10)
})

test_that("as.matrix works as expected",{
    txt <- c('Bacon ipsum dolor amet tenderloin hamburger bacon t-bone,', 
             'Tenderloin turducken corned beef bacon.', 
             'Burgdoggen venison tail, hamburger filet mignon capicola meatloaf pig pork belly.')
    mt <- dfm(txt)
    expect_equivalent(diag(as.matrix(quanteda:::textstat_proxy(mt))), 
                      rep(1, 3))
})

test_that("quanteda:::textstat_proxy stops as expected for methods not supported",{
    expect_error(quanteda:::textstat_proxy(test_mt, method = "Yule"))
})

test_that("quanteda:::textstat_proxy works on zero-frequency features", {
    d1 <- dfm(c("a b c", "a b c d"))
    d2 <- dfm(letters[1:6])
    dtest <- dfm_select(d1, d2)
    
    expect_equal(
        quanteda:::textstat_proxy(dtest, method = "cosine")[2,1], 0.866,
        tolerance = 0.001
    )    
    expect_equal(
        quanteda:::textstat_proxy(dtest, method = "correlation")[2,1], 0.707,
        tolerance = 0.001
    )    
})

test_that("quanteda:::textstat_proxy works on zero-feature documents (#952)", {
    corp <- corpus(c('a b c c', 'b c d', 'a'),
                   docvars = data.frame(grp = factor(c("A", "A", "B"), levels = LETTERS[1:3])))
    mt <- dfm(corp)
    mt <- dfm_group(mt, groups = "grp", fill = TRUE)

    expect_equal(
        as.numeric(quanteda:::textstat_proxy(mt, method = "cosine")[1,]),
        c(1, 0.2581, 0),
        tolerance = 0.001
    ) 
    expect_equal(
        as.numeric(quanteda:::textstat_proxy(mt, method = "correlation")[1,]),
        c(1, -0.5222, 0),
        tolerance = 0.001
    )    
})

test_that("selection takes integer or logical vector", {
    expect_identical(quanteda:::textstat_proxy(test_mt, c(1,5), margin = "features"),
                     quanteda:::textstat_proxy(test_mt, c("senat", "chief"), margin = "features"))
    l1 <- featnames(test_mt) %in% c("senat", "chief")
    expect_identical(quanteda:::textstat_proxy(test_mt, l1, margin = "features"),
                     quanteda:::textstat_proxy(test_mt, c("senat", "chief"), margin = "features"))
    
    expect_error(quanteda:::textstat_proxy(test_mt, "xxxx", margin = "features"))
    expect_error(quanteda:::textstat_proxy(test_mt, 1000, margin = "features"))
    
    expect_identical(quanteda:::textstat_proxy(test_mt, c(2,4), margin = "documents"),
                     quanteda:::textstat_proxy(test_mt, c("1985-Reagan", "1993-Clinton"), margin = "documents"))
    l2 <- docnames(test_mt) %in% c("1985-Reagan", "1993-Clinton")
    expect_identical(quanteda:::textstat_proxy(test_mt, l2, margin = "documents"),
                     quanteda:::textstat_proxy(test_mt, c("1985-Reagan", "1993-Clinton"), margin = "documents"))
    
    expect_error(quanteda:::textstat_proxy(test_mt, "nothing", margin = "documents"))
    expect_error(quanteda:::textstat_proxy(test_mt, 100, margin = "documents"))
})

test_that("selection works with dfm with padding", {
    
    toks <- tokens(c(doc1 = 'a b c d e', doc2 = 'b c f e'), remove_punct = TRUE)
    toks <- tokens_remove(toks, 'b', padding = TRUE)
    mt <- dfm(toks)
    expect_silent(quanteda:::textstat_proxy(mt, selection = c('c'), margin = 'features'))
    expect_silent(quanteda:::textstat_proxy(mt, selection = c(''), margin = 'features'))
    expect_silent(quanteda:::textstat_proxy(mt, selection = c('doc2'), margin = 'documents'))
    
})

test_that("raises error when dfm is empty (#1419)", {
    mt <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_error(quanteda:::textstat_proxy(mt),
                 quanteda:::message_error("dfm_empty"))
})

test_that("raises error when p is smaller than 1", {
    expect_error(textstat_dist(test_mt, method = "minkowski", p = 0))
    expect_error(textstat_dist(test_mt, method = "minkowski", p = -1))
})

test_that("sparse objects are of expected class and occur when expected", {
    skip("***Needs attention***")
    expect_is(
        textstat_dist(dfm_weight(test_mt, "prop"), value = "sparsematrix", min_dist = .07),
        "dtCMatrix"
    )
    expect_is(
        textstat_dist(dfm_weight(test_mt, "prop"), min_dist = 0),
        "dtCMatrix"
    )
})

