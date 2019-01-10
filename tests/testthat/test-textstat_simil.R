context("test textstat_dist and textstat_simil versus older methods")

mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
mt <- dfm_trim(mt, min_termfreq = 10)

test_that("test old and new textstat_dist are the same", {

    expect_equivalent(textstat_dist(mt), 
                      textstat_dist_old(mt),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, margin = "features"), 
                      textstat_dist_old(mt, margin = "features"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, selection = "1985-Reagan"), 
                      textstat_dist_old(mt, selection = "1985-Reagan"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, method = "euclidean"), 
                     textstat_dist_old(mt, method = "euclidean"))
    
    expect_equivalent(textstat_dist(mt, method = "euclidean"), 
                      textstat_dist_old(mt, method = "euclidean"),
                      tolerance = 0.01)
    
    # equialent only when the dfm is dense
    expect_equivalent(textstat_dist(mt + 1, method = "kullback"), 
                      textstat_dist_old(mt + 1, method = "kullback"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, method = "manhattan"), 
                      textstat_dist_old(mt, method = "manhattan"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, method = "maximum"), 
                      textstat_dist_old(mt, method = "maximum"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, method = "canberra"), 
                      textstat_dist_old(mt, method = "canberra"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, method = "minkowski"), 
                      textstat_dist_old(mt, method = "minkowski"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_dist(mt, method = "minkowski", p = 3), 
                      textstat_dist_old(mt, method = "minkowski", p = 3),
                      tolerance = 0.01)
    
})


test_that("test old and new textstat_simil are the same", {
    
    expect_equivalent(textstat_simil(mt), 
                      textstat_simil_old(mt), 
                      tolerance = 0.01)
    
    expect_equivalent(textstat_simil(mt, margin = "features"), 
                      textstat_simil_old(mt, margin = "features"),
                      tolerance = 0.01)

    expect_equivalent(textstat_simil(mt, selection = "1985-Reagan"), 
                      textstat_simil_old(mt, selection = "1985-Reagan"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_simil(mt, method = "cosine"), 
                      textstat_simil_old(mt, method = "cosine"),
                      tolerance = 0.01)
    
    expect_equivalent(textstat_simil(mt, method = "correlation"), 
                      textstat_simil_old(mt, method = "correlation"))
    
    expect_equivalent(textstat_simil(mt, method = "jaccard"), 
                     textstat_simil_old(mt, method = "jaccard"))
    
    expect_equivalent(textstat_simil(mt, method = "ejaccard"), 
                     textstat_simil_old(mt, method = "ejaccard"))
    
    expect_equivalent(textstat_simil(mt, method = "dice"), 
                     textstat_simil_old(mt, method = "dice"))
    
    expect_equivalent(textstat_simil(mt, method = "edice"), 
                     textstat_simil_old(mt, method = "edice"))
    
    expect_equivalent(textstat_simil(mt, method = "simple matching"), 
                     textstat_simil_old(mt, method = "simple matching"))
    
    expect_equivalent(textstat_simil(mt, method = "faith"), 
                     textstat_simil_old(mt, method = "faith"))
    
})

test_that("dist object has all the attributes", {
    d1 <- textstat_dist(mt)
    expect_equal(class(attr(d1, "Label")), "character")
    expect_equal(class(attr(d1, "Size")), "integer")
    expect_equal(class(attr(d1, "call")), "call")
    expect_equal(class(attr(d1, "Diag")), "logical")
    expect_equal(class(attr(d1, "Upper")), "logical")
    expect_equal(class(attr(d1, "method")), "character")
    
    d2 <- textstat_simil(mt)
    expect_equal(class(attr(d2, "Label")), "character")
    expect_equal(class(attr(d2, "Size")), "integer")
    expect_equal(class(attr(d2, "call")), "call")
    expect_equal(class(attr(d2, "Diag")), "logical")
    expect_equal(class(attr(d2, "Upper")), "logical")
    expect_equal(class(attr(d2, "method")), "character")
})

test_that("as.matrix.simil is consistent with the equivalent proxy function", {
    skip_if_not_installed("proxy")
    toks <- tokens(c(doc1 = "a b c d", doc2 = "c d e f"), remove_punct = TRUE)
    mt <- dfm(toks)
    
    expect_identical(
        textstat_simil(mt, method = "cosine") %>% quanteda:::as.matrix.simil(diag = NA),
        textstat_simil(mt, method = "cosine") %>% proxy:::as.matrix.simil(diag = NA)
    )
    expect_identical(
        textstat_simil(mt, method = "cosine") %>% quanteda:::as.matrix.simil(diag = 1.0),
        textstat_simil(mt, method = "cosine") %>% proxy:::as.matrix.simil(diag = 1.0)
    )
    expect_identical(
        textstat_simil(mt) %>% 
            quanteda:::as.matrix.simil() %>%
            diag(),
        c(doc1 = 1, doc2 = 1)
    )
    expect_identical(
        textstat_simil(mt) %>% 
            quanteda:::as.matrix.simil(diag = NA) %>%
            diag(),
        c(doc1 = as.numeric(NA), doc2 = as.numeric(NA))
    )
})

test_that("selection takes integer or logical vector", {
    expect_equivalent(textstat_simil(mt, c(2, 5), margin = "features"),
                      textstat_simil(mt, c("mr", "president"), margin = "features"))
    l1 <- featnames(mt) %in% c("mr", "president")
    expect_equivalent(textstat_simil(mt, l1, margin = "features"),
                      textstat_simil(mt, c("mr", "president"), margin = "features"))
    
    expect_error(textstat_simil(mt, "xxxx", margin = "features"))
    expect_error(textstat_simil(mt, 1000, margin = "features"))
    
    expect_equivalent(textstat_simil(mt, c(2,4), margin = "documents"),
                      textstat_simil(mt, c("1985-Reagan", "1993-Clinton"), margin = "documents"))
    l2 <- docnames(mt) %in% c("1985-Reagan", "1993-Clinton")
    expect_equivalent(textstat_simil(mt, l2, margin = "documents"),
                      textstat_simil(mt, c("1985-Reagan", "1993-Clinton"), margin = "documents"))
    
    expect_error(textstat_simil(mt, "nothing", margin = "documents"))
    expect_error(textstat_simil(mt, 100, margin = "documents"))
    
    
    expect_equivalent(textstat_dist(mt, c(2, 5), margin = "features"),
                      textstat_dist(mt, c("mr", "president"), margin = "features"))
    l3 <- featnames(mt) %in% c("mr", "president")
    expect_equivalent(textstat_dist(mt, l3, margin = "features"),
                      textstat_dist(mt, c("mr", "president"), margin = "features"))
    
    expect_error(textstat_dist(mt, "xxxx", margin = "features"))
    expect_error(textstat_dist(mt, 1000, margin = "features"))
    
    expect_equivalent(textstat_dist(mt, c(2,4), margin = "documents"),
                      textstat_dist(mt, c("1985-Reagan", "1993-Clinton"), margin = "documents"))
    l4 <- docnames(mt) %in% c("1985-Reagan", "1993-Clinton")
    expect_equivalent(textstat_dist(mt, l4, margin = "documents"),
                      textstat_dist(mt, c("1985-Reagan", "1993-Clinton"), margin = "documents"))
    
    expect_error(textstat_dist(mt, "nothing", margin = "documents"))
    expect_error(textstat_dist(mt, 100, margin = "documents"))
})

test_that("textstat_dist() returns NA for empty dfm", {
    skip("Skip until textstat_dist() has been corrected for empty dfms")
    mt <- dfm_trim(data_dfm_lbgexample, 1000)
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "euclidean"),
        stats::dist(as.matrix(mt), method = "euclidean")
    )
    expect_equivalent(
        textstat_dist(mt, method = "kullback"),
        proxy::dist(as.matrix(mt), method = "kullback")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "manhattan"),
        stats::dist(as.matrix(mt), method = "manhattan")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "maximum"),
        stats::dist(as.matrix(mt), method = "maximum")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "canberra"),
        stats::dist(as.matrix(mt), method = "canberra")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "minkowski"),
        stats::dist(as.matrix(mt), method = "minkowski", p = 2)
    )
})

test_that("textstat_simil() returns NA for empty dfm", {
    skip("Skip until textstat_simil() has been corrected for empty dfms")
    mt <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_equivalent(
        textstat_simil(mt, method = "correlation"),
        cor(t(as.matrix(mt)), method = "pearson") %>% as.dist()
    )
    expect_equivalent(
        textstat_simil(mt, method = "cosine"),
        proxy::simil(as.matrix(mt), method = "cosine")
    )
    expect_equivalent(
        textstat_simil(mt, method = "jaccard"),
        proxy::simil(as.matrix(mt), method = "jaccard")
    )
    expect_equivalent(
        textstat_simil(mt, method = "ejaccard"),
        proxy::simil(as.matrix(mt), method = "ejaccard")
    )
    expect_equivalent(
        textstat_simil(mt, method = "dice"),
        proxy::simil(as.matrix(mt), method = "dice")
    )
    expect_equivalent(
        textstat_simil(mt, method = "edice"),
        proxy::simil(as.matrix(mt), method = "edice")
    )
    expect_equivalent(
        textstat_simil(mt, method = "hamman"),
        proxy::simil(as.matrix(mt), method = "hamman")
    )
    expect_equivalent(
        textstat_simil(mt, method = "simple matching"),
        proxy::simil(as.matrix(mt), method = "simple matching")
    )
    expect_equivalent(
        textstat_simil(mt, method = "faith"),
        proxy::simil(as.matrix(mt), method = "faith")
    )
})

test_that("textstat_dist() returns NA for zero-variance documents", {
    skip("skip until textstat_dist() works correctly for zero-variance documents")
    mt <- data_dfm_lbgexample[1:5, 1:20]
    mt[1:2, ] <- 0
    mt[3:4, ] <- 1
    mt <- as.dfm(mt)

    expect_equivalent(
        textstat_dist(mt, method = "euclidean"),
        stats::dist(as.matrix(mt), method = "euclidean")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "kullback"),
        proxy::dist(as.matrix(mt), method = "kullback")
    )
    expect_equivalent(
        textstat_dist(mt, method = "manhattan"),
        stats::dist(as.matrix(mt), method = "manhattan")
    )
    expect_equivalent(
        textstat_dist(mt, method = "maximum"),
        stats::dist(as.matrix(mt), method = "maximum")
    )
    # fails
    expect_equivalent(
        textstat_dist(mt, method = "canberra"),
        stats::dist(as.matrix(mt), method = "canberra")
    )
    expect_equivalent(
        textstat_dist(mt, method = "minkowski"),
        stats::dist(as.matrix(mt), method = "minkowski", p = 2)
    )
})

test_that("textstat_simil() returns NA for zero-variance documents", {
    skip("skip until textstat_simil() works correctly for zero-variance documents")
    mt <- data_dfm_lbgexample[1:5, 1:20]
    mt[1:2, ] <- 0
    mt[3:4, ] <- 1
    mt <- as.dfm(mt)

    expect_equivalent(
        textstat_simil(mt, method = "correlation") %>% as.matrix(),
        suppressWarnings(stats::cor(t(as.matrix(mt)), method = "pearson"))
    )
    # fails
    expect_equal(
        textstat_simil(mt, method = "cosine"),
        matrix(c(rep(NaN, 13), 1, rep(NaN, 3), 1, rep(NaN, 7)), nrow = 5,
                 dimnames = list(paste0("R", 1:5), paste0("R", 1:5))) %>%
            as.dist()
    )
    # fails
    expect_equivalent(
        textstat_simil(mt, method = "jaccard"),
        proxy::simil(as.matrix(mt), method = "jaccard")
    )
    # fails
    expect_equivalent(
        textstat_simil(mt, method = "ejaccard"),
        proxy::simil(as.matrix(mt), method = "ejaccard")
    )
    # fails
    expect_equivalent(
        textstat_simil(mt, method = "dice"),
        proxy::simil(as.matrix(mt), method = "dice")
    )
    # fails
    expect_equal(
        textstat_simil(mt, method = "edice"),
        # proxy has this wrong, since 2xy / (xx + yy) means that if 
        # both x and y are empty then this should be NaN
        proxy::simil(as.matrix(mt), method = "edice")
    )
    expect_equivalent(
        textstat_simil(mt, method = "hamman"),
        proxy::simil(as.matrix(mt), method = "hamman")
    )
    expect_equivalent(
        textstat_simil(mt, method = "simple matching"),
        proxy::simil(as.matrix(mt), method = "simple matching")
    )
    expect_equivalent(
        textstat_simil(mt, method = "faith"),
        proxy::simil(as.matrix(mt), method = "faith")
    )
})

test_that("selection is always on colums (#1549)", {
    mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
    expect_equal(
        colnames(textstat_simil(mt, margin = "documents", 
                                selection = c("1985-Reagan", "1989-Bush"))), 
        c("1985-Reagan", "1989-Bush")
    )
    expect_equal(
        colnames(textstat_simil(mt, margin = "documents", selection = c(2, 3))), 
        c("1985-Reagan", "1989-Bush")
    )
    expect_equal(
        colnames(textstat_simil(mt, margin = "features", selection = c("justice", "and"))), 
        c("justice", "and")
    )
    expect_equal(
        colnames(textstat_simil(mt, margin = "features", selection = c(4, 6))), 
        c("mr", "chief")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "documents", 
                               selection = c("1985-Reagan", "1989-Bush"))), 
        c("1985-Reagan", "1989-Bush")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "documents", selection = c(2, 3))), 
        c("1985-Reagan", "1989-Bush")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "features", selection = c("justice", "and"))), 
        c("justice", "and")
    )
    expect_equal(
        colnames(textstat_dist(mt, margin = "features", selection = c(4, 6))), 
        c("mr", "chief")
    )
})
