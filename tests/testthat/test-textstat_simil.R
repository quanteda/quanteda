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
