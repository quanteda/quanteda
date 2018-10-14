context("test quanteda:::textstat_proxy.R")

mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
mt <- dfm_trim(mt, min_termfreq = 10)

test_that("test old and new textstat_dist are the same", {

    expect_identical(textstat_dist(mt), 
                     textstat_dist_old(mt))
    
    expect_identical(textstat_dist(mt, margin = "features"), 
                     textstat_dist_old(mt, margin = "features"))
    
    
    expect_identical(textstat_dist(mt, selection = "1985-Reagan"), 
                     textstat_dist_old(mt, selection = "1985-Reagan"))
    
    expect_identical(textstat_dist(mt, method = "euclidean"), 
                     textstat_dist_old(mt, method = "euclidean"))
    
    expect_identical(textstat_dist(mt, method = "euclidean"), 
                     textstat_dist_old(mt, method = "euclidean"))
    
    expect_identical(textstat_dist(mt, method = "kullback"), 
                     textstat_dist_old(mt, method = "kullback"))
    
    expect_identical(textstat_dist(mt, method = "manhattan"), 
                     textstat_dist_old(mt, method = "manhattan"))
    
    expect_identical(textstat_dist(mt, method = "maximum"), 
                     textstat_dist_old(mt, method = "maximum"))
    
    expect_identical(textstat_dist(mt, method = "canberra"), 
                     textstat_dist_old(mt, method = "canberra"))
    
    expect_identical(textstat_dist(mt, method = "minkowski"), 
                     textstat_dist_old(mt, method = "minkowski"))
    
    expect_identical(textstat_dist(mt, method = "minkowski", p = 3), 
                     textstat_dist_old(mt, method = "minkowski", p = 3))
    
})


test_that("test old and new textstat_simil are the same", {
    
    expect_identical(textstat_simil(mt), 
                     textstat_simil_old(mt))
    
    expect_identical(textstat_simil(mt, margin = "features"), 
                     textstat_simil_old(mt, margin = "features"))
    
    
    expect_identical(textstat_simil(mt, selection = "1985-Reagan"), 
                         textstat_simil_old(mt, selection = "1985-Reagan"))
    
    expect_identical(textstat_simil(mt, method = "cosine"), 
                     textstat_simil_old(mt, method = "cosine"))
    
    expect_identical(textstat_simil(mt, method = "correlation"), 
                     textstat_simil_old(mt, method = "correlation"))
    
    expect_identical(textstat_simil(mt, method = "jaccard"), 
                     textstat_simil_old(mt, method = "jaccard"))
    
    expect_identical(textstat_simil(mt, method = "ejaccard"), 
                     textstat_simil_old(mt, method = "ejaccard"))
    
    expect_identical(textstat_simil(mt, method = "dice"), 
                     textstat_simil_old(mt, method = "dice"))
    
    expect_identical(textstat_simil(mt, method = "edice"), 
                     textstat_simil_old(mt, method = "edice"))
    
    expect_identical(textstat_simil(mt, method = "hamman"), 
                     textstat_simil_old(mt, method = "hamman"))
    
    expect_identical(textstat_simil(mt, method = "simple matching"), 
                     textstat_simil_old(mt, method = "simple matching"))
    
    expect_identical(textstat_simil(mt, method = "faith"), 
                     textstat_simil_old(mt, method = "faith"))
    
})
