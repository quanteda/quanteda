context("test dfm_trim")

test_that("dfm_trim", {
    
    mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900 & Year < 2017)
    preDictDfm <- dfm(mycorpus, remove_punct = TRUE, remove_numbers = TRUE, remove_hyphens = FALSE)
    
    nfeature(dfm_trim(preDictDfm, min_count = 7))
    nfeature(dfm_trim(preDictDfm, min_count = 0.001))
    
    expect_equal(nfeature(dfm_trim(preDictDfm, min_count = 0.001)), 1045)
    expect_equal(nfeature(dfm_trim(preDictDfm, min_count = 7)), 1045)
    
    expect_equal(nfeature(dfm_trim(preDictDfm, min_docfreq = 0.05)), 3077)
    expect_equal(nfeature(dfm_trim(preDictDfm, min_docfreq = 2)), 3077)
    
    expect_equal(nfeature(dfm_trim(preDictDfm, sparsity = 0.95)), 3077)
    expect_equal(nfeature(dfm_trim(preDictDfm, sparsity = 0.95)), nfeature(dfm_trim(preDictDfm, min_docfreq = 0.05)))
    expect_equal(nfeature(dfm_trim(preDictDfm, min_docfreq = 0.05)), 3077)
    
})

test_that("dfm_trim works as expected", {
    mydfm <- dfm(c("This is a sentence.", "This is a second sentence.", "Third sentence.", "Fouth sentence.", "Fifth sentence."))
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "Removing features occurring:")
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "fewer than 2 times: 4")
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "in fewer than 2 documents: 4")
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "  Total features removed: 4 \\(44.4%\\).")
})

test_that("dfm_trim works as expected", {
    mydfm <- dfm(c("This is a sentence.", "This is a second sentence.", "Third sentence.", "Fouth sentence.", "Fifth sentence."))
    expect_message(dfm_trim(mydfm, max_count =2, max_docfreq=2, verbose=T),
                   regexp = "more than 2 times: 2")
    expect_message(dfm_trim(mydfm, max_count =2, max_docfreq=2, verbose=T),
                   regexp = "in more than 2 documents: 2")
    
    expect_message(dfm_trim(mydfm, max_count =5, max_docfreq=5, verbose=T),
                   regexp = "No features removed.")
})

test_that("dfm_trim works without trimming arguments #509", {
    mydfm <- dfm(c("This is a sentence.", "This is a second sentence.", "Third sentence."))
    expect_equal(dim(mydfm[-2, ]), c(2, 7))
    expect_equal(dim(dfm_trim(mydfm[-2, ], verbose = FALSE)), c(2, 6))
})

test_that("dfm_trim doesn't break because of duplicated feature names (#829)", {
    mydfm <- dfm(c(d1 = "a b c d e", d2 = "a a b b e f", d3 = "b c e e f f f"))
    colnames(mydfm)[3] <- "b"
    expect_equal(
        as.matrix(dfm_trim(mydfm, min_count = 1)),
        matrix(c(1,1,1,1,1,0, 2,2,0,0,1,1, 0,1,1,0,2,3), byrow = TRUE, nrow = 3,
               dimnames = list(docs = c("d1", "d2", "d3"), features = c(letters[c(1,2,2,4:6)])))
    )
    expect_equal(
        as.matrix(dfm_trim(mydfm, min_count = 2)),
        matrix(c(1,1,1,1,0, 2,2,0,1,1, 0,1,1,2,3), byrow = TRUE, nrow = 3,
               dimnames = list(docs = c("d1", "d2", "d3"), features = c(letters[c(1,2,2,5:6)])))
    )
    expect_equal(
        as.matrix(dfm_trim(mydfm, min_count = 3)),
        matrix(c(1,1,1,0, 2,2,1,1, 0,1,2,3), byrow = TRUE, nrow = 3,
               dimnames = list(docs = c("d1", "d2", "d3"), features = c(letters[c(1,2,5:6)])))
    )
})



