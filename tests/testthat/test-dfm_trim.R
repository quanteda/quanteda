context("test dfm_trim")

test_that("dfm_trim", {
    
    mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900 & Year < 2017)
    preDictDfm <- dfm(mycorpus, remove_punct = TRUE, remove_numbers = TRUE, remove_hyphens = FALSE)
    
    feat_pt_50 <- quantile(colSums(preDictDfm), 0.5, type = 1)
    expect_equal(nfeat(dfm_trim(preDictDfm, min_count = feat_pt_50)), 3326)
    expect_equal(nfeat(dfm_trim(preDictDfm, min_count = 0.5)), 3326)
    expect_equal(nfeat(dfm_trim(preDictDfm, min_count = 2)), 3326)
    
    feat_pt_80 <- quantile(colSums(preDictDfm), 0.8, type = 1)
    expect_equal(nfeat(dfm_trim(preDictDfm, max_count = feat_pt_80)), 5074)
    expect_equal(nfeat(dfm_trim(preDictDfm, max_count = 0.8)), 5074)
    expect_equal(nfeat(dfm_trim(preDictDfm, max_count = 5)), 5074)

    expect_equal(nfeat(dfm_trim(preDictDfm, min_docfreq = 0.05)), 3077)
    expect_equal(nfeat(dfm_trim(preDictDfm, min_docfreq = 2)), 3077)
    
    expect_equal(nfeat(dfm_trim(preDictDfm, sparsity = 0.95)), 3077)
    expect_equal(nfeat(dfm_trim(preDictDfm, sparsity = 0.95)), nfeat(dfm_trim(preDictDfm, min_docfreq = 0.05)))
    expect_equal(nfeat(dfm_trim(preDictDfm, min_docfreq = 0.05)), 3077)
    
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

test_that("dfm_trim works with min_count larger than total number (#1181)", {
    
    testdfm <- dfm(c(d1 = "a a a a b b", d2 = "a b b c"))
    expect_equal(dimnames(dfm_trim(testdfm, min_count = 6)), 
                list(docs = c("d1", "d2"), features = character())
    )
    expect_equal(dimnames(dfm_trim(testdfm, min_docfreq = 3)), 
                 list(docs = c("d1", "d2"), features = character())
    )

})

test_that("dfm_trim works on previously weighted dfms (#1237)", {
    dfm1 <- dfm(c("the quick brown fox jumps over the lazy dog", 
                  "the quick brown foxy ox jumps over the lazy god"))
    dfm2 <- dfm_tfidf(dfm1)
    expect_equal(
        dfm_trim(dfm2, min_count = 0, min_docfreq = .5) %>% as.matrix(),
        matrix(c(.30103, 0, .30103, 0, 0, .30103, 0, .30103, 0, .30103), nrow = 2,
               dimnames = list(docs = c("text1", "text2"), 
                               features = c("fox", "dog", "foxy", "ox", "god"))),
        tol = .0001
    )
    expect_warning(
        dfm_trim(dfm2, min_docfreq = .5),
        "dfm has been previously weighted; consider changing default min_count"
    )    
    expect_silent(dfm_trim(dfm2, min_count = 1, min_docfreq = .5))
    expect_equal(
        dim(dfm_trim(dfm2, min_count = 1, min_docfreq = .5)),
        c(2, 0)
    )
})
