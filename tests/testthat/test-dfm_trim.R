test_that("dfm_trim works", {
    mydfm <- dfm(tokens(c(d1 = "a b c d e", d2 = "a a b b e f", d3 = "b c e e f f f")))
    s <- sum(mydfm)
    
    expect_equal(nfeat(dfm_trim(mydfm, min_termfreq = 0.5, termfreq_type = "quantile")), 4)
    expect_equal(nfeat(dfm_trim(mydfm, min_termfreq = 3 / s, termfreq_type = "prop")), 4)
    expect_equal(nfeat(dfm_trim(mydfm, min_termfreq = 3)), 4)
    
    expect_equal(nfeat(dfm_trim(mydfm, max_termfreq = 0.8, termfreq_type = "quantile")), 6)
    expect_equal(nfeat(dfm_trim(mydfm, max_termfreq = 4 / s, termfreq_type = "prop")), 6)
    expect_equal(nfeat(dfm_trim(mydfm, max_termfreq = 4)), 6)

    expect_equal(nfeat(dfm_trim(mydfm, min_docfreq = 0.5, docfreq_type = "quantile")), 5)
    expect_equal(nfeat(dfm_trim(mydfm, min_docfreq = 2 / 3, docfreq_type = "prop")), 5)
    expect_equal(nfeat(dfm_trim(mydfm, min_docfreq = 2)), 5)
    
    expect_equal(nfeat(dfm_trim(mydfm, max_docfreq = 0.5, docfreq_type = "quantile")), 4)
    expect_equal(nfeat(dfm_trim(mydfm, max_docfreq = 2 / 3, docfreq_type = "prop")), 4)
    expect_equal(nfeat(dfm_trim(mydfm, max_docfreq = 2)), 4)
    
    expect_equal(nfeat(dfm_trim(mydfm, min_termfreq = 2, termfreq_type = "rank")), 3)
    expect_equal(nfeat(dfm_trim(mydfm, min_termfreq = 4)), 3)
    
    expect_equal(nfeat(dfm_trim(mydfm, max_termfreq = 4, termfreq_type = "rank")), 3)
    expect_equal(nfeat(dfm_trim(mydfm, max_termfreq = 3)), 3)
    
    expect_equal(nfeat(dfm_trim(mydfm, min_docfreq = 1, docfreq_type = "rank")), 2)
    expect_equal(nfeat(dfm_trim(mydfm, min_docfreq = 3)), 2)
    
    expect_equal(nfeat(dfm_trim(mydfm, max_docfreq = 2, docfreq_type = "rank")), 4)
    expect_equal(nfeat(dfm_trim(mydfm, max_docfreq = 2)), 4)
    
    expect_equal(nfeat(dfm_trim(mydfm, sparsity = 0.0)), 2)
    expect_equal(nfeat(dfm_trim(mydfm, sparsity = 0.5)), 5)
    expect_equal(nfeat(dfm_trim(mydfm, sparsity = 1.0)), 6)
    
})

# test_that("dfm_trim works as expected", {
#     mydfm <- dfm(tokens(c("This is a sentence.", "This is a second sentence.", 
#                           "Third sentence.", "Fouth sentence.", "Fifth sentence.")))
#     expect_message(dfm_trim(mydfm, min_termfreq = 2, min_docfreq = 2, verbose = TRUE),
#                    regexp = "Removing features occurring:")
#     expect_message(dfm_trim(mydfm, min_termfreq = 2, min_docfreq = 2, verbose = TRUE),
#                    regexp = "fewer than 2 times: 4")
#     expect_message(dfm_trim(mydfm, min_termfreq = 2, min_docfreq = 2, verbose = TRUE),
#                    regexp = "in fewer than 2 documents: 4")
#     expect_message(dfm_trim(mydfm, min_termfreq = 2, min_docfreq = 2, verbose = TRUE),
#                    regexp = "  Total features removed: 4 \\(44.4%\\).")
# })

# test_that("dfm_trim works as expected", {
#     mydfm <- dfm(tokens(c("This is a sentence.", "This is a second sentence.", 
#                           "Third sentence.", "Fouth sentence.", "Fifth sentence.")))
#     expect_message(dfm_trim(mydfm, max_termfreq = 2, max_docfreq = 2, verbose = TRUE),
#                    regexp = "more than 2 times: 2")
#     expect_message(dfm_trim(mydfm, max_termfreq = 2, max_docfreq = 2, verbose = TRUE),
#                    regexp = "in more than 2 documents: 2")
#     
#     expect_message(dfm_trim(mydfm, max_termfreq = 5, max_docfreq = 5, verbose = TRUE),
#                    regexp = "No features removed.")
# })

test_that("dfm_trim works without trimming arguments #509", {
    mydfm <- dfm(tokens(c("This is a sentence.", "This is a second sentence.", "Third sentence.")))
    expect_equal(dim(mydfm[-2, ]), c(2, 7))
    expect_equal(dim(dfm_trim(mydfm[-2, ], verbose = FALSE)), c(2, 6))
})

test_that("dfm_trim works with duplicated feature names (#829)", {
    mydfm <- dfm(tokens(c(d1 = "a b c d e", d2 = "a a b b e f", d3 = "b c e e f f f")))
    colnames(mydfm)[3] <- "b"
    expect_equal(
        as.matrix(dfm_trim(mydfm, min_termfreq = 1)),
        matrix(c(1,1,1,1,1,0, 2,2,0,0,1,1, 0,1,1,0,2,3), byrow = TRUE, nrow = 3,
               dimnames = list(docs = c("d1", "d2", "d3"), 
                               features = c(letters[c(1,2,2,4:6)])))
    )
    expect_equal(
        as.matrix(dfm_trim(mydfm, min_termfreq = 2)),
        matrix(c(1,1,1,1,0, 2,2,0,1,1, 0,1,1,2,3), byrow = TRUE, nrow = 3,
               dimnames = list(docs = c("d1", "d2", "d3"), 
                               features = c(letters[c(1,2,2,5:6)])))
    )
    # expect_equal(
    #     as.matrix(dfm_trim(mydfm, min_termfreq = 3)),
    #     matrix(c(1,1,1,0, 2,2,1,1, 0,1,2,3), byrow = TRUE, nrow = 3,
    #            dimnames = list(docs = c("d1", "d2", "d3"), 
    #                            features = c(letters[c(1,2,5:6)])))
    # )
})

test_that("dfm_trim works with min_termfreq larger than total number (#1181)", {
    testdfm <- dfm(tokens(c(d1 = "a a a a b b", d2 = "a b b c")))
    expect_equal(dimnames(dfm_trim(testdfm, min_termfreq = 6)), 
                list(docs = c("d1", "d2"), features = character())
    )
    expect_equal(dimnames(dfm_trim(testdfm, min_docfreq = 3)), 
                 list(docs = c("d1", "d2"), features = character())
    )

})

test_that("dfm_trim works on previously weighted dfms (#1237)", {
    dfm1 <- dfm(tokens(c("the quick brown fox jumps over the lazy dog", 
                  "the quick brown foxy ox jumps over the lazy god")))
    dfm2 <- dfm_tfidf(dfm1)
    expect_equal(
        suppressWarnings(
            as.matrix(dfm_trim(dfm2, min_termfreq = 0, min_docfreq = .5, 
                               termfreq_type = "prop", docfreq_type = "prop"))
        ),
        matrix(c(.30103, 0, .30103, 0, 0, .30103, 0, .30103, 0, .30103), nrow = 2,
               dimnames = list(docs = c("text1", "text2"), 
                               features = c("fox", "dog", "foxy", "ox", "god"))),
        tol = .0001
    )
    suppressWarnings(expect_warning(
        dfm_trim(dfm2, min_docfreq = .5, docfreq_type = "prop"),
        "dfm has been previously weighted"
    ))
    suppressWarnings(expect_warning(
        dfm_trim(dfm2, min_termfreq = 1, min_docfreq = .5, docfreq_type = "prop"),
        "dfm has been previously weighted"
    ))
    
    expect_equal(
        dim(suppressWarnings(dfm_trim(dfm2, min_termfreq = 1, min_docfreq = .5, docfreq_type = "prop"))),
        c(2, 0)
    )
})

test_that("dfm_trim error with invalid input", {
    dfmat <- dfm(tokens(c("the quick brown fox jumps over the lazy dog", 
                  "the quick brown foxy ox jumps over the lazy god")))
    
    # min_termfreq
    expect_error(
        dfm_trim(dfmat, min_termfreq = -1, termfreq_type = "count"),
        "The value of min_termfreq must be between 0 and Inf"
    )
    expect_error(
        dfm_trim(dfmat, min_termfreq = 1.1, termfreq_type = "prop"),
        "The value of min_termfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, min_termfreq = 1.1, termfreq_type = "quantile"),
        "The value of min_termfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, min_termfreq = 0, termfreq_type = "rank"),
        "The value of min_termfreq must be between 1 and Inf"
    )
    
    # max_termfreq
    expect_error(
        dfm_trim(dfmat, max_termfreq = -1, termfreq_type = "count"),
        "The value of max_termfreq must be between 0 and Inf"
    )
    expect_error(
        dfm_trim(dfmat, max_termfreq = 1.1, termfreq_type = "prop"),
        "The value of max_termfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, max_termfreq = 1.1, termfreq_type = "quantile"),
        "The value of max_termfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, max_termfreq = 0, termfreq_type = "rank"),
        "The value of max_termfreq must be between 1 and Inf"
    )
    
    # min_docfreq
    expect_error(
        dfm_trim(dfmat, min_docfreq = -1, docfreq_type = "count"),
        "The value of min_docfreq must be between 0 and Inf"
    )
    expect_error(
        dfm_trim(dfmat, min_docfreq = 1.1, docfreq_type = "prop"),
        "The value of min_docfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, min_docfreq = 1.1, docfreq_type = "quantile"),
        "The value of min_docfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, min_docfreq = 0, docfreq_type = "rank"),
        "The value of min_docfreq must be between 1 and Inf"
    )
    
    # max_docfreq
    expect_error(
        dfm_trim(dfmat, max_docfreq = -1, docfreq_type = "count"),
        "The value of max_docfreq must be between 0 and Inf"
    )
    expect_error(
        dfm_trim(dfmat, max_docfreq = 1.1, docfreq_type = "prop"),
        "The value of max_docfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, max_docfreq = 1.1, docfreq_type = "quantile"),
        "The value of max_docfreq must be between 0 and 1"
    )
    expect_error(
        dfm_trim(dfmat, max_docfreq = 0, docfreq_type = "rank"),
        "The value of max_docfreq must be between 1 and Inf"
    )
})

test_that("dfm_trim() verbose works", {
    dfmat <- dfm(tokens(data_corpus_inaugural[1:2]))
    expect_message(
        dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 2, verbose = TRUE),
        "dfm_trim() changed from 635 features (2 documents) to 19 features (2 documents)",
        fixed = TRUE
    )
})
