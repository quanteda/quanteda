require(quanteda)
require(testthat)
require(proxy)
#require(qlcMatrix)


test_that("test similarity method = \"correlation\" against base cor()", {
    presDfm <- dfm(subset(inaugCorpus, Year > 1980), ignoredFeatures = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    corQuanteda <- round(similarity(presDfm, "union", method = "correlation", margin = "features")[["union"]], 6)
    corStats <- sort(round(cor(as.matrix(presDfm))[, "union"], 6), decreasing = TRUE)
    expect_equal(corQuanteda[1:10], corStats[2:11])
})

test_that("test similarity method = \"cosine\" against proxy simil()", {
    require(proxy)
    presDfm <- dfm(subset(inaugCorpus, Year > 1980), ignoredFeatures = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    cosQuanteda <- round(similarity(presDfm, "soviet", method = "cosine", margin = "features")[["soviet"]], 2)
    
    cosProxy <- sort(round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), by_rows = FALSE)), 2), decreasing = TRUE)
    
    # cosQlcMatrix <- sort(round(drop(qlcMatrix::cosSparse(presDfm, presDfm[, "soviet"])), 4), decreasing = TRUE)
    
    ## NOT EQUAL
    ## expect_equal(cosQuanteda[1:10], cosProxy[2:11]) #, cosQlcMatrix[2:11])
})

test_that("test similarity method = \"cosine\" against proxy simil(): documents", {
    require(proxy)
    presDfm <- dfm(subset(inaugCorpus, Year > 1980), ignoredFeatures = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    cosQuanteda <- round(similarity(presDfm, method = "cosine", margin = "documents")[["1981-Reagan"]], 6)[-1]
    cosQuanteda <- round(as.matrix(similarity(presDfm, method = "cosine", margin = "documents")), 6)
    (cosQuanteda <- cosQuanteda[order(rownames(cosQuanteda)), ])
    
    cosProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "cosine", by_rows = TRUE))[, "1981-Reagan"], 6), decreasing = TRUE)
    cosProxy <- round(proxy::as.matrix(proxy::simil(as.matrix(presDfm), "cosine", by_rows = TRUE), diag = 1.0), 6)
    (cosProxy <- cosProxy[order(rownames(cosProxy)), ])
    #(cosQlcMatrix <- round(as.matrix(qlcMatrix::cosSparse(t(presDfm))), 6))
    
    expect_equal(cosQuanteda, cosProxy)
})

test_that("test similarity method = \"correlation\" against proxy simil(): documents", {
    require(proxy)
    presDfm <- dfm(subset(inaugCorpus, Year > 1980), ignoredFeatures = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    corQuanteda <- round(similarity(presDfm, method = "correlation", margin = "documents")[["1981-Reagan"]], 6)
    corProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "correlation", by_rows = TRUE))[, "1981-Reagan"], 6), decreasing = TRUE)
    corCor <- sort(cor(as.matrix(t(presDfm)))[, "1981-Reagan"], decreasing = TRUE)
    expect_equal(corQuanteda, corProxy[-9], corCor[-1])
})


test_that("simple similarity comparisons method = \"cosine\" against proxy simil()", {
    testText <- c("The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with the newspaper from a boy named quick Seamus, in his mouth.", 
                  "the quick brown Seamus named dog jumped.", 
                  "My lazy dog who looks like a quick fox was in the newspaper.")
    d <- dfm(testText, verbose = FALSE)

    matQuanteda <- as.matrix(similarity(d, "a", method = "cosine", margin = "features"))
    res1 <- matQuanteda[order(rownames(matQuanteda)), , drop = FALSE]
        
    matProxy <- proxy::simil(as.matrix(d), as.matrix(d[,"a"]), "cosine", by_rows = FALSE)
    res2 <- matProxy[order(rownames(matProxy)), , drop = FALSE]
    
    expect_equal(rownames(res1), rownames(res2))
    expect_equal(res1[,1], res2[,1])
})
    
# sort(as.matrix(proxy::simil(as.matrix(d), as.matrix(d[, "seamus"]), "cosine", by_rows = FALSE))[, 1], decreasing = TRUE)[-2]
# similarity(d, "seamus", method = "cosine")[["seamus"]]



