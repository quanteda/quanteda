context('test textstat_dist.R')

# euclidean 
test_that("test textstat_dist method = \"euclidean\" against proxy dist() and stats dist(): features", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    eucQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet", method = "euclidean", margin = "features"))[,"soviet"], 2)
    eucQuanteda <- eucQuanteda[order(names(eucQuanteda))]
    eucQuanteda <- eucQuanteda[-which(names(eucQuanteda) == "soviet")]
    
    eucProxy <- round(drop(proxy::dist(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "euclidean", by_rows = FALSE)), 2)
    eucProxy <- eucProxy[order(names(eucProxy))]
    eucProxy <- eucProxy[-which(names(eucProxy) == "soviet")]
    
    expect_equal(eucQuanteda, eucProxy)
})

test_that("test textstat_dist method = \"Euclidean\" against proxy dist() and stats dist(): documents", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    eucQuanteda <- sort(round(as.matrix(textstat_dist(presDfm, method = "euclidean", margin = "documents"))[,"1981-Reagan"], 6), decreasing = FALSE)
    eucProxy <- sort(round(as.matrix(proxy::dist(as.matrix(presDfm), "euclidean", diag = FALSE, upper = FALSE, p = 2))[, "1981-Reagan"], 6), decreasing = FALSE)
    eucStats <- sort(round(as.matrix(stats::dist(as.matrix(presDfm), method = "euclidean", diag = FALSE, upper = FALSE, p = 2))[,"1981-Reagan"], 6),  decreasing = FALSE)
    expect_equal(eucQuanteda, eucProxy)
    expect_equal(eucQuanteda, eucStats)
    
    # Only calculate distance on the selections
    eucQuanteda <- sort(round(as.matrix(textstat_dist(presDfm, "1981-Reagan", method = "euclidean", margin = "documents"))[,"1981-Reagan"], 6), decreasing = FALSE)
    expect_equal(eucQuanteda, eucProxy)
})

# Chi-squared distance 
# Instead of comparing to Proxy package, ExPosition is compared to. Because Proxy::simil uses different formula
# eucProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "Chi-squared", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
test_that("test textstat_dist method = \"Chi-squred\" against ExPosition::chi2Dist(): features", {
    skip_if_not_installed("ExPosition")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    chiQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet", method = "Chisquared", margin = "features"))[,"soviet"], 2)
    chiQuanteda <- chiQuanteda[order(names(chiQuanteda))]
    chiQuanteda <- chiQuanteda[-which(names(chiQuanteda) == "soviet")]
    
    chiExp <- ExPosition::chi2Dist(t(as.matrix(presDfm)))
    chiExp <- sort(round(as.matrix(chiExp$D)[, "soviet"], 2), decreasing = FALSE)
    chiExp <- chiExp[order(names(chiExp))]
    chiExp <- chiExp[-which(names(chiExp) == "soviet")]
    
    expect_equal(chiQuanteda, chiExp)
})

test_that("test textstat_dist method = \"Chi-squred\" against ExPosition::chi2Dist(): documents", {
    skip_if_not_installed("ExPosition")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    chiQuanteda <- sort(round(as.matrix(textstat_dist(presDfm, method = "Chisquared", margin = "documents"))[,"1981-Reagan"], 6), decreasing = FALSE)
    chiExp <- ExPosition::chi2Dist(as.matrix(presDfm))
    chiExp <- sort(round(as.matrix(chiExp$D)[, "1981-Reagan"], 6), decreasing = FALSE)
    expect_equal(chiQuanteda, chiExp)
})

# Kullback-Leibler divergence
# proxy::dist() will generate NA for matrix with zeros, hence a matrix only with non-zero entries is used here.
test_that("test textstat_dist method = \"Kullback-Leibler\" against proxy dist(): documents", {
    skip_if_not_installed("proxy")
    m <- matrix(rexp(550, rate=.1), nrow = 5)
    kullQuanteda <- round(as.matrix(textstat_dist(as.dfm(m), method = "kullback", margin = "documents")), 2)
    kullProxy <- round(as.matrix(proxy::dist(m, "kullback", diag = FALSE, upper = FALSE)), 2)
    expect_equal(kullQuanteda, kullProxy)
    
    rownames(m) <- c("a", "b", "c", "d", "e")
    mydfm <- new("dfmSparse", Matrix::Matrix(as.matrix(m), sparse=TRUE, dimnames = list(docs = rownames(m), features=colnames(m))))
    kullQuanteda <- round(as.matrix(textstat_dist(mydfm, "a", method = "kullback", margin = "documents"))["a",], 2)
    kullProxy <- round(drop(proxy::dist(as.matrix(mydfm), as.matrix(mydfm[1, ]), "kullback", diag = FALSE, upper = FALSE)), 2)
    kullProxy <- kullProxy[order(names(kullProxy))]
    expect_equivalent(kullQuanteda, kullProxy)
})

# Manhattan distance
test_that("test textstat_dist method = \"manhattan\" against proxy dist() : documents", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    manQuanteda <- round(as.matrix(textstat_dist(presDfm, method = "manhattan", margin = "documents")), 2)
    manProxy <- round(as.matrix(proxy::dist(as.matrix(presDfm), "manhattan", diag = FALSE, upper = FALSE)), 2)
    expect_equal(manQuanteda, manProxy)
})

test_that("test textstat_dist method = \"manhattan\" against proxy dist() : features", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    manQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet",  method = "manhattan", margin = "features"))[,"soviet"], 2)
    manQuanteda <- manQuanteda[order(names(manQuanteda))]
    manQuanteda <- manQuanteda[-which(names(manQuanteda) == "soviet")]
    
    manProxy <- round(drop(proxy::dist(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "manhattan", by_rows = FALSE)), 2)
    manProxy <- manProxy[order(names(manProxy))]
    manProxy <- manProxy[-which(names(manProxy) == "soviet")]
    expect_equal(manQuanteda, manProxy)
})

# Maximum/Supremum distance
test_that("test textstat_dist method = \"Maximum\" against proxy dist() : documents", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    maxQuanteda <- round(as.matrix(textstat_dist(presDfm, method = "maximum", margin = "documents")), 2)
    maxProxy <- round(as.matrix(proxy::dist(as.matrix(presDfm), "maximum", diag = FALSE, upper = FALSE)), 2)
    expect_equal(maxQuanteda, maxProxy)
})

test_that("test textstat_dist method = \"Maximum\" against proxy dist() : features", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    maxQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet",  method = "maximum", margin = "features"))[,"soviet"], 2)
    maxQuanteda <- maxQuanteda[order(names(maxQuanteda))]
    maxQuanteda <- maxQuanteda[-which(names(maxQuanteda) == "soviet")]
    
    maxProxy <- round(drop(proxy::dist(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "maximum", by_rows = FALSE)), 2)
    maxProxy <- maxProxy[order(names(maxProxy))]
    maxProxy <- maxProxy[-which(names(maxProxy) == "soviet")]
    expect_equal(maxQuanteda, maxProxy)
})

# Canberra distance
test_that("test textstat_dist method = \"Canberra\" against proxy dist() : documents", {
    
    skip_if_not_installed("proxy")
    skip_on_os("solaris")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    canQuanteda <- round(as.matrix(textstat_dist(presDfm, method = "canberra", margin = "documents")), 2)
    canProxy <- round(as.matrix(proxy::dist(as.matrix(presDfm), "canberra", diag = FALSE, upper = FALSE)), 2)
    expect_equal(canQuanteda, canProxy)
})

test_that("test textstat_dist method = \"Canberra\" against proxy dist() : features", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980 & Year < 2017), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    canQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet",  method = "canberra", margin = "features"))[,"soviet"], 2)
    canQuanteda <- canQuanteda[order(names(canQuanteda))]
    canQuanteda <- canQuanteda[-which(names(canQuanteda) == "soviet")]
    
    canProxy <- round(drop(proxy::dist(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "canberra", by_rows = FALSE)), 2)
    canProxy <- canProxy[order(names(canProxy))]
    canProxy <- canProxy[-which(names(canProxy) == "soviet")]
    expect_equal(canQuanteda, canProxy)
})

# Minkowski distance
test_that("test textstat_dist method = \"Minkowski\" against proxy dist() : documents", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    minkQuanteda <- round(as.matrix(textstat_dist(presDfm, method = "minkowski", margin = "documents", p = 3)), 2)
    minkProxy <- round(as.matrix(proxy::dist(as.matrix(presDfm), "minkowski", diag = FALSE, upper = FALSE, p=3)), 2)
    expect_equal(minkQuanteda, minkProxy)
})

test_that("test textstat_dist method = \"Canberra\" against proxy dist() : features", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    minkQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet",  method = "minkowski", margin = "features", p = 4))[,"soviet"], 2)
    minkQuanteda <- minkQuanteda[order(names(minkQuanteda))]
    minkQuanteda <- minkQuanteda[-which(names(minkQuanteda) == "soviet")]
    
    minkProxy <- round(drop(proxy::dist(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "minkowski", by_rows = FALSE, p = 4)), 2)
    minkProxy <- minkProxy[order(names(minkProxy))]
    minkProxy <- minkProxy[-which(names(minkProxy) == "soviet")]
    expect_equal(minkQuanteda, minkProxy)
})

# Hamming distance
test_that("test textstat_dist method = \"hamming\" against e1071::hamming.distance: documents", {
    skip_if_not_installed("e1071")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    hammingQuanteda <- sort(as.matrix(textstat_dist(presDfm, "1981-Reagan", method = "hamming", margin = "documents", upper = TRUE))[,"1981-Reagan"], decreasing = FALSE)
    hammingQuanteda <- hammingQuanteda[-which(names(hammingQuanteda) == "1981-Reagan")]
    
    if (requireNamespace("e1071", quietly = TRUE)){
        hammingE1071 <- sort(e1071::hamming.distance(as.matrix(tf(presDfm, "boolean")))[, "1981-Reagan"], decreasing = FALSE)
        if("1981-Reagan" %in% names(hammingE1071)) hammingE1071 <- hammingE1071[-which(names(hammingE1071) == "1981-Reagan")]
    } else {
        hammingE1071 <- c(711, 724, 745, 766, 766, 778, 785, 804, 851)
    }
    expect_equivalent(hammingQuanteda, hammingE1071)
})

test_that("test textstat_dist method = \"hamming\" against e1071::hamming.distance: features", {
    skip_if_not_installed("e1071")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    hammingQuanteda <- as.matrix(textstat_dist(presDfm, "soviet", method = "hamming", margin = "features"))[,"soviet"]
    hammingQuanteda <- hammingQuanteda[order(names(hammingQuanteda))]
    hammingQuanteda <- hammingQuanteda[-which(names(hammingQuanteda) == "soviet")]
    
    presM <- t(as.matrix(tf(presDfm, "boolean")))
    hammingE1071 <- e1071::hamming.distance(presM)[, "soviet"]
    hammingE1071 <- hammingE1071[order(names(hammingE1071))]
    if("soviet" %in% names(hammingE1071)) hammingE1071 <- hammingE1071[-which(names(hammingE1071) == "soviet")]
    expect_equal(hammingQuanteda, hammingE1071)
})

test_that("test textstat_dist method = \"binary\" against proxy::simil(): features", {
    skip_if_not_installed("proxy")
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    jacQuanteda <- round(as.matrix(textstat_dist(presDfm, "soviet", method = "binary", margin = "features"))[,"soviet"], 2)
    jacQuanteda <- jacQuanteda[order(names(jacQuanteda))]
    jacQuanteda <- jacQuanteda[-which(names(jacQuanteda) == "soviet")]
    
    jacProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "binary", by_rows = FALSE)), 2)
    jacProxy <- jacProxy[order(names(jacProxy))]
    if("soviet" %in% names(jacProxy)) jacProxy <- jacProxy[-which(names(jacProxy) == "soviet")]
    
    expect_equal(jacQuanteda, jacProxy)
})

test_that("textstat_dist works as expected for selections",{
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    sim <- suppressWarnings(textstat_dist(presDfm, c("2009-Obama" , "2013-Obama"), n = 5, margin = "documents"))
    
    expect_equal(round(as.matrix(sim)["1981-Reagan","1985-Reagan"],2), 0.0)
})

test_that("as.list.dist works as expected",{
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    ddist <- textstat_dist(presDfm, method = "hamming")
    ddist_list <- as.list(ddist)
    expect_equal(names(ddist_list$`1981-Reagan`)[1:3], c("2009-Obama", "2013-Obama", "1997-Clinton"))
    expect_equivalent(ddist_list$`1981-Reagan`[1:3], c(851, 804, 785))
})

test_that("textstat_dist stops as expected for methods not supported",{
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    expect_error(textstat_dist(presDfm, method = "Yule"), 
                 "The metric is not currently supported by quanteda, please use other packages such as proxy::dist\\(\\)\\/simil\\(\\).")
})
