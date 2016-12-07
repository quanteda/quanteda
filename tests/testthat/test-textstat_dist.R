require(quanteda)
require(testthat)
require(proxy)
#require(ExPosition)
#require(qlcMatrix)

# correlation
test_that("test textstat_simil method = \"correlation\" against proxy simil(): documents", {
    require(proxy)
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    corQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, method = "correlation", margin = "documents"))[,"1981-Reagan"], 6), decreasing = TRUE)
    corProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), by_rows = TRUE, diag = TRUE))[, "1981-Reagan"], 6), decreasing = TRUE)
    corCor <- sort(cor(as.matrix(t(presDfm)))[, "1981-Reagan"], decreasing = TRUE)
    expect_equal(corQuanteda[-9], corProxy[-9], corCor[-1])
})

test_that("test textstat_simil method = \"correlation\" against base cor(): features (allow selection)", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    corQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "union", method = "correlation", margin = "features"))[,"union"], 6), decreasing = TRUE)
    corStats <- sort(round(cor(as.matrix(presDfm))[, "union"], 6), decreasing = TRUE)
    expect_equal(corQuanteda[1:10], corStats[2:11])
})

# cosine
test_that("test textstat_simil method = \"cosine\" against proxy simil(): features", {
    require(proxy)
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    cosQuanteda <- round(as.matrix(textstat_simil(presDfm, method = "cosine", margin = "features"))[,"soviet"], 2)
    cosQuanteda <- cosQuanteda[order(names(cosQuanteda))]
    cosQuanteda <- cosQuanteda[-which(names(cosQuanteda) == "soviet")]
    
    cosProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "cosine", by_rows = FALSE)), 2)
    cosProxy <- cosProxy[order(names(cosProxy))]
    cosProxy <- cosProxy[-which(names(cosProxy) == "soviet")]
    
    expect_equal(cosQuanteda, cosProxy)
})


test_that("test textstat_simil method = \"cosine\" against proxy simil(): documents", {
    require(proxy)
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    cosQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, method = "cosine", margin = "documents"))[,"1981-Reagan"], 6), decreasing = TRUE)
    cosProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "cosine", by_rows = TRUE, diag = TRUE))[, "1981-Reagan"], 6), decreasing = TRUE)
    expect_equal(cosQuanteda[-9], cosProxy[-9])
})

# euclidean 
test_that("test textstat_dist method = \"euclidean\" against proxy dist() and stats dist(): features", {
    require(proxy)
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
    require(proxy)
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    eucQuanteda <- sort(round(as.matrix(textstat_dist(presDfm, method = "euclidean", margin = "documents"))[,"1981-Reagan"], 6), decreasing = FALSE)
    eucProxy <- sort(round(as.matrix(proxy::dist(as.matrix(presDfm), "euclidean", diag = FALSE, upper = FALSE, p = 2))[, "1981-Reagan"], 6), decreasing = FALSE)
    eucStats <- sort(round(as.matrix(dist(as.matrix(presDfm), method = "euclidean", diag = FALSE, upper = FALSE, p = 2))[,"1981-Reagan"], 6),  decreasing = FALSE)
    expect_equal(eucQuanteda, eucProxy, eucStats)
})

# jaccard - binary
test_that("test textstat_simil method = \"jaccard\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    jacQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "jaccard", margin = "documents", diag= FALSE, tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    jacQuanteda <- jacQuanteda[-which(names(jacQuanteda) == "1981-Reagan")]
    jacProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "jaccard", diag = FALSE, upper = FALSE, p = 2))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(jacProxy)) jacProxy <- jacProxy[-which(names(jacProxy) == "1981-Reagan")]
    expect_equal(jacQuanteda, jacProxy)
})

test_that("test textstat_simil method = \"jaccard\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    jacQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "jaccard", margin = "features"))[,"soviet"], 2)
    jacQuanteda <- jacQuanteda[order(names(jacQuanteda))]
    jacQuanteda <- jacQuanteda[-which(names(jacQuanteda) == "soviet")]
    
    jacProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "jaccard", by_rows = FALSE)), 2)
    jacProxy <- jacProxy[order(names(jacProxy))]
    if("soviet" %in% names(jacProxy)) jacProxy <- jacProxy[-which(names(jacProxy) == "soviet")]
    
    expect_equal(jacQuanteda, jacProxy)
})

# ejaccard - real-valued data
test_that("test textstat_simil method = \"ejaccard\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    ejacQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "eJaccard", margin = "documents", tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    ejacQuanteda <- ejacQuanteda[-which(names(ejacQuanteda) == "1981-Reagan")]
    ejacProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "ejaccard", diag = FALSE, upper = FALSE, p = 2))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(ejacProxy)) ejacProxy <- ejacProxy[-which(names(ejacProxy) == "1981-Reagan")]
    expect_equal(ejacQuanteda, ejacProxy)
})

test_that("test textstat_simil method = \"ejaccard\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    ejacQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "eJaccard", margin = "features"))[,"soviet"], 2)
    ejacQuanteda <- ejacQuanteda[order(names(ejacQuanteda))]
    ejacQuanteda <- ejacQuanteda[-which(names(ejacQuanteda) == "soviet")]
    
    ejacProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "ejaccard", by_rows = FALSE)), 2)
    ejacProxy <- ejacProxy[order(names(ejacProxy))]
    if("soviet" %in% names(ejacProxy)) ejacProxy <- ejacProxy[-which(names(ejacProxy) == "soviet")]
    
    expect_equal(ejacQuanteda, ejacProxy)
})

# dice -binary
test_that("test textstat_simil method = \"dice\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    diceQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "dice", margin = "documents", tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    diceQuanteda <- diceQuanteda[-which(names(diceQuanteda) == "1981-Reagan")]
    diceProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "dice", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(diceProxy)) diceProxy <- diceProxy[-which(names(diceProxy) == "1981-Reagan")]
    expect_equal(diceQuanteda, diceProxy)
})

test_that("test textstat_simil method = \"dice\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    diceQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "dice", margin = "features"))[,"soviet"], 2)
    diceQuanteda <- diceQuanteda[order(names(diceQuanteda))]
    diceQuanteda <- diceQuanteda[-which(names(diceQuanteda) == "soviet")]
    
    diceProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "dice", by_rows = FALSE)), 2)
    diceProxy <- diceProxy[order(names(diceProxy))]
    diceProxy <- diceProxy[-which(names(diceProxy) == "soviet")]
    
    expect_equal(diceQuanteda, diceProxy)
})

# edice -real valued data
test_that("test textstat_simil method = \"edice\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    ediceQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "eDice", margin = "documents", tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    ediceQuanteda <- ediceQuanteda[-which(names(ediceQuanteda) == "1981-Reagan")]
    ediceProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "edice", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(ediceProxy)) ediceProxy <- ediceProxy[-which(names(ediceProxy) == "1981-Reagan")]
    expect_equal(ediceQuanteda, ediceProxy)
})

test_that("test textstat_simil method = \"edice\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    ediceQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "eDice", margin = "features"))[,"soviet"], 2)
    ediceQuanteda <- ediceQuanteda[order(names(ediceQuanteda))]
    ediceQuanteda <- ediceQuanteda[-which(names(ediceQuanteda) == "soviet")]
    
    ediceProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "edice", by_rows = FALSE)), 2)
    ediceProxy <- ediceProxy[order(names(ediceProxy))]
    ediceProxy <- ediceProxy[-which(names(ediceProxy) == "soviet")]
    
    expect_equal(ediceQuanteda, ediceProxy)
})

# simple matching coefficient
test_that("test textstat_simil method = \"simple matching\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    smcQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "simple matching", margin = "documents", tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    smcQuanteda <- smcQuanteda[-which(names(smcQuanteda) == "1981-Reagan")]
    smcProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "simple matching", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(smcProxy)) smcProxy <- smcProxy[-which(names(smcProxy) == "1981-Reagan")]
    expect_equal(smcQuanteda, smcProxy)
})

test_that("test textstat_simil method = \"simple matching\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    smcQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "simple matching", margin = "features"))[,"soviet"], 2)
    smcQuanteda <- smcQuanteda[order(names(smcQuanteda))]
    smcQuanteda <- smcQuanteda[-which(names(smcQuanteda) == "soviet")]
    
    smcProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "simple matching", by_rows = FALSE)), 2)
    smcProxy <- smcProxy[order(names(smcProxy))]
    smcProxy <- smcProxy[-which(names(smcProxy) == "soviet")]
    
    expect_equal(smcQuanteda, smcProxy)
})

# Hamann similarity (Hamman similarity in proxy::dist)
test_that("test textstat_simil method = \"hamann\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    hamnQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "hamann", margin = "documents", tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    hamnQuanteda <- hamnQuanteda[-which(names(hamnQuanteda) == "1981-Reagan")]
    hamnProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "hamman", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(hamnProxy)) hamnProxy <- hamnProxy[-which(names(hamnProxy) == "1981-Reagan")]
    expect_equal(hamnQuanteda, hamnProxy)
})

test_that("test textstat_simil method = \"hamann\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    hamnQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "hamann", margin = "features"))[,"soviet"], 2)
    hamnQuanteda <- hamnQuanteda[order(names(hamnQuanteda))]
    hamnQuanteda <- hamnQuanteda[-which(names(hamnQuanteda) == "soviet")]
    
    hamnProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "hamman", by_rows = FALSE)), 2)
    hamnProxy <- hamnProxy[order(names(hamnProxy))]
    hamnProxy <- hamnProxy[-which(names(hamnProxy) == "soviet")]
    
    expect_equal(hamnQuanteda, hamnProxy)
})

# Faith similarity 
test_that("test textstat_simil method = \"faith\" against proxy::simil(): documents", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    faithQuanteda <- sort(round(as.matrix(textstat_simil(presDfm, "1981-Reagan", method = "faith", margin = "documents", tri = TRUE))[,"1981-Reagan"], 6), decreasing = FALSE)
    faithQuanteda <- faithQuanteda[-which(names(faithQuanteda) == "1981-Reagan")]
    faithProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "faith", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
    if("1981-Reagan" %in% names(faithProxy)) faithProxy <- faithProxy[-which(names(faithProxy) == "1981-Reagan")]
    expect_equal(faithQuanteda, faithProxy)
})

test_that("test textstat_simil method = \"faith\" against proxy::simil(): features", {
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    faithQuanteda <- round(as.matrix(textstat_simil(presDfm, "soviet", method = "faith", margin = "features"))[,"soviet"], 2)
    faithQuanteda <- faithQuanteda[order(names(faithQuanteda))]
    faithQuanteda <- faithQuanteda[-which(names(faithQuanteda) == "soviet")]
    
    faithProxy <- round(drop(proxy::simil(as.matrix(presDfm), as.matrix(presDfm[, "soviet"]), "faith", by_rows = FALSE)), 2)
    faithProxy <- faithProxy[order(names(faithProxy))]
    faithProxy <- faithProxy[-which(names(faithProxy) == "soviet")]
    
    expect_equal(faithQuanteda, faithProxy)
})

# Chi-squared distance 
# Instead of comparing to Proxy package, ExPosition is compared to. Because Proxy::simil uses different formula
# eucProxy <- sort(round(as.matrix(proxy::simil(as.matrix(presDfm), "Chi-squared", diag = FALSE, upper = FALSE))[, "1981-Reagan"], 6), decreasing = FALSE)
test_that("test textstat_dist method = \"Chi-squred\" against ExPosition::chi2Dist(): features", {
    skip_on_cran()
    skip_on_appveyor()
    skip_on_travis()
    require(proxy)
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    chiQuanteda <- round(as.matrix(textstat_dist(presDfm, method = "Chisquared", margin = "features"))[,"soviet"], 2)
    chiQuanteda <- chiQuanteda[order(names(chiQuanteda))]
    chiQuanteda <- chiQuanteda[-which(names(chiQuanteda) == "soviet")]
    
    chiExp <- ExPosition::chi2Dist(t(as.matrix(presDfm)))
    chiExp <- sort(round(as.matrix(chiExp$D)[, "soviet"], 2), decreasing = FALSE)
    chiExp <- chiExp[order(names(chiExp))]
    chiExp <- chiExp[-which(names(chiExp) == "soviet")]
    
    expect_equal(chiQuanteda, chiExp)
})

test_that("test textstat_dist method = \"Chi-squred\" against ExPosition::chi2Dist(): documents", {
    skip_on_cran()
    skip_on_appveyor()
    skip_on_travis()
    require(proxy)
    presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), remove = stopwords("english"),
                   stem = TRUE, verbose = FALSE)
    
    chiQuanteda <- sort(round(as.matrix(textstat_dist(presDfm, method = "Chisquared", margin = "documents"))[,"1981-Reagan"], 6), decreasing = FALSE)
    chiExp <- ExPosition::chi2Dist(as.matrix(presDfm))
    chiExp <- sort(round(as.matrix(chiExp$D)[, "1981-Reagan"], 6), decreasing = FALSE)
    expect_equal(chiQuanteda, chiExp)
})

# Kullback-Leibler divergence
# proxy::dist() will generate NA for matrix with zeros, hence a matrix only with non-zero entries is used here.
test_that("test textstat_dist method = \"Euclidean\" against proxy dist() and stats dist(): documents", {
    require(proxy)
    m <- matrix(rexp(550, rate=.1), nrow = 5)
    kullQuanteda <- round(as.matrix(textstat_dist(as.dfm(m), method = "kullback", margin = "documents")), 2)
    kullProxy <- round(as.matrix(proxy::dist(m, "kullback", diag = FALSE, upper = FALSE)), 2)
    expect_equal(kullQuanteda, kullProxy)
})

# rbenchmark::benchmark(
#     old =similarity(presDfm, method = "cosine"),
#     new = as.list(textstat_simil(presDfm, method = "cosine")),
#     replications = 1
# )
