context("test textstat_simil2.R")

test_mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), remove = stopwords("english"),
               stem = TRUE, verbose = FALSE)

# correlation
test_that("test textstat_simil2 method = correlation on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "correlation", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "correlation", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = correlation on features)", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "correlation", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "correlation", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "correlation", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "correlation", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = cosine on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "cosine", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "cosine", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "cosine", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "cosine", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = cosine on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "cosine", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "cosine", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "cosine", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "cosine", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = jaccard on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "jaccard", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "jaccard", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "jaccard", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "jaccard", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = jaccard on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "jaccard", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "jaccard", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "jaccard", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "jaccard", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = ejaccard on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "ejaccard", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "ejaccard", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "ejaccard", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "ejaccard", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = ejaccard on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "ejaccard", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "ejaccard", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "ejaccard", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "ejaccard", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = dice on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "dice", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "dice", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "dice", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "dice", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = dice on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "dice", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "dice", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "dice", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "dice", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = edice on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "edice", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "edice", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "edice", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "edice", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = edice on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "edice", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "edice", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "edice", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "edice", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = simple matching on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "simple matching", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "simple matching", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "simple matching", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "simple matching", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = \"simple matching\" against proxy::simil(): features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt[,1:100], method = "simple matching", margin = "features"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt[,1:100]), 
                                 method = "simple matching", by_rows = FALSE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "soviet", method = "simple matching", margin = "features"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt[,"soviet"]), 
                                 method = "simple matching", by_rows = FALSE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = hamman on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "hamman", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "hamman", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "hamman", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "hamman", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = hamman on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "hamman", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "hamman", by_rows = TRUE, diag = TRUE))
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "hamman", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "hamman", by_rows = TRUE, diag = TRUE))
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = faith on documents", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "faith", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "faith", by_rows = TRUE, diag = TRUE))
    diag(s1) <- diag(s2) <- NA # coersion of dist object to matrix induces artificial diagonal values
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "faith", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "faith", by_rows = TRUE, diag = TRUE))
    diag(s3) <- diag(s4) <- NA # coersion of dist object to matrix induces artificial diagonal values
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("test textstat_simil2 method = faith on features", {
    skip_if_not_installed("proxy")
    s1 <- as.matrix(textstat_simil2(test_mt, method = "faith", margin = "documents"))
    s2 <- as.matrix(proxy::simil(as.matrix(test_mt), 
                                 method = "faith", by_rows = TRUE, diag = TRUE))
    diag(s1) <- diag(s2) <- NA # coersion of dist object to matrix induces artificial diagonal values
    expect_equal(s1, s2, tolerance = 0.001)
    
    s3 <- as.matrix(textstat_simil2(test_mt, "1985-Reagan", method = "faith", margin = "documents"))
    s4 <- as.matrix(proxy::simil(as.matrix(test_mt), as.matrix(test_mt["1985-Reagan",]), 
                                 method = "faith", by_rows = TRUE, diag = TRUE))
    diag(s3) <- diag(s4) <- NA # coersion of dist object to matrix induces artificial diagonal values
    expect_equal(as.numeric(s3), as.numeric(s4), tolerance = 0.001)
})

test_that("as.matrix works as expected",{
    txt <- c('Bacon ipsum dolor amet tenderloin hamburger bacon t-bone,', 
             'Tenderloin turducken corned beef bacon.', 
             ' Burgdoggen venison tail, hamburger filet mignon capicola meatloaf pig pork belly.')
    mt <- dfm(txt)
    expect_equivalent(diag(as.matrix(textstat_simil2(mt))), 
                      rep(1, 3))
})

test_that("textstat_simil2 stops as expected for methods not supported",{
    expect_error(textstat_simil2(test_mt, method = "Yule"))
})

test_that("textstat_simil2 works on zero-frequency features", {
    d1 <- dfm(c("a b c", "a b c d"))
    d2 <- dfm(letters[1:6])
    dtest <- dfm_select(d1, d2)
    
    expect_equal(
        textstat_simil2(dtest, method = "cosine")[2,1], 0.866,
        tolerance = 0.001
    )    
    expect_equal(
        textstat_simil2(dtest, method = "correlation")[2,1], 0.707,
        tolerance = 0.001
    )    
})

test_that("textstat_simil2 works on zero-feature documents (#952)", {
    corp <- corpus(c('a b c c', 'b c d', 'a'),
                   docvars = data.frame(grp = factor(c("A", "A", "B"), levels = LETTERS[1:3])))
    mt <- dfm(corp)
    mt <- dfm_group(mt, groups = "grp", fill = TRUE)

    expect_equal(
        as.numeric(textstat_simil2(mt, method = "cosine")[1,]),
        c(1, 0.2581, 0),
        tolerance = 0.001
    ) 
    expect_equal(
        as.numeric(textstat_simil2(mt, method = "correlation")[1,]),
        c(1, -0.5222, 0),
        tolerance = 0.001
    )    
})

test_that("selection offers option to enable an alien vector/matrix", {
    expect_error(textstat_simil2(test_mt, c(1,2,3,4,5,6,7), margin = "features"), NA)
})

test_that("selection works with dfm with padding", {
    
    toks <- tokens(c(doc1 = 'a b c d e', doc2 = 'b c f e'), remove_punct = TRUE)
    toks <- tokens_remove(toks, 'b', padding = TRUE)
    mt <- dfm(toks)
    expect_silent(textstat_simil2(mt, selection = c('c'), margin = 'features'))
    expect_silent(textstat_simil2(mt, selection = c(''), margin = 'features'))
    expect_silent(textstat_simil2(mt, selection = c('doc2'), margin = 'documents'))
    
})

test_that("raises error when dfm is empty (#1419)", {
    mx <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_error(textstat_simil2(mx),
                 quanteda:::message_error("dfm_empty"))
})
