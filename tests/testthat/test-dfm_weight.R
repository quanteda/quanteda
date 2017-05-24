context("test dfm_weight")

test_that("dfm_weight works", {
    str <- c("apple is better than banana", "banana banana apple much better")
    mydfm <- dfm(str, remove = stopwords("english"))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "frequency")), 2),
                      matrix(c(1, 1, 1, 1, 1, 2, 0, 1), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "relFreq")), 2),
                      matrix(c(0.33, 0.2, 0.33, 0.2, 0.33, 0.4, 0, 0.2), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "relMaxFreq")), 2),
                      matrix(c(1, 0.5, 1, 0.5, 1, 1, 0, 0.5), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "logFreq")), 2),
                      matrix(c(1, 1, 1, 1, 1, 1.30, 0, 1), nrow = 2))
    
    # replication of worked example from
    # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
    str <- c("this is a  a sample", "this is another example another example example")
    wikiDfm <- dfm(str)
    expect_equivalent(round(as.matrix(tfidf(wikiDfm, normalize = TRUE)), 2),
                      matrix(c(0, 0, 0, 0, 0.12, 0, 0.06, 0, 0, 0.09, 0, 0.13), nrow = 2))
})

test_that("dfm_weight works with weights", {
    str <- c("apple is better than banana", "banana banana apple much better")
    w <- c(apple = 5, banana = 3, much = 0.5)
    mydfm <- dfm(str, remove = stopwords("english"))
    
    expect_equivalent(as.matrix(dfm_weight(mydfm, weights = w)),
                      matrix(c(5, 5, 1, 1, 3, 6, 0, 0.5), nrow = 2))

    expect_warning(
        dfm_weight(mydfm, type = "relFreq", weights = w),
        "type is ignored when numeric weights are supplied"
    )
    
    w <- c(apple = 5, banana = 3, much = 0.5, notfound = 10)
    expect_equivalent(as.matrix(dfm_weight(mydfm, weights = w)),
                      matrix(c(5, 5, 1, 1, 3, 6, 0, 0.5), nrow = 2))
    expect_warning(
        dfm_weight(mydfm, weights = w),
        "ignoring 1 unmatched weight feature"
    )

})