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
    expect_equivalent(round(as.matrix(tfidf(wikiDfm, scheme_tf = "prop")), 2),
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

test_that("dfm_weight exceptions work", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions"))
    mydfm_tfprop <- dfm_weight(mydfm, "relFreq")
    expect_message(
        dfm_weight(mydfm_tfprop, "tfidf"),
        "No weighting applied: you should not weight an already weighted dfm\\."
    )
})

test_that("tfidf works for combinations", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions"))
    expect_equal(
        dfm_weight(mydfm, "tfidf"),
        tfidf(mydfm)
    )
    
})

test_that("docfreq works as expected", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions",
                   "He ate pickles in the car."))
    expect_equivalent(
        docfreq(mydfm, scheme = "unary"),
        rep(1, ncol(mydfm))
    )
    expect_equivalent(
        docfreq(dfm_smooth(mydfm, 1)),
        rep(3, ncol(mydfm))
    )
    expect_equivalent(
        docfreq(dfm_smooth(mydfm, 1), threshold = 3),
        rep(0, ncol(mydfm))
    )
    expect_equivalent(
        docfreq(dfm_smooth(mydfm, 1), threshold = 2),
        c(rep(0, 7), 1, rep(0, 7))
    )
    expect_equivalent(
        docfreq(mydfm, scheme = "inversemax"),
        log10(max(docfreq(mydfm, "count")) / docfreq(mydfm, "count"))
    )
    expect_identical(
        as.vector(docfreq(mydfm, scheme = "inverseprob")),
        pmax(0, log10((nrow(mydfm) - docfreq(mydfm, "count")) / docfreq(mydfm, "count")))
    )
})

test_that("deprecated normalize argument still works in tfidf", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions"))
    expect_equal(
        tfidf(mydfm),
        suppressWarnings(tfidf(mydfm, normalize = FALSE))
    )
    expect_equal(
        tfidf(mydfm, scheme_tf = "prop"),
        suppressWarnings(tfidf(mydfm, normalize = TRUE))
    )
    expect_warning(
        tfidf(mydfm, normalize = TRUE),
        "normalize is deprecated"
    )
})

test_that("new tfidf returns same results as older one", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions",
                   "He ate pickles in the car."))
    expect_equivalent(
        tfidf(mydfm),
        quanteda:::tfidf_old(mydfm)
    )
})

test_that("tf with logave now working as expected", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions"))
    manually_calculated <- 
        as.matrix((1 + log10(mydfm)) / (1 + log10(apply(mydfm, 1, function(x) sum(x) / sum(x>0)))))
    manually_calculated[is.infinite(manually_calculated)] <- 0
    expect_equivalent(
        as.matrix(tf(mydfm, scheme = "logave")),
        manually_calculated
    )
})

test_that("tfidf works with different log base", {
    mydfm <- dfm(c("He went out to buy a car", 
                   "He went out and bought pickles and onions"))
    expect_true(
        !identical(
            as.matrix(tfidf(mydfm)), 
            as.matrix(tfidf(mydfm, base = 2))
        )
    )
})

test_that("docfreq works when features have duplicated names (#829)", {
    mydfm <- dfm(c(d1 = "a b c d e", d2 = "a a b b e f", d3 = "b e e f f f"))
    colnames(mydfm)[3] <- "b"
    expect_equal(
        docfreq(mydfm, USE.NAMES = TRUE),
        c(a=2, b=3, b=1, d=1, e=3, f=2)
    )
    expect_equal(
        docfreq(mydfm, USE.NAMES = FALSE),
        c(2, 3, 1, 1, 3, 2)
    )
})

test_that("dfm_weight works with zero-frequency features (#929)", {
    d1 <- dfm(c("a b c", "a b c d"))
    d2 <- dfm(letters[1:6])
    
    dtest <- dfm_select(d1, d2)
    
    expect_equal(
        as.matrix(tf(dtest, "prop")),
        matrix(c(0.33, 0.25, 0.33, 0.25, 0.33, 0.25, 0, 0.25, 0, 0, 0, 0), nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = letters[1:6])),
        tolerance = .01
    )
    expect_equal(
        docfreq(dtest),
        c(a = 2, b = 2, c = 2, d = 1, e = 0, f = 0)
    )
    expect_equal(
        as.matrix(tfidf(dtest, "prop")),
        matrix(c(rep(0, 6), 0.000, 0.07525, rep(0, 4)), nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = letters[1:6])),
        tolerance = .001
    )
})
