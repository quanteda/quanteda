test_that("dfm_weight works", {
    str <- c("apple is better than banana", "banana banana apple much better")
    dfmt <- dfm(tokens(str)) |>
        dfm_remove(stopwords("english"))

    expect_equivalent(round(as.matrix(dfm_weight(dfmt, scheme = "count")), 2),
                      matrix(c(1, 1, 1, 1, 1, 2, 0, 1), nrow = 2))

    expect_equivalent(round(as.matrix(dfm_weight(dfmt, scheme = "prop")), 2),
                      matrix(c(0.33, 0.2, 0.33, 0.2, 0.33, 0.4, 0, 0.2), nrow = 2))

    expect_equivalent(round(as.matrix(dfm_weight(dfmt, scheme = "propmax")), 2),
                      matrix(c(1, 0.5, 1, 0.5, 1, 1, 0, 0.5), nrow = 2))

    expect_equivalent(round(as.matrix(dfm_weight(dfmt, scheme = "logcount")), 2),
                      matrix(c(1, 1, 1, 1, 1, 1.30, 0, 1), nrow = 2))

    # replication of worked example from
    # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
    str <- c("this is a  a sample", "this is another example another example example")
    wikidfm <- dfm(tokens(str))
    expect_equal(
        as.matrix(dfm_tfidf(wikidfm, scheme_tf = "prop")),
        matrix(c(0, 0, 0, 0, 0.120412, 0, 0.060206, 0, 0, 0.08600857, 0, 0.1290129), nrow = 2,
               dimnames = list(docs = c("text1", "text2"),
                               features = c("this", "is", "a", "sample", "another", "example"))),
        tol = .0001
    )
    # print(as.matrix(dfm_tfidf(wikidfm, scheme_tf = "prop")))
    # print(matrix(c(0, 0, 0, 0, 0.120412, 0, 0.060206, 0, 0, 0.08600857, 0, 0.1290129), nrow = 2,
    #              dimnames = list(docs = c("text1", "text2"),
    #                              features = c("this", "is", "a", "sample", "another", "example"))))
})

test_that("dfm_weight works with weights", {
    str <- c("apple is better than banana", "banana banana apple much better")
    w <- c(apple = 5, banana = 3, much = 0.5)
    dfmt <- dfm(tokens(str)) |>
        dfm_remove(stopwords("english"))
    
    dfmt_w <- dfm_weight(dfmt, weights = w)
    expect_true(is.dfm(dfmt_w))
    expect_equivalent(as.matrix(dfmt_w),
                      matrix(c(5, 5, 1, 1, 3, 6, 0, 0.5), nrow = 2))

    expect_warning(
        dfm_weight(dfmt, scheme = "relfreq", weights = w),
        "scheme is ignored when numeric weights are supplied"
    )

    w2 <- c(apple = 5, banana = 3, much = 0.5, notfound = 10)
    suppressWarnings(
        dfmt_w2 <- dfm_weight(dfmt, weights = w2)
    )
    expect_true(is.dfm(dfmt_w2))
    expect_equivalent(as.matrix(dfmt_w2),
                      matrix(c(5, 5, 1, 1, 3, 6, 0, 0.5), nrow = 2))
    expect_warning(
        dfm_weight(dfmt, weights = w2),
        "ignoring 1 unmatched weight feature"
    )

})

test_that("dfm_weight exceptions work", {
    dfmt <- dfm(tokens(c("He went out to buy a car",
                   "He went out and bought pickles and onions")))
    dfmt_tfprop <- dfm_weight(dfmt, "prop")
    expect_error(
        dfm_tfidf(dfmt_tfprop),
        "will not weight a dfm already term-weighted as 'prop'; use force = TRUE to override"
    )
    expect_is(
        dfm_tfidf(dfmt_tfprop, force = TRUE),
        "dfm"
    )
    expect_is(
        dfm_weight(dfmt_tfprop, scheme = "logcount", force = TRUE),
        "dfm"
    )
})

test_that("docfreq works as expected", {
    dfmt <- dfm(tokens(c("He went out to buy a car",
                   "He went out and bought pickles and onions",
                   "He ate pickles in the car.")))
    expect_equivalent(
        docfreq(dfmt, scheme = "unary"),
        rep(1, ncol(dfmt))
    )
    expect_equivalent(
        docfreq(dfm_smooth(dfmt, 1)),
        rep(3, ncol(dfmt))
    )
    expect_equivalent(
        docfreq(dfm_smooth(dfmt, 1), threshold = 3),
        rep(0, ncol(dfmt))
    )
    expect_equivalent(
        docfreq(dfm_smooth(dfmt, 1), threshold = 2),
        c(rep(0, 7), 1, rep(0, 7))
    )
    expect_equivalent(
        docfreq(dfmt, scheme = "inversemax"),
        log10(max(docfreq(dfmt, "count")) / docfreq(dfmt, "count"))
    )
    expect_identical(
        as.vector(docfreq(dfmt, scheme = "inverseprob")),
        pmax(0, log10((nrow(dfmt) - docfreq(dfmt, "count")) / docfreq(dfmt, "count")))
    )
    expect_warning(
        docfreq(dfmt, scheme = "unary", base = 2),
        "base not used for this scheme"
    )
    expect_warning(
        docfreq(dfmt, scheme = "unary", k = 1),
        "k not used for this scheme"
    )
    expect_warning(
        docfreq(dfmt, scheme = "unary", smoothing = 1),
        "smoothing not used for this scheme"
    )
})

test_that("tf with logave now working as expected", {
    dfmt <- dfm(tokens(c("He went out to buy a car",
                   "He went out and bought pickles and onions")))
    manually_calculated <-
        as.matrix((1 + log10(dfmt)) / (1 + log10(apply(dfmt, 1, function(x) sum(x) / sum(x > 0)))))
    manually_calculated[is.infinite(manually_calculated)] <- 0
    expect_equivalent(
        as.matrix(dfm_weight(dfmt, scheme = "logave")),
        manually_calculated
    )
})

test_that("tfidf works with different log base", {
    dfmt <- dfm(tokens(c("He went out to buy a car",
                   "He went out and bought pickles and onions")))
    expect_true(
        !identical(
            as.matrix(dfm_tfidf(dfmt)),
            as.matrix(dfm_tfidf(dfmt, base = 2))
        )
    )
})

test_that("docfreq works when features have duplicated names (#829)", {
    dfmt <- dfm(tokens(c(d1 = "a b c d e", d2 = "a a b b e f", d3 = "b e e f f f")))
    colnames(dfmt)[3] <- "b"
    expect_equal(
        docfreq(dfmt),
        c(a = 2, b = 3, b = 1, d = 1, e = 3, f = 2)
    )
})

test_that("dfm_weight works with zero-frequency features (#929)", {
    d1 <- dfm(tokens(c("a b c", "a b c d")))
    d2 <- dfm(tokens(letters[1:6]))
    dtest <- dfm_match(d1, featnames(d2))

    expect_equal(
        as.matrix(dfm_weight(dtest, "prop")),
        matrix(c(0.33, 0.25, 0.33, 0.25, 0.33, 0.25, 0, 0.25, 0, 0, 0, 0), nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = letters[1:6])),
        tolerance = .01
    )
    expect_equal(
        docfreq(dtest),
        c(a = 2, b = 2, c = 2, d = 1, e = 0, f = 0)
    )
    expect_equal(
        as.matrix(dfm_tfidf(dtest, "prop")),
        matrix(c(rep(0, 6), 0.000, 0.07525, rep(0, 4)), nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = letters[1:6])),
        tolerance = .001
    )
})

test_that("settings are recorded for tf-idf weightings", {
    txt <- c(text1 = "The new law included a capital gains tax, and an inheritance tax.",
             text2 = "New York City has raised a taxes: an income tax and a sales tax.")
    dfmt <- dfm(tokens(txt, remove_punct = TRUE))
    dfmt_tfidf <- dfm_tfidf(dfmt)
    expect_equal(dfmt_tfidf@meta$object$weight_tf$scheme, "count")
    expect_equal(dfmt_tfidf@meta$object$weight_df$scheme, "inverse")
    expect_equal(dfmt_tfidf@meta$object$weight_df[["base"]], 10)

    expect_equal(dfmt_tfidf@meta$object$weight_tf$scheme, "count")
    expect_equal(dfmt_tfidf@meta$object$weight_df$scheme, "inverse")
    expect_equal(dfm_tfidf(dfmt, base = 10)@meta$object$weight_df[["base"]], 10)
    expect_equal(dfm_tfidf(dfmt, base = 2)@meta$object$weight_df[["base"]], 2)
    expect_equal(dfm_tfidf(dfmt, scheme_tf = "prop", base = 2)@meta$object$weight_tf$scheme, "prop")
    expect_equal(dfm_tfidf(dfmt, scheme_tf = "prop", base = 2)@meta$object$weight_df[["base"]], 2)

    expect_equal(dfm_tfidf(dfmt, scheme_df = "inversemax")@meta$object$weight_df$scheme, "inversemax")
    expect_equal(dfm_tfidf(dfmt, scheme_df = "inversemax", k = 1)@meta$object$weight_df$k, 1)
})

test_that("weights argument works, issue 1150", {
    txt <- c("brown brown yellow green", "yellow green blue")
    dfmt <- dfm(tokens(txt))
    w <- c(brown = 0.1, yellow = 0.3, green = 0.4, blue = 2)

    expect_equal(
        as.matrix(dfm_weight(dfmt, weights = w)),
        matrix(c(0.2, 0, 0.3, 0.3, 0.4, 0.4, 0, 2), nrow = 2,
               dimnames = list(docs = c("text1", "text2"),
                               features = c("brown", "yellow", "green", "blue")))
    )

    expect_equal(
        as.matrix(dfm_weight(dfmt, weights = w[c(2, 3, 4)])),
        matrix(c(2, 0, 0.3, 0.3, 0.4, 0.4, 0, 2), nrow = 2,
               dimnames = list(docs = c("text1", "text2"),
                               features = c("brown", "yellow", "green", "blue")))
    )

    expect_equal(
        as.matrix(dfm_weight(dfmt, weights = w[c(1, 3, 2)])),
        matrix(c(0.2, 0, 0.3, 0.3, 0.4, 0.4, 0, 1), nrow = 2,
               dimnames = list(docs = c("text1", "text2"),
                               features = c("brown", "yellow", "green", "blue")))
    )

    # test when a feature is not assigned a weight
    txt2 <- c(d1 = "brown brown yellow green black", d2 = "yellow green blue")
    dfmt2 <- dfm(tokens(txt2))
    w2 <- c(green = .1, blue = .2, brown = .3, yellow = .4)
    expect_equal(
        as.matrix(dfm_weight(dfmt2, weights = w2)),
        matrix(c(.6, 0, .4, .4, .1, .1, 1, 0, 0, .2), nrow = 2,
               dimnames = list(docs = c("d1", "d2"), features = c("brown", "yellow", "green", "black", "blue")))
    )
})

test_that("docfreq works previously a weighted dfm (#1237)", {
    df1 <- dfm(data_dfm_lbgexample) |> dfm_tfidf(scheme_tf = "prop")
    computed <- c(rep(1, 5), 2, 2, 3, 3, 3, 4)
    names(computed) <- letters[1:11]
    expect_equal(
        docfreq(df1)[1:11],
        computed
    )
})

test_that("smooth slot is correctly set (#1274)", {
    expect_equal(as.dfm(data_dfm_lbgexample)@meta$object$smooth, 0)

    # smoothed by 1
    dfms1 <- dfm_smooth(data_dfm_lbgexample, smoothing = 1)
    expect_equal(dfms1@meta$object$smooth, 1)

    # smoothed by 0.5
    dfms0_5 <- dfm_smooth(data_dfm_lbgexample, smoothing = 0.5)
    expect_equal(dfms0_5@meta$object$smooth, 0.5)

    # smoothed by 1 and then by another 2
    dfms1_2 <- dfm_smooth(dfms1, smoothing = 2)
    expect_equal(dfms1_2@meta$object$smooth, 3)
})

test_that("dfm_weight invalid scheme produces error", {
    expect_error(
        dfm_weight(data_dfm_lbgexample, scheme = "nonexistent"),
        "\'arg\' should be one of",
    )
})

test_that("featfreq() works", {
    dfmat <- dfm(tokens(c(d1 = "a a a b", d2 = "a b c")))
    expect_identical(
        featfreq(dfmat),
        c(a = 4, b = 2, c = 1)
    )
})

