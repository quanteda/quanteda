context("test v2 core object operators")

test_that("corpus core operators work for v2", {
    corp <- corpus_subset(data_corpus_inaugural, Year <= 1805)
    
    expect_is(corp["1793-Washington"], "corpus")
    expect_identical(corp[2], 
                     corp["1793-Washington"])
    expect_identical(corp[c(FALSE, TRUE, FALSE, FALSE, FALSE)], 
                     corp["1793-Washington"])
    
    expect_identical(corp[[2]], 
                     corp[["1793-Washington"]])
    expect_identical(corp[c(FALSE, TRUE, FALSE, FALSE, FALSE)][[1]], 
                     corp[["1793-Washington"]])
    
    expect_is(corp$Year, "integer")
    expect_identical(
        corp$Year,
        docvars(corp, "Year")
    )
    # when docvar does not yet exist
    expect_identical(corp$nonexistent, NULL)
    
    corp2 <- corp
    corp2$Year <- floor(corp2$Year / 100)
    expect_identical(corp2$Year, c(17, 17, 17, 18, 18))
    
    # when docvar does not yet exist
    corp2$ones <- 1L
    expect_identical(docvars(corp2, "ones"), rep(1L, 5))
    
    # removing docvars through NULL assignment
    corp2$ones <- NULL
    expect_false("ones" %in% names(docvars(corp2)))
})

test_that("tokens core operators work for v2", {
    corp <- corpus_subset(data_corpus_inaugural, Year <= 1805)
    toks <- tokens(corp)[1:5]
    
    expect_is(toks["1793-Washington"], "tokens")
    expect_identical(toks[2], toks["1793-Washington"])
    expect_identical(toks[c(FALSE, TRUE, FALSE, FALSE, FALSE)], 
                     toks["1793-Washington"])

    expect_is(toks[["1793-Washington"]], "character")
    expect_identical(toks[[2]], toks[["1793-Washington"]])
    expect_identical(toks[c(FALSE, TRUE, FALSE, FALSE, FALSE)][[1]], 
                     toks[["1793-Washington"]])
    
    # when docvar does not yet exist
    expect_identical(toks$nonexistent, NULL)

    expect_is(toks$Year, "integer")
    expect_identical(
        toks$Year,
        docvars(toks, "Year")
    )
})

test_that("dfm core operators work for v2", {
    dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year <= 1805))
    
    # expect_is(dfmat["1793-Washington"], NA)
    # expect_identical(
    #     dfmat["1793-Washington"],
    #     as.matrix(dfmat)["1793-Washington"]
    # )
   
     expect_error(
        dfmat[["1793-Washington"]], 
        "[[ not defined for a dfm/fcm object",
        fixed = TRUE
    )

    expect_is(dfmat$Year, "integer")
    expect_identical(
        dfmat$Year,
        docvars(dfmat, "Year")
    )
    # when docvar does not yet exist
    expect_identical(dfmat$nonexistent, NULL)
})

test_that("fcm core operators work for v2", {
    dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year <= 1805))
    fcmat <- fcm(dfm_trim(dfmat, min_termfreq = 20))
    
    # expect_is(fcmat[c("the", "and")], c(NA, NA))
    # expect_identical(
    #     fcmat[c("the", "and")],
    #     as.matrix(fcmat)[c("the", "and")]
    # )
    
    expect_error(
        fcmat[["the"]], 
        "[[ not defined for a dfm/fcm object",
        fixed = TRUE
    )
    
    expect_error(
        fcmat$Year,
        "$ not defined for an fcm object",
        fixed = TRUE
    )
    expect_error(
        fcmat$Year <- 999,
        "$<- not defined for an fcm object",
        fixed = TRUE
    )
})

test_that("dictionary core operators work for v2", {
    dict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                            opposition = c("Opposition", "reject", "notincorpus")),
                       tolower = FALSE)
    
    expect_identical(
        dict$opposition,
        c("Opposition", "reject", "notincorpus")
    )
    expect_identical(
        dict[["opposition"]],
        dict$opposition
    )
    expect_identical(
        dict["opposition"],
        dictionary(list(opposition = c("Opposition", "reject", "notincorpus")),
                       tolower = FALSE)
    )
})
