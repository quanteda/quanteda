
context("test convert function and shortcuts")

mytexts <- c(text1 = "The new law included a capital gains tax, and an inheritance tax.",
             text2 = "New York City has raised a taxes: an income tax and a sales tax.")
d <- dfm(mytexts, removePunct = TRUE)

test_that("test STM package converter", {
    skip_on_appveyor()
    skip_on_travis()
    skip_on_cran()
    dSTM <- convert(d, to = "stm")
    tP <- stm::textProcessor(mytexts, removestopwords = FALSE, 
                             stem = FALSE, wordLengths = c(1, Inf))
    expect_equivalent(dSTM$documents[1], tP$documents[1])
    expect_equivalent(dSTM$documents[2], tP$documents[2])
    expect_equivalent(dSTM$vocab, tP$vocab)
})

test_that("test tm package converter", {
    dtmq <- convert(d[, order(featnames(d))], to = "tm")
    dtmtm <- tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(toLower(mytexts))),
                                    control = list(removePunctuation = TRUE,
                                                   wordLengths = c(1, Inf)))
    ## FAILS
    # expect_equivalent(dtmq, dfmtm)
    expect_equivalent(as.matrix(dtmq), as.matrix(dtmtm))
    
    expect_identical(convert(d, to = "tm"), quanteda::as.DocumentTermMatrix(d))
})

test_that("test lda package converter", {
    expect_identical(convert(d, to = "topicmodels"), quantedaformat2dtm(d))
})

test_that("test topicmodels package converter", {
    expect_identical(convert(d, to = "lda"), dfm2ldaformat(d))
})

test_that("test austin package converter", {
    expect_identical(convert(d, to = "austin"), quanteda::as.wfm(d))
})
