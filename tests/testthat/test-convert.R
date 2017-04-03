
context("test convert function and shortcuts")

mytexts <- c(text1 = "The new law included a capital gains tax, and an inheritance tax.",
             text2 = "New York City has raised a taxes: an income tax and a sales tax.")
d <- dfm(mytexts, remove_punct = TRUE)

test_that("test STM package converter", {
    skip_if_not_installed("stm")
    skip_if_not_installed("tm")
    
    dSTM <- convert(d, to = "stm")
    tP <- stm::textProcessor(mytexts, removestopwords = FALSE, 
                             stem = FALSE, wordLengths = c(1, Inf))
    expect_equivalent(dSTM$documents[1], tP$documents[1])
    expect_equivalent(dSTM$documents[2], tP$documents[2])
    expect_equivalent(dSTM$vocab, tP$vocab)
})

test_that("test tm package converter", {
    skip_if_not_installed("tm")
    dtmq <- convert(d[, order(featnames(d))], to = "tm")
    dtmtm <- tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(char_tolower(mytexts))),
                                    control = list(removePunctuation = TRUE,
                                                   wordLengths = c(1, Inf)))
    ## FAILS
    # expect_equivalent(dtmq, dfmtm)
    expect_equivalent(as.matrix(dtmq), as.matrix(dtmtm))
    
    expect_identical(convert(d, to = "tm"), quanteda::as.DocumentTermMatrix(d))
})

test_that("test lda package converter", {
    skip_if_not_installed("tm")
    expect_identical(convert(d, to = "topicmodels"), quantedaformat2dtm(d))
})

test_that("test topicmodels package converter", {
    skip_if_not_installed("tm")
    expect_identical(convert(d, to = "lda"), dfm2ldaformat(d))
})

test_that("test austin package converter", {
    expect_identical(convert(d, to = "austin"), quanteda::as.wfm(d))
})

test_that("test lsa converter", {
    skip_if_not_installed("lsa")
    require(lsa)
    # create some files
    td <- tempfile()
    dir.create(td)
    write( c("cat", "dog", "mouse"), file = paste(td, "D1", sep="/") )
    write( c("hamster", "mouse", "sushi"), file = paste(td, "D2", sep="/") )
    write( c("dog", "monster", "monster"), file = paste(td, "D3", sep="/") )
    # read them, create a document-term matrix
    lsamat <- lsa::textmatrix(td)
    
    lsamat2 <- convert(dfm(tokens(c(D1 = c("cat dog mouse"),
                                    D2 = c("hamster mouse sushi"), 
                                    D3 = c("dog monster monster")))),
                       to = "lsa")
    expect_equivalent(lsamat, lsamat2)    
    
})



