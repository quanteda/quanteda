
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

test_that("test STM package converter with metadata", {
    skip_if_not_installed("stm")
    skip_if_not_installed("tm")
    dv <- data.frame(myvar = c("A", "B"), row.names = names(mytexts))
    mycorpus <- corpus(mytexts, docvars = dv)
    dm <- dfm(mycorpus, remove_punct = TRUE)
    dSTM <- convert(dm, to = "stm")
    tP <- stm::textProcessor(mytexts, removestopwords = FALSE, 
                             stem = FALSE, wordLengths = c(1, Inf))
    expect_equivalent(dSTM$documents[1], tP$documents[1])
    expect_equivalent(dSTM$documents[2], tP$documents[2])
    expect_equivalent(dSTM$vocab, tP$vocab)
    expect_identical(dSTM$meta, dv)
})

test_that("test STM package converter with metadata w/zero-count document", {
    skip_if_not_installed("stm")
    skip_if_not_installed("tm")
    mytexts2 <- c(text1 = "The new law included a capital gains tax, and an inheritance tax.",
                  text2 = ";",  # this will become empty
                  text3 = "New York City has raised a taxes: an income tax and a sales tax.")
    dv <- data.frame(myvar = c("A", "B", "C"), row.names = names(mytexts2))
    mycorpus <- corpus(mytexts2, docvars = dv)
    dm <- dfm(mycorpus, remove_punct = TRUE)
    expect_true(ntoken(dm)[2] == 0)
    
    dSTM <- suppressWarnings(convert(dm, to = "stm"))
    tP <- stm::textProcessor(mytexts, removestopwords = FALSE, 
                             stem = FALSE, wordLengths = c(1, Inf))
    expect_equivalent(dSTM$documents[1], tP$documents[1])
    expect_equivalent(dSTM$documents[2], tP$documents[2])
    expect_equivalent(dSTM$vocab, tP$vocab)
    expect_identical(dSTM$meta, dv[-2, , drop = FALSE])
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

test_that("test stm converter: under extreme situations ", {
    #zero-count document
    mydfm <- as.dfm(matrix(c(1, 0, 2, 0, 
                             0, 0, 1, 2, 
                             0, 0, 0, 0, 
                             1, 2, 3, 4), byrow = TRUE, nrow = 4))
    expect_warning(convert(mydfm, to = "stm"), "Dropped empty document\\(s\\): doc3")

    #zero-count feature
    mydfm <- as.dfm(matrix(c(1, 0, 2, 0, 
                             0, 0, 1, 2, 
                             1, 0, 0, 0, 
                             1, 0, 3, 4), byrow = TRUE, nrow = 4))
    expect_warning(stmdfm<-convert(mydfm, to = "stm"), "zero-count features: feat2")
    
    skip_if_not_installed("stm")
    require(stm)
    stm_model <- stm(documents = stmdfm$documents, vocab = stmdfm$vocab, K=3)
    expect_output(print(stm_model), "A topic model with 3 topics")
    
    #when dfm is 0% sparse
    stmdfm <- convert(as.dfm(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)), to = "stm")
    expect_equal(length(stmdfm$documents), 3)
})

test_that("lsa converter works under extreme situations", {
    skip_if_not_installed("lsa")
    #zero-count document
    mydfm <- as.dfm(matrix(c(1, 0, 2, 0, 
                             0, 0, 1, 2, 
                             0, 0, 0, 0, 
                             1, 2, 3, 4), byrow = TRUE, nrow = 4))
    # lsa handles empty docs with a warning message 
    expect_warning(lsalsa <- lsa::lsa(convert(mydfm, to = "lsa")), "there are singular values which are zero")
    expect_equal(class(lsalsa), "LSAspace")  
    
    #zero-count feature
    mydfm <- as.dfm(matrix(c(1, 0, 2, 0, 
                             0, 0, 1, 2, 
                             1, 0, 0, 0, 
                             1, 0, 3, 4), byrow = TRUE, nrow = 4))
    expect_warning(lsalsa <- lsa::lsa(convert(mydfm, to = "lsa")), "there are singular values which are zero")
    expect_equal(class(lsalsa), "LSAspace") 
    
    #when dfm is 0% sparse
    lsadfm <- convert(as.dfm(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)), to = "lsa")
    expect_equal(suppressWarnings(class(lsa(lsadfm))), "LSAspace") 
})

test_that("topicmodels converter works under extreme situations", {
    skip_if_not_installed("topicmodels")
    #zero-count document
    mydfm <- as.dfm(matrix(c(1, 0, 2, 0, 
                             0, 0, 1, 2, 
                             0, 0, 0, 0, 
                             1, 2, 3, 4), byrow = TRUE, nrow = 4))
    motifresult <- LDA(convert(mydfm, to = "topicmodels"), k = 3)
    expect_equivalent(class(motifresult), "LDA_VEM")  
    
    #zero-count feature
    mydfm <- as.dfm(matrix(c(1, 0, 2, 0, 
                             0, 0, 1, 2, 
                             1, 0, 0, 0, 
                             1, 0, 3, 4), byrow = TRUE, nrow = 4))
    motifresult <- LDA(convert(mydfm, to = "topicmodels"), k = 3)
    expect_equivalent(class(motifresult), "LDA_VEM") 
    
    #when dfm is 0% sparse
    motifdfm <- convert(as.dfm(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)), to = "topicmodels")
    motifresult <- LDA(motifdfm, 3)
    expect_equivalent(class(motifresult), "LDA_VEM")
})
