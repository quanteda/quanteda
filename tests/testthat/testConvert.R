require(quanteda)
#require(testthat)
#require(stm)

test_that("test STM package converter", {
    mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
                 "New York City has raised a taxes: an income tax and a sales tax.")
    d <- dfm(mytexts)
    dSTM <- convert(d, to = "stm")
#    tP <- stm::textProcessor(mytexts, removestopwords = FALSE, stem = FALSE, wordLengths = c(1, Inf))
#    expect_equivalent(dSTM$documents[1], tP$documents[1])
#    expect_equivalent(dSTM$documents[2], tP$documents[2])
#    expect_equivalent(dSTM$vocab, tP$vocab)
})
