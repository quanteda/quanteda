context("test textmodel_NB")

## Example from 13.1 of _An Introduction to Information Retrieval_
trainingset <- as.dfm(matrix(c(1, 2, 0, 0, 0, 0,
                               0, 2, 0, 0, 1, 0,
                               0, 1, 0, 1, 0, 0,
                               0, 1, 1, 0, 0, 1,
                               0, 3, 1, 0, 0, 1), 
                             ncol=6, nrow=5, byrow=TRUE,
                             dimnames = list(docs = paste("d", 1:5, sep = ""),
                                             features = c("Beijing", "Chinese",  "Japan", "Macao", 
                                                          "Shanghai", "Tokyo"))))
trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)

test_that("class priors are preserved in correct order", {
    expect_equal(textmodel_NB(trainingset, trainingclass, prior = "uniform")$Pc,
                 c(N = 0.5, Y = 0.5))
    expect_equal(textmodel_NB(trainingset, trainingclass, prior = "docfreq")$Pc,
                 c(N = 0.25, Y = 0.75))
    expect_equal(round(textmodel_NB(trainingset, trainingclass, prior = "termfreq")$Pc, 2),
                 c(Y = 0.73, N = 0.27))
})

test_that("predicted NB probabilities are correct", {
    expect_equal(round(predict(textmodel_NB(trainingset, trainingclass, prior = "docfreq"))$posterior.prob, 2)[5,1],
                 0.2)
})
