###
### Jurafsky and Martin example 3rd ed section 7.1
###
require(quanteda)
txt <- c(train1 = "just plain boring",
         train2 = "entirely predictable and lacks energy",
         train3 = "no surprises and very few laughs",
         train4 = "very powerful",
         train5 = "the most fun film of the summer",
         test1  = "predictable with no originality")
dfmJM <- dfm(txt, verbose = FALSE)

# NB
trainclass <- factor(c(rep("N", 3), "P", "P", NA))
(nbJM7_1 <- textmodel_NB(dfmJM, trainclass, prior = "docfreq"))
predict(nbJM7_1)
# from the text: pretty close posteriors for class of test1
c(1.8e-6, 5.7e-7) / (1.8e-6 + 5.7e-7)
