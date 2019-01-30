context("test corpus_sample")

corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                 two = "First sentence, doc2.  Second sentence, doc2."))
corpsent <- corpus_reshape(corp, to = "sentences")

test_that("test corpus_sample to see if without grouping, documents can be oversampled", {
    set.seed(100)
    corpsamp <- corpus_sample(corpsent, replace = TRUE)
    expect_gt(sum(stringi::stri_detect_regex(docnames(corpsamp), "^one")), 3)         
})

test_that("test corpus_sample to see if with grouping, documents can be oversampled", {
    for (i in 1:10) {
        corpsamp <- corpus_sample(corpsent, replace = TRUE, by = "document")
        expect_equal(
            sum(stringi::stri_detect_regex(docnames(corpus_sample(corpsamp, replace = TRUE, by = "docid_")), "^one")),
            3
        )
    }
})

test_that("corpus with prob does not work with by", {
    expect_error(
        corpus_sample(corpsent, prob = rep(0.2, 5), by = docnames(corpsent)),
        "prob not implemented with by"
    )
})

