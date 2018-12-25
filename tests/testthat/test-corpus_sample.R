context("corpus_sample tests")

corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                      two = "First sentence, doc2.  Second sentence, doc2."))
corp_sent <- corpus_reshape(corp, to = "sentences")

test_that("test corpus_sample to see if without grouping, documents can be oversampled", {
    set.seed(100)
    corp_sample <- corpus_sample(corp_sent, replace = TRUE)
    expect_gt(sum(stringi::stri_detect_regex(docnames(corp_sample), "^one")), 3)         
})

test_that("test corpus_sample to see if with grouping, documents can be oversampled", {
    for (i in 1:10) {
        set.seed(100)
        corp_sample <- corpus_sample(corp_sent, replace = TRUE, by = "document")
        expect_equal(sum(stringi::stri_detect_regex(docnames(corp_sample), "^one")), 3)
    }
})

test_that("disabled nresample funtions \"work\"", {
    expect_equal(quanteda:::nresample(data_corpus_inaugural), 0)
    expect_equal(quanteda:::is.resampled(data_corpus_inaugural), FALSE)
})
