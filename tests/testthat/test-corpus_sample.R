context("corpus_sample tests")


corpdoc <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                      two = "First sentence, doc2.  Second sentence, doc2."))
corpsent <- corpus_reshape(corpdoc, to = "sentences")


test_that("test corpus_sample to see if without grouping, documents can be oversampled", {
    # sampling without document grouping should be able to produce oversampling of a document
    set.seed(100)
    expect_gt(
        sum(stringi::stri_detect_regex(docnames(corpus_sample(corpsent, replace = TRUE)), "^one")),
        3
    )         
})

test_that("test corpus_sample to see if with grouping, documents can be oversampled", {
    # sampling without document grouping should be able to produce oversampling of a document
    # resample 10 times
    for (i in 1:10) {
        expect_equal(
            sum(stringi::stri_detect_regex(docnames(corpus_sample(corpsent, replace = TRUE, by = "_document")), "^one")),
            3
        )
    }
})

test_that("disabled nresample funtions \"work\"", {
    expect_equal(quanteda:::nresample(data_corpus_inaugural), 0)
    expect_equal(quanteda:::is.resampled(data_corpus_inaugural), FALSE)
})

test_that("corpus with prob does not work with by", {
    expect_error(
        corpus_sample(corpsent, prob = rep(0.2, 5), by = docnames(corpsent)),
        "prob not implemented with by"
    )
})
