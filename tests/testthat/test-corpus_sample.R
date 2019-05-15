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

test_that("sample_bygroup works with sizes", {
    grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
    
    # sampling same size each group
    tmp <- quanteda:::sample_bygroup(1:10, group = grvec, size = 2, replace = TRUE)
    expect_true(all(tmp[1:2] >= 0 & tmp[1:2] <= 3))
    expect_true(all(tmp[3:4] >= 4 & tmp[3:4] <= 7))
    expect_true(all(tmp[5:6] >= 8 & tmp[5:6] <= 10))
    
    # sampling from a vector of sizes
    tmp <- quanteda:::sample_bygroup(1:10, group = grvec, size = c(1, 1, 3), replace = TRUE)
    expect_true(all(tmp[1] >= 0 & tmp[1] <= 3))
    expect_true(all(tmp[2] >= 4 & tmp[2] <= 7))
    expect_true(all(tmp[3:5] >= 8 & tmp[3:5] <= 10))
    
    # default group size sampling
    tmp <- quanteda:::sample_bygroup(1:10, group = grvec, replace = TRUE)
    expect_true(all(tmp[1:3] >= 0 & tmp[1:3] <= 3))
    expect_true(all(tmp[4:7] >= 4 & tmp[4:7] <= 7))
    expect_true(all(tmp[8:10] >= 8 & tmp[8:10] <= 10))
    
    # oversampling within group
    tmp <- quanteda:::sample_bygroup(c(1:2, c(101:102)), 
                                     group = rep(letters[1:2], each = 2),
                                     size = 5, replace = TRUE)
    expect_true(all(tmp[1:5] >= 0 & tmp[1:5] <= 2))
    expect_true(all(tmp[6:10] >= 100 & tmp[6:10] <= 102))
    
    expect_error(
        quanteda:::sample_bygroup(1:10, group = grvec, size = c(2, 3), replace = TRUE),
        "size not equal in length to the number of groups"
    )
})

test_that("corpus_sample by group works", {
    corp <- corpus(
        paste("Document number", seq_len(10)),
        docvars = data.frame(id = paste0("id", seq_len(10)),
                             grp = c(rep("A", 5), rep("B", 5)), 
                             stringsAsFactors = FALSE)
    )
    expect_equal(
        docvars(corpus_sample(corp, size = 2, by = "grp"), "grp"),
        rep(LETTERS[1:2], each = 2)
    )
    expect_equal(
        docvars(corpus_sample(corp, size = 25, by = "grp", replace = TRUE), "grp"),
        rep(LETTERS[1:2], each = 25)
    )
    expect_equal(
        docvars(corpus_sample(corp, by = "grp"), "grp"),
        rep(LETTERS[1:2], each = 5)
    )
})
