context("test corpus_sample")

corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                 two = "First sentence, doc2.  Second sentence, doc2."))
corp_sent <- corpus_reshape(corp, to = "sentences")

test_that("test corpus_sample to see if without grouping, documents can be oversampled", {
    set.seed(100)
    corp_samp <- corpus_sample(corp_sent, replace = TRUE)
    expect_gt(sum(stringi::stri_detect_regex(docnames(corp_samp), "^one")), 3)         
})

test_that("test corpus_sample to see if with grouping, documents can be oversampled", {
    for (i in 1:10) {
        corp_samp <- corpus_sample(corp_sent, replace = TRUE, by = "document")
        expect_equal(
            sum(stringi::stri_detect_regex(docnames(corpus_sample(corp_samp, replace = TRUE, by = "docid_")), "^one")),
            3
        )
    }
})

test_that("corpus with prob does not work with by", {
    expect_error(
        corpus_sample(corp_sent, prob = rep(0.2, 5), by = docnames(corp_sent)),
        "prob not implemented with by"
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

test_that("corpus_sample works with one docvar (#1813)", {
    corp <- corpus(LETTERS[1:5], 
                   docvars = data.frame(int = 1:5))
    expect_true(
        setequal(docvars(corpus_sample(corp), "int"), 1:5)
    )
    expect_true(
        all(docvars(corpus_sample(corp, 4, replace = TRUE), "int") %in% 1:5),
    )
})
