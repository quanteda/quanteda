context("test spacyr methods")

test_that("test quanetda methods for spacy_parsed objects", {
    load("../data/data_spacy_parsed.RData")
    expect_equal(docnames(data_spacy_parsed), "text1")
    expect_equal(ndoc(data_spacy_parsed), 1)
    expect_equal(ntype(data_spacy_parsed), c(text1 = 7))
    expect_equal(ntoken(data_spacy_parsed), c(text1 = 7))
})
