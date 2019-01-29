context("test docnames")

test_that("docnames always return names even if there aren't", {
    corp <- corpus(c("aaa", "bbb", "ccc"))
    expect_equal(length(docnames(corp)), ndoc(corp))

    toks <- as.tokens(list("aaa", "bbb", "ccc"))
    expect_equal(length(docnames(toks)), ndoc(toks))
})

test_that("docnames<- works with corpus, tokens and dfm (#987)", {
    corp <- corpus(c("aaa", "bbb", "ccc"))
    toks <- tokens(corp)
    mx <- dfm(toks)

    name_new <- c("doc1", "doc2", "doc3")
    docnames(corp) <- name_new
    docnames(toks) <- name_new
    docnames(mx) <- name_new

    expect_equal(docnames(corp), name_new)
    expect_equal(docnames(toks), name_new)
    expect_equal(docnames(mx), name_new)
    expect_equal(attr(corp, "docvars")[["docname_"]], name_new)
    expect_equal(attr(toks, "docvars")[["docname_"]], name_new)
    expect_equal(attr(mx, "docvars")[["docname_"]], name_new)
})

test_that("dfm docnames cannot be made integer before textstat_simil", {
    dfmat <- data_dfm_lbgexample
    dfmat@Dimnames$docs <- seq_len(ndoc(dfmat))
    expect_identical(
        dimnames(as.matrix(textstat_simil(dfmat)))[[1]],
        as.character(1:6)
    )
})
