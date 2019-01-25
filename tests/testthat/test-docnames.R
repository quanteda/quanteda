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

    expect_equal(docvars(corp, "_document"), name_new)
    expect_equal(docvars(toks, "_document"), name_new)
    expect_equal(docvars(mx, "_document"), name_new)
})

test_that("docnames are character only (#1574)", {
    # for corpus
    corp <- corpus(c("a b c", "x y z"), docnames = 1:2)
    expect_is(docnames(corp), "character")

    # for tokens
    toks <- tokens(c("a b c", "x y z"))
    docnames(toks) <- 1:2
    expect_is(docnames(toks), "character")
    names(toks) <- 1:2
    expect_is(docnames(toks), "character")

    # for tokens
    dfmat <- dfm(c("a b c", "x y z"))
    docnames(dfmat) <- 1:2
    expect_is(docnames(dfmat), "character")
    rownames(dfmat) <- 1:2
    expect_is(docnames(dfmat), "character")
})
