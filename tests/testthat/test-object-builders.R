context("test object builder functions")

test_that("corpus object builder retains class", {
    corp <- corpus(c("a / b", "a / b c", "abc"),
                   docvars = data.frame(dv = 11:13),
                   meta = list(mymeta = "Test meta"))
    class(corp) <- c("myclass", class(corp))
    expect_identical(
        class(corp),
        class(corpus_sample(corp, size = 2))
    )
    expect_identical(
        class(corp),
        class(corpus_segment(corp, "/"))
    )
    expect_identical(
        class(corp),
        class(corpus_reshape(corp, to = "sentences"))
    )
    expect_identical(
        class(corp),
        class(corp[1:2])
    )
})

test_that("tokens object builder retains class", {
    corp <- corpus(c("a / b", "a / b c", "abc"),
                   docvars = data.frame(dv = 11:13),
                   meta = list(mymeta = "Test meta"))
    toks <- tokens(corp)
    class(toks) <- c("myclass", class(toks))
    
    expect_identical(
        class(toks),
        class(tokens_sample(toks, size = 2))
    )
    expect_identical(
        class(toks),
        class(toks[1:2])
    )
})
