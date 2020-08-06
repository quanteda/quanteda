context("test caching")

test_that("test caching is working", {
    skip_on_os("solaris")
    
    corp <- corpus(c("a b c d"))
    toks <- tokens(corp)
    dfmt <- dfm(toks)
    
    cache1 <- sample(1:100, 10)
    quanteda:::set_cache(corp, "something", cache1)
    expect_identical(attr(corp, "meta")$object$something$data, cache1)
    expect_identical(quanteda:::get_cache(corp, "something"), cache1)
    
    cache2 <- sample(1:100, 10)
    quanteda:::set_cache(toks, "something", cache2)
    expect_identical(attr(toks, "meta")$object$something$data, cache2)
    expect_identical(quanteda:::get_cache(toks, "something"), cache2)
    
    cache3 <- sample(1:100, 10)
    quanteda:::set_cache(dfmt, "something", cache3)
    expect_identical(attr(dfmt, "meta")$object$something$data, cache3)
    expect_identical(quanteda:::get_cache(dfmt, "something"), cache3)
    
})

test_that("test caching is passed to downstream objects", {
    skip_on_os("solaris")

    cache <- data.frame(number = sample(1:100, 10))
    corp <- corpus(c("a b c d"))
    quanteda:::set_cache(corp, "summary", cache)
    expect_identical(attr(corp, "meta")$object$summary$data, cache)
    toks <- tokens(corp)
    expect_identical(attr(toks, "meta")$object$summary$data, cache)
    dfmt <- dfm(toks)
    expect_identical(dfmt@meta$object$summary$data, cache)
})
