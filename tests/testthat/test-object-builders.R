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


test_that("object builder is robust agains different input", {
    
    # user-difined class is given
    corp <- quanteda:::build_corpus("a b c", 
                                    docvars = quanteda:::make_docvars(1L),
                                    class = "myclass")
    expect_equal(
        class(corp), c("myclass", "corpus", "character")
    )
    class(corp) <- "myclass2"
    corp2 <- quanteda:::rebuild_corpus(corp, attributes(corp))
    expect_equal(
        class(corp2), c("myclass2", "corpus", "character")
    )
    
    toks <- quanteda:::build_tokens(list(1:3), 
                                    types = c("a", "b", "c"), 
                                    docvars = quanteda:::make_docvars(1L),
                                    class = "myclass")
    expect_equal(
        class(toks), c("myclass", "tokens")
    )
    class(toks) <- "myclass2"
    toks2 <- quanteda:::rebuild_tokens(toks, attributes(toks))
    expect_equal(
        class(toks2), c("myclass2", "tokens")
    )
    
    # docvars has wrong number of rows
    expect_error({
        quanteda:::build_corpus("a b c", 
                                docvars = quanteda:::make_docvars(2L))
    })
    expect_error({
        quanteda:::build_tokens(list(1:3), 
                                types = c("a", "b", "c"), 
                                docvars = quanteda:::make_docvars(2L))
    })
    
})

test_that("object rebuilder keeps attributes", {
    mat1 <- matrix(c(1, 2, 0, 0, 1, 1), nrow = 2)
    dfmat1 <- quanteda:::build_dfm(mat1,
                                   c("a", "b", "c"),
                                   docvars = quanteda:::make_docvars(2L))
    dfmat2 <- quanteda:::rebuild_dfm(dfmat1, attributes(dfmat1))
    expect_identical(attributes(dfmat1),
                     attributes(dfmat2))
    
    mat2 <- matrix(c(1, 2, 3, 0, 0, 1, 1, 2, 1), nrow = 3)
    fcmat1 <- quanteda:::build_fcm(mat2,
                                   c("a", "b", "c"))
    fcmat2 <- quanteda:::rebuild_fcm(fcmat1, attributes(fcmat1))
    expect_identical(attributes(fcmat1),
                     attributes(fcmat2))
    
    lis <- list(A = list(c("a", "aa", "AA")))
    dict1 <- quanteda:::build_dictionary2(lis, 
                                          valuetype = "regex", 
                                          separator = "+")
    dict2 <- quanteda:::rebuild_dictionary2(dict1, attributes(dict1))
    expect_identical(attributes(dict1),
                     attributes(dict2))
})
