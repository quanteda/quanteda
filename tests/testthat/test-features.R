context('test features.R')

test_that("character vector works consistently on tokens", {
    
    toks <- tokens(c("a b c d e a_b_c d e"))
    feat <- c('a', 'b', 'c')
    expect_equivalent(
        as.list(tokens_compound(toks, features = feat))[[1]],
        c("a", "b", "c", "d", "e", "a_b_c", "d", "e"))
    
    expect_equivalent(
        as.list(tokens_select(toks, features = feat))[[1]],
        c("a", "b", "c"))
    
    expect_equivalent(
        as.list(tokens_remove(toks, features = feat))[[1]],
        c("d", "e", "a_b_c", "d", "e"))
    
    expect_equivalent(
        kwic(toks, keywords = feat)[,5],
        c("a b c"))
})

test_that("character vector works consistently on dfm", {
    
    mx <- dfm(c("a b c d e a_b_c d e"))
    feat <- c('a', 'b', 'c')

    expect_equivalent(
        featnames(dfm_select(mx, features = feat)),
        c("a", "b", "c"))
    
    expect_equivalent(
        featnames(dfm_remove(mx, features = feat)),
        c("d", "e", "a_b_c"))
})

test_that("character vector with whitespace works consistently on tokens", {
      
    txt <- c("a b c d e a_b_c d e")
    toks <- tokens(txt)
    toksch <- as.character(toks)
    feat <- 'a b c'
    expect_equivalent(
        as.list(tokens_compound(toks, features = feat))[[1]],
        toksch
    )
    expect_equivalent(
        as.list(tokens_compound(toks, features = phrase(feat)))[[1]],
        c("a_b_c", "d", "e", "a_b_c", "d", "e")
    )

    expect_equivalent(
        as.list(tokens_select(toks, features = feat))[[1]],
        character(0)
    )
    expect_equivalent(
        as.list(tokens_select(toks, features = phrase(feat)))[[1]],
        c("a", "b", "c")
    )
    
    expect_equivalent(
        as.list(tokens_remove(toks, features = feat))[[1]],
        toksch
    )
    
    expect_equal(
        nrow(kwic(toks, keywords = feat)),
        0
    )
})

test_that("character vector with whitespace works consistently on dfm", {
    
    mx <- dfm(c("a b c d e a_b_c d e"))
    feat <- 'a b c'
    expect_equivalent(
        featnames(dfm_select(mx, features = feat)),
        character())
    
    expect_equivalent(
        featnames(dfm_remove(mx, features = feat)),
        c("a", "b", "c", "d", "e", "a_b_c"))
})

test_that("character vector with whitespace and wildcard works consistent on tokens", {
    
    toks <- tokens(c("a b c d e a_b_c d e"))
    toksch <- as.character(toks)
    feat <- '* d e'
    expect_equivalent(
        as.list(tokens_compound(toks, features = feat))[[1]],
        toksch
    )
    expect_equivalent(
        as.list(tokens_compound(toks, features = phrase(feat)))[[1]],
        c("a", "b", "c_d_e", "a_b_c_d_e")
    )

    expect_equivalent(
        as.list(tokens_select(toks, features = feat))[[1]],
        character(0)
    )
    expect_equivalent(
        as.list(tokens_select(toks, features = phrase(feat)))[[1]],
        c("c", "d", "e", "a_b_c", "d", "e")
    )
    
    expect_equivalent(
        as.list(tokens_remove(toks, features = feat))[[1]],
        toksch
    )
    expect_equivalent(
        as.list(tokens_remove(toks, features = phrase(feat)))[[1]],
        c("a", "b")
    )
    
    expect_equal(nrow(kwic(toks, keywords = feat)), 0)
    
    ## INCORRECT - TO FIX
    # kwic(toks, keywords = phrase(feat))

})

test_that("list works consistently on tokens", {
    
    toks <- tokens(c("a b c d e a_b_c d e"))
    feat <- list(c('a', 'b', 'c'))
    expect_equivalent(
        as.list(tokens_compound(toks, features = feat))[[1]],
        c("a_b_c", "d", "e", "a_b_c", "d", "e"))
    
    expect_equivalent(
        as.list(tokens_select(toks, features = feat))[[1]],
        c("a", "b", "c"))
    
    expect_equivalent(
        as.list(tokens_remove(toks, features = feat))[[1]],
        c("d", "e", "a_b_c", "d", "e"))
    
    expect_equivalent(
        kwic(toks, keywords = feat)[,5],
        c("a b c"))
})

test_that("dictionary works consistently on tokens", {
    
    toks <- tokens(c("a b c d e a_b_c d e"))
    toksch <- as.character(toks)
    dict <- dictionary(ABC = 'a b c', D = 'd', E = 'e')

    expect_equal(
        as.list(tokens_compound(toks, features = dict))[[1]],
        toksch
    )
    expect_equal(
        as.list(tokens_compound(toks, features = phrase(dict)))[[1]],
        c("a_b_c", "d", "e", "a_b_c", "d", "e")
    )

    expect_equal(
        as.list(tokens_select(toks, features = dict))[[1]],
        c("d", "e", "d", "e")
    )
    expect_equal(
        as.list(tokens_select(toks, features = phrase(dict)))[[1]],
        c("a", "b", "c", "d", "e", "d", "e")
    )
    
    expect_equal(
        as.list(tokens_remove(toks, features = dict))[[1]],
        c("a", "b", "c", "a_b_c")
    )
    expect_equal(
        as.list(tokens_remove(toks, features = phrase(dict)))[[1]],
        c("a_b_c")
    )
    
    expect_equal(
        as.data.frame(kwic(toks, keywords = dict))$keyword,
        c("d e", "d e")
    )
    ## INCORRECT - TO FIX
    expect_equal(
        as.data.frame(kwic(toks, keywords = phrase(dict)))$keyword,
        c("a b c d e", "d e")
    )
})

test_that("dictionary works consistently on dfm", {
    
    mx <- dfm(c("a b c d e a_b_c d e"))
    dict <- dictionary(ABC = 'a_b_c', D = 'd', E = 'e')
    expect_equivalent(
        featnames(dfm_select(mx, features = dict)),
        c('d', 'e', 'a_b_c'))
    
    expect_equivalent(
        featnames(dfm_remove(mx, features = dict)),
        c("a", "b", "c"))
})

