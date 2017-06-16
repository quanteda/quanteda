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
      
    toks <- tokens(c("a b c d e a_b_c d e"))
    feat <- 'a b c'
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
    feat <- '* d e'
    expect_equivalent(
        as.list(tokens_compound(toks, features = feat))[[1]],
        c("a", "b", "c_d_e", "a_b_c_d_e"))
    
    expect_equivalent(
        as.list(tokens_select(toks, features = feat))[[1]],
        c("c", "d", "e", "a_b_c", "d", "e"))
    
    expect_equivalent(
        as.list(tokens_remove(toks, features = feat))[[1]],
        c("a", "b"))
    
    expect_equivalent(
        kwic(toks, keywords = feat)[,5],
        c("c d e a_b_c d e"))
    
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
    dict <- dictionary(ABC = 'a b c', D = 'd', E = 'e')
    expect_equivalent(
        as.list(tokens_compound(toks, features = dict))[[1]],
        c("a_b_c", "d", "e", "a_b_c", "d", "e"))
    
    expect_equivalent(
        as.list(tokens_select(toks, features = dict))[[1]],
        c("a", "b", "c", "d", "e", "d", "e"))
    
    expect_equivalent(
        as.list(tokens_remove(toks, features = dict))[[1]],
        c("a_b_c"))
    
    expect_equivalent(
        kwic(toks, keywords = dict)[,5],
        c("a b c d e", "d e"))
})

test_that("dictionary works consistently on dfm", {
    
    mx <- dfm(c("a b c d e a_b_c d e"))
    dict <- dictionary(ABC = 'a b c', D = 'd', E = 'e')
    expect_equivalent(
        featnames(dfm_select(mx, features = feat)),
        character())
    
    expect_equivalent(
        featnames(dfm_remove(mx, features = feat)),
        c("a", "b", "c", "d", "e", "a_b_c"))
})

