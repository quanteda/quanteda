context('test tokens_group.R')


test_that("test that tokens_group is working", {
    
    txts <- c('a b c d', 'e f g h', 'A B C', 'X Y Z')
    toks <- tokens(txts)
    expect_equal(
        as.list(quanteda:::tokens_group(toks, c(1, 1, 2, 2))),
        list('1' = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
             '2' = c('A', 'B', 'C', 'X', 'Y', 'Z'))
    )
    
    expect_equal(
        as.list(quanteda:::tokens_group(toks, c(2, 1, 2, 1))),
        list('2' = c('a', 'b', 'c', 'd', 'A', 'B', 'C'),
             '1' = c('e', 'f', 'g', 'h', 'X', 'Y', 'Z'))
    )
    
    expect_equal(
        as.list(quanteda:::tokens_group(toks, c('Z', 'A', 'Z', 'A'))),
        list('Z' = c('a', 'b', 'c', 'd', 'A', 'B', 'C'),
             'A' = c('e', 'f', 'g', 'h', 'X', 'Y', 'Z'))
    )
    
})

test_that("tokens_group works with empty documents", {
    
    toks <- tokens(c(doc1 = 'a b c c', doc2 = 'b c d', doc3 = ''))
    expect_equivalent(
        as.list(quanteda:::tokens_group(toks, c('doc1', 'doc1', 'doc2'))),
        list(doc1 = c("a", "b", "c", "c", "b", "c", "d"), doc2 = character())
    )
    
    expect_equivalent(
        as.list(quanteda:::tokens_group(toks, c(1, 1, 2))),
        list(doc1 = c("a", "b", "c", "c", "b", "c", "d"), doc2 = character())
    )
})

test_that("dfm_group and tokes_group are equivalent", {
    
    txts <- c('a b c c', 'b c d', 'a')
    toks <- tokens(txts)

    expect_identical(
        dfm_group(dfm(toks), c('doc1', 'doc1', 'doc2')),
        dfm(quanteda:::tokens_group(toks, c('doc1', 'doc1', 'doc2'))))
    
    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 2)),
        dfm(quanteda:::tokens_group(toks, c(1, 1, 2))))

})

