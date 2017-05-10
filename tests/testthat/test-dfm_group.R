context("test dfm_select")


test_that("test dfm_group", {
    
    testdfm <- dfm(c('a b c c', 'b c d', 'a'))
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c('doc1', 'doc1', 'doc2'))),
        matrix(c(1, 1, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
    
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c(1, 1, 2))),
        matrix(c(1, 1, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
})

test_that("dfm_group works with empty documents", {
    
    testdfm <- dfm(c('a b c c', 'b c d', ''))
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c('doc1', 'doc1', 'doc2'))),
        matrix(c(1, 0, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
    
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c(1, 1, 2))),
        matrix(c(1, 0, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
})
