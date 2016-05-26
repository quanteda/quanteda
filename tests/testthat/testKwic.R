context('test kwic.R')

test_that("test attr(kwic, 'ntoken') with un-named texts", {

    testkwic <- kwic(texts(c(
     'The quick brown fox jumped over the lazy dog',
     'The quick brown fox',
     'The quick brown dog jumped over the lazy dog',
     NA
     )), 'fox')

    testntokens <- c(9, 4, 9, 1)
    names(testntokens) <- c('text1', 'text2', 'text3', 'text4')

    expect_that(
        attr(testkwic, 'ntoken'),
        equals(testntokens)
    )
})

test_that("test attr(kwic, 'ntoken') text names", {
              
    testkwic <- kwic(inaugCorpus, 'american')

    expect_that(
        names(attr(testkwic, 'ntoken')),
        equals(names(texts(inaugCorpus)))
    )
})
