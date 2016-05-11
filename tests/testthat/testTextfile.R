require(quanteda)

context('Testing textfile.R')

test_that("test reading structured text files with different columns", {
    testcorpus <- textfile(
        "./tests/testthat/data/fruits*.csv",
        textField='text'
    )

    expect_that(
         docvars(testcorpus),
         equals(data.frame(list(
            color=c('green', 'orange', NA, NA), 
            shape=c(NA, NA, 'round', 'long')
            ),
            stringsAsFactors=F
        ))
    )
    expect_that(
         texts(testcorpus),
         equals(c('apple', 'orange', 'apple', 'banana'))
    )
})

