context('test corpus.R')

test_that("test show.corpus", {

    testcorpus <- corpus(c('The'))
    expect_that(
        show(testcorpus),
        prints_text('Corpus consisting of 1 document.')
    )


    testcorpus <- corpus(
        c('The', 'quick', 'brown', 'fox')
    )
    expect_that(
        show(testcorpus),
        prints_text('Corpus consisting of 4 documents.')
    )

    testcorpus <- corpus(
        c('The', 'quick', 'brown', 'fox'),
        docvars=data.frame(list(test=1:4))
    )
    expect_that(
        show(testcorpus),
        prints_text('Corpus consisting of 4 documents and 1 docvar.')
    )

    testcorpus <- corpus(
        c('The', 'quick', 'brown', 'fox'),
        docvars=data.frame(list(test=1:4, test2=1:4))
    )
    expect_that(
        show(testcorpus),
        prints_text('Corpus consisting of 4 documents and 2 docvars.')
    )

})
