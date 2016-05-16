context('test textfile.R')

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

    # TODO: Add tests for cached corpus

})

fox <- "The quick brown fox jumps over the lazy dog."

test_that("test textfile with single filename", {
    expect_equal(
        texts(textfile('tests/testthat/data/fox/fox.txt')),
        fox
    )

})

test_that("test textfile with vector of filenames", {
    expect_equal(
        length(texts(textfile(
            c(
                'tests/testthat/data/fruits/apple.txt',
                'tests/testthat/data/fruits/orange.txt'
             )
        ))),
        2
    )
})

test_that("test textfile with glob-style mask", {
    expect_equal(
        length(texts(textfile(
                'tests/testthat/data/glob/*.txt'
        ))),
        5
    )

    expect_equal(
        length(texts(textfile(
                'tests/testthat/data/glob/?.txt'
        ))),
        4
    )


    # Glob in non-last part of filename
    expect_equal(
        length(texts(textfile(
                'tests/testthat/data/glob/*/1.txt'
        ))),
        2
    )


    # TODO: glob multiple filetypes
    # expect_equal(
    #     length(texts(textfile(
    #             'tests/testthat/data/glob/*'
    #     ))),
    #     6
    # )

    # TODO: glob special characters in filename

})


test_that("test texts.corpusSource error with groups!=NULL", {
    expect_that(
        texts(textfile('tests/testthat/data/fox/fox.txt'), groups='anything'),
        throws_error()
     )
})

test_that("test docvars.corpusSource warning with field!=NULL", {
    expect_that(
        docvars(textfile('tests/testthat/data/fox/fox.txt'), field='anything'),
        gives_warning()
     )
})

test_that("test getdocvarsFromFilenames", {

    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple.txt', '2_orange.txt')
        ),
        equals(data.frame(list(docvar1=c('1','2'), docvar2=c('apple', 'orange')), stringsAsFactors=F))
    )

    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.txt', '2_orange_orange.txt')
        ),
        equals(data.frame(list(docvar1=c('1','2'), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )


})
