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


# TODO: Add tests for the various method signatures


test_that("test textfile with single filename", {
    fox <- "The quick brown fox jumps over the lazy dog."
    expect_equal(
        texts(textfile('../data/fox/fox.txt')),
        fox
    )
})

test_that("test classes, slots, and extractor functions", {

    testtextfile <- textfile('../data/fox/fox.txt')


    expect_equal(
        slotNames(testtextfile),
        c('texts', 'docvars', 'source', 'created', 'cachedfile')
    )


    expect_is(testtextfile, 'corpusSource')
    expect_is(testtextfile@texts, 'character')
    expect_is(testtextfile@docvars, 'data.frame')
    expect_is(testtextfile@source, 'character')
    expect_is(testtextfile@cachedfile, 'character')


})

test_that("test textfile with vector of filenames", {
    expect_equal(
        length(texts(textfile(
            c(
                '../data/fruits/apple.txt',
                '../data/fruits/orange.txt'
             )
        ))),
        2
    )
})

test_that("test textfile with glob-style mask", {
    expect_equal(
        length(texts(textfile(
                '../data/glob/*.txt'
        ))),
        5
    )

    expect_equal(
        length(texts(textfile(
                '../data/glob/?.txt'
        ))),
        4
    )


    # TODO: Glob in non-last part of filename
    #  expect_equal(
    #      length(texts(textfile(
    #              '../data/glob/*/1.txt'
    #      ))),
    #      2
    #  )


    # TODO: glob multiple filetypes
    # expect_equal(
    #     length(texts(textfile(
    #             '../data/glob/*'
    #     ))),
    #     6
    # )

    # TODO: glob special characters in filename

})


test_that("test non-implemented functions", {

    expect_that(
        textfile('../data/empty/test.doc'),
        throws_error('doc files not implemented yet')
    )

    expect_that(
        textfile('../data/empty/test.docx'),
        throws_error('doc files not implemented yet')
    )

    expect_that(
        textfile('../data/empty/test.pdf'),
        throws_error('pdf files not implemented yet')
    )


})

test_that("test get_csv", {
    testcorpus <- textfile('../data/csv/test.csv', textField='text')

    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )

    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('../data/csv/test.csv', textField='nonexistant'),
        throws_error('column name nonexistant not found')
    )

})

test_that("test get_xml", {
    testtext <- get_xml('../data/xml/test.xml', textField='text')
    expect_that(
        testtext$docv,
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        testtext$txts,
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    testcorpus <- textfile('../data/xml/test.xml', textField='text')

    expect_that(
        docvars(testtext),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99))))
    )
    expect_that(
        texts(testtext),
        equals(c('Lorem ipsum', 'Dolor sit'))
    )

    expect_that(
        textfile('../data/xml/test.xml', textField='nonexistant'),
        throws_error('node .* not found')
    )

    expect_that(
        textfile('../data/xml/test.xml', textField=1),
        gives_warning('You should specify textField by name.*')
    )
    expect_that(
        texts(textfile('../data/xml/test.xml', textField=1)),
        equals(c('Lorem ipsum', 'Dolor sit'))
    )


})

test_that("test getFileType", {

      expect_equal(getFileType(c('anything', 'or', 'other')), 'vector')

      expect_equal(getFileType('*.txt'), 'filemask')
      expect_equal(getFileType('http://example.org/*test.txt'), 'txt')
      expect_equal(getFileType('http://example.org/test.txt'), 'txt')

      expect_equal(getFileType('test.txt'), 'txt')
      expect_equal(getFileType('test.tar.gz'), 'gz')
      expect_equal(getFileType('test.docx'), 'word')
      expect_equal(getFileType('test.somethingelse'), 'unknown')
      expect_equal(getFileType('test'), 'unknown')

      # TODO: filenames with glob specialchars
      # TODO: Should it support multiple filenames?
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
            fnames=c('1-apple.txt', '2-orange.txt'),
            dvsep="_"
        ),
        equals(data.frame(list(docvar1=c('1','2'), docvar2=c('apple', 'orange')), stringsAsFactors=F))
    )

    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.txt', '2_orange_orange.txt')
        ),
        equals(data.frame(list(docvar1=c('1','2'), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )

    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.json', '2_orange_orange.json')
        ),
        equals(data.frame(list(docvar1=c('1','2'), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )

    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.json', '2_orange.json')
        ),
        throws_error("Filename elements are not equal in length.")
    )

    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.json', '2_orange_orange.json'),
            docvarnames=c('id', 'fruit', 'colour')
        ),
        equals(data.frame(list(id=c('1','2'), fruit=c('apple', 'orange')), colour=c('red', 'orange'), stringsAsFactors=F))
    )


    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.json', '2_orange_orange.json'),
            docvarnames=c('id', 'fruit')
        ),
        gives_warning('Fewer docnames supplied than exist docvars - last 1 docvars were given generic names.')
    )


    expect_that(
        getdocvarsFromFilenames(
            fnames=c('1_apple_red.json', '2_orange_orange.json'),
            docvarnames=c('id', 'fruit')
        ),
        equals(data.frame(list(id=c('1','2'), fruit=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )



})

test_that("test texts.corpusSource error with groups!=NULL", {
    expect_that(
        texts(textfile('../data/fox/fox.txt'), groups='anything'),
        throws_error()
     )
})

test_that("test docvars.corpusSource warning with field!=NULL", {
    expect_that(
        docvars(textfile('../data/fox/fox.txt'), field='anything'),
        gives_warning()
     )
})


