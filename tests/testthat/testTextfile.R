#TODO: merge json
#TODO: merge rbindall
# TODO: RTL
# TODO: bi-directional text
# TODO: encodings in filenames


context('test textfile.R')

TESTDATADIR <- "tests/data/"

test_that("test show.corpusSource", {

    expect_that(
        show(textfile( 'tests/data/fox/fox.txt')),
        prints_text('corpusSource object consisting of 1 document and 0 docvars.')
    )

    testcorpusSource <- textfile(
        c(
            'tests/data/fruits/apple.txt',
            'tests/data/fruits/orange.txt'
         )
    )
    expect_that(
        show(testcorpusSource),
        prints_text('corpusSource object consisting of 2 documents and 0 docvars.')
    )

    expect_that(
        show(textfile('tests/data/csv/test.csv', textField='text')),
        prints_text('corpusSource object consisting of 2 documents and 2 docvars.')
    )


    testcorpusSource <- textfile( 'tests/data/fox/fox.txt', cache=T)
    expect_that(
        show(testcorpusSource),
        prints_text('corpusSource object with data cached')
    )


})


# TODO: Add tests for the various method signatures


test_that("test textfile with single filename", {
    fox <- "The quick brown fox jumps over the lazy dog."
    expect_equal(
        texts(textfile('tests/data/fox/fox.txt')),
        fox
    )
})

test_that("test cached textfile with single filename", {
    fox <- "The quick brown fox jumps over the lazy dog."
    expect_equal(
        texts(textfile('tests/data/fox/fox.txt', cache=T)),
        fox
    )
})

test_that("test classes, slots, and extractor functions", {

    testtextfile <- textfile('tests/data/fox/fox.txt')


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
                'tests/data/fruits/apple.txt',
                'tests/data/fruits/orange.txt'
             )
        ))),
        2
    )
})

test_that("test textfile with glob-style mask", {
    expect_equal(
        length(texts(textfile(
                'tests/data/glob/*.txt'
        ))),
        5
    )

    expect_equal(
        length(texts(textfile(
                'tests/data/glob/?.txt'
        ))),
        4
    )


    # TODO: Glob in non-last part of filename
    #  expect_equal(
    #      length(texts(textfile(
    #              'tests/data/glob/*/1.txt'
    #      ))),
    #      2
    #  )


    # TODO: glob multiple filetypes
    # expect_equal(
    #     length(texts(textfile(
    #             'tests/data/glob/*'
    #     ))),
    #     6
    # )

    # TODO: glob special characters in filename

})

test_that("test structured textfile with glob-style mask", {
    expect_equal(
        length(texts(textfile(
                'tests/data/csv/*.csv', textField='text'
        ))),
        4
    )
    expect_equal(
        nrow(docvars(textfile(
                'tests/data/csv/*.csv', textField='text'
        ))),
        4
    )
})


test_that("test non-implemented functions", {

    expect_that(
        textfile('tests/data/empty/test.doc'),
        throws_error('doc files not implemented yet')
    )

    expect_that(
        textfile('tests/data/empty/test.docx'),
        throws_error('doc files not implemented yet')
    )

    expect_that(
        textfile('tests/data/empty/test.pdf'),
        throws_error('pdf files not implemented yet')
    )


})

test_that("test warning for unrecognized filetype", {
    expect_that(
        textfile('tests/data/empty/test.nonesuch'),
        throws_error('unrecognized fileType:unknown')
    )
})


# TODO: Refactor this to loop over filetypes
test_that("test csv files", {
    # Test corpus object
    testcorpus <- textfile('tests/data/csv/test.csv', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('tests/data/csv/test.csv', textField='nonexistant'),
        throws_error('column name nonexistant not found')
    )

})

test_that("test tab files", {
    # Test corpus object
    testcorpus <- textfile('tests/data/tab/test.tab', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('tests/data/tab/test.tab', textField='nonexistant'),
        throws_error('column name nonexistant not found')
    )

})

test_that("test tsv files", {
    # Test corpus object
    testcorpus <- textfile('tests/data/tsv/test.tsv', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('tests/data/tsv/test.tsv', textField='nonexistant'),
        throws_error('column name nonexistant not found')
    )

})


test_that("test xml files", {
    # Test corpus object
    testcorpus <- textfile('tests/data/xml/test.xml', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('tests/data/xml/test.xml', textField='nonexistant'),
        throws_error('node .* not found')
    )

    expect_that(
        textfile('tests/data/xml/test.xml', textField=1),
        gives_warning('You should specify textField by name.*')
    )
    expect_that(
        texts(textfile('tests/data/xml/test.xml', textField=1)),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )


})

# TODO: This doesn't appear to work at all
#  test_that("test zip files", {
#      # TODO: Only supports zipfiles which contain txt files
#      testcorpus <- textfile('tests/data/zip/test.zip')
#      expect_that(
#          texts(testcorpus),
#          equals(c('Lorem ipsum.', 'The quick', 'Dolor sit', 'brown fox'))
#      )
#  })

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

test_that("test textfile() with docvarsfrom=filenames", {

    expect_that(
        docvars(textfile('tests/data/docvars/one/*', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), stringsAsFactors=F))
    )

    expect_that(
        docvars(textfile('tests/data/docvars/dash/*', docvarsfrom='filenames', dvsep='-')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), stringsAsFactors=F))
    )


    expect_that(
        docvars(textfile('tests/data/docvars/two/*txt', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )

    #  docvarsfrom='filenames' only works with txt files, by design?
    #  expect_that(
    #      docvars(textfile('tests/data/docvars/two/*json', docvarsfrom='filenames', textField='nonesuch')),
    #      equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    #  )

    expect_that(
        docvars(textfile('tests/data/docvars/unequal/*', docvarsfrom='filenames')),
        throws_error("Filename elements are not equal in length.")
    )

    expect_that(
        docvars(textfile('tests/data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id', 'fruit', 'colour')
        )),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), colour=c('red', 'orange'), stringsAsFactors=F))
    )


    expect_that(
        docvars(textfile('tests/data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id', 'fruit')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 1 docvar given generic names.')
    )
    expect_that(
        docvars(textfile('tests/data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id', 'fruit')
        )),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )


    expect_that(
        docvars(textfile('tests/data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 2 docvars given generic names.')
    )

    #TODO: What happens if you supply more docnames?

    expect_that(
        docvars(textfile('tests/data/docvars/two/1_apple_red.txt', docvarsfrom='filenames')),
        throws_error('File type txt not supported with these arguments.')
    )

    expect_that(
        docvars(textfile('tests/data/docvars/two/*txt', docvarsfrom='nonesuch')),
        gives_warning('docvarsfrom=nonesuch not supported.')
    )




})

test_that("test texts.corpusSource error with groups!=NULL", {
    expect_that(
        texts(textfile('tests/data/fox/fox.txt'), groups='anything'),
        throws_error()
     )
})

test_that("test docvars.corpusSource warning with field!=NULL", {
    expect_that(
        docvars(textfile('tests/data/fox/fox.txt'), field='anything'),
        gives_warning()
     )
})


test_that("test textfile encoding parameter", {

  fox <- "The quick brown fox jumps over the lazy dog."
  # Test ASCII encoded file, read as ASCII
   expect_that(
      texts(textfile('tests/data/encoding/ascii.txt', encoding='ascii')),
      equals(fox)
    )
  # Test ASCII encoded file, read as UTF-8
   expect_that(
      texts(textfile('tests/data/encoding/ascii.txt', encoding='utf-8')),
      equals(fox)
    )
  # Test ASCII encoded file, read as UTF-16: should not work
   expect_that(
      texts(textfile('tests/data/encoding/ascii.txt', encoding='utf-16')) == fox,
      is_false()
    )

  # Test Latin-1/ISO-8859-1 encoded text
   expect_that(
      texts(textfile('tests/data/encoding/latin1.txt', encoding='latin1')),
      equals('"Fix, Schwyz!" quäkt Jürgen blöd vom Paß')
    )

   
  ####
  #### See ?encodedTextFiles
  ####
   
  # Test Unicode encoded files:
  #    - UTF-8 with BOM
  #    - UTF-8 without BOM
  #    - UTF-16 big-endian with BOM
  #    - UTF-16 big-endian without BOM
  #    - UTF-16 little-endian with BOM
  #    - UTF-16 little-endian without BOM
  #    - Windows-1252

   # frenchText <- "Le cœur déçu mais l'âme plutôt naïve, Louÿs rêva de crapaüter en canoë au delà des îles, près du mälström où brûlent les novæ."
   # 
   # encodings = c('utf-8',       'utf-8',     'utf-16',     'utf-16',       'utf-16',       'utf-16le',       'latin1', 'windows-1252')
   # filenames = c('utf-8-nobom', 'utf-8-bom', 'utf-16-bom', 'utf-16-nobom', 'utf-16le-bom', 'utf-16le-nobom', 'latin1', 'windows-1252')
   # 
   # for (i in 1:length(encodings)) {
   #   print(paste(filenames[[i]], encodings[[i]]))
   #   expect_that(
   #    texts(textfile(paste0('tests/data/encoding/', filenames[[i]], '.txt'), encoding=encodings[[i]])),
   #    equals(frenchText)
   #  )
   # }

  })
