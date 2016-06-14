#TODO: redo json
# TODO: re-do docs
# TODO: Check and remove extranous codes
# TODO: Add commented-out checks, json
# TODO: encoding of non-txt file
# TODO: check encoginds length


context('test textfile.R')

test_that("test show.corpusSource", {

    expect_that(
        show(textfile( '../data/fox/fox.txt')),
        prints_text('corpusSource object consisting of 1 document and 0 docvars.')
    )

    testcorpusSource <- textfile(
        c(
            '../data/fruits/apple.txt',
            '../data/fruits/orange.txt'
         )
    )
    expect_that(
        show(testcorpusSource),
        prints_text('corpusSource object consisting of 2 documents and 0 docvars.')
    )

    expect_that(
        show(textfile('../data/csv/test.csv', textField='text')),
        prints_text('corpusSource object consisting of 2 documents and 2 docvars.')
    )


    testcorpusSource <- textfile( '../data/fox/fox.txt', cache=T)
    expect_that(
        show(testcorpusSource),
        prints_text('corpusSource object with data cached')
    )


})


# TODO: Add tests for the various method signatures


test_that("test textfile with single filename", {
    fox <- "The quick brown fox jumps over the lazy dog."
    expect_equal(
        texts(textfile('../data/fox/fox.txt')),
        fox
    )
})

test_that("test cached textfile with single filename", {
    fox <- "The quick brown fox jumps over the lazy dog."
    expect_equal(
        texts(textfile('../data/fox/fox.txt', cache=T)),
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

test_that("test structured textfile with glob-style mask", {
    expect_equal(
        length(texts(textfile(
                '../data/csv/*.csv', textField='text'
        ))),
        4
    )
    expect_equal(
        nrow(docvars(textfile(
                '../data/csv/*.csv', textField='text'
        ))),
        4
    )
})


test_that("test non-implemented functions", {

    expect_that(
        textfile('../data/empty/empty.doc'),
        throws_error('Unsupported extension doc')
    )

    expect_that(
        textfile('../data/empty/empty.docx'),
        throws_error('Unsupported extension docx')
    )

    expect_that(
        textfile('../data/empty/empty.pdf'),
        throws_error('Unsupported extension pdf')
    )


})

test_that("test warning for unrecognized filetype", {
    expect_that(
        textfile('../data/empty/empty.nonesuch'),
        throws_error('Unsupported extension nonesuch')
    )
})


# TODO: Refactor this to loop over filetypes
test_that("test csv files", {
    # Test corpus object
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

test_that("test tab files", {
    # Test corpus object
    testcorpus <- textfile('../data/tab/test.tab', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('../data/tab/test.tab', textField='nonexistant'),
        throws_error('column name nonexistant not found')
    )

})

test_that("test tsv files", {
    # Test corpus object
    testcorpus <- textfile('../data/tsv/test.tsv', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )

    expect_that(
        textfile('../data/tsv/test.tsv', textField='nonexistant'),
        throws_error('column name nonexistant not found')
    )

})


test_that("test xml files", {
    # Test corpus object
    testcorpus <- textfile('../data/xml/test.xml', textField='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), stringsAsFactors=F))
    )
    expect_that(
        texts(testcorpus),
        equals(c('Lorem ipsum.', 'Dolor sit'))
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
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )


})

test_that("test json files", {
    expect_equal(
        texts(textfile('../data/json/*json', textField='text')),
        c('Lorem ipsum', 'Dolor sit', 'The quick', 'brown fox', 'Now is the winter')
    )

    #  test.json and test2.json are newline-delimited json
    #  test3.json is a single json object
    expect_equal(
        docvars(textfile('../data/json/*json', textField='text')),
        data.frame(list(
          colour=c('green', 'red', 'orange', 'blue', NA), 
          number=c(42, 99, 0, NA, 3)),
          stringsAsFactors=F)
    )
})


# TODO: This doesn't appear to work at all
#  test_that("test zip files", {
#      # TODO: Only supports zipfiles which contain txt files
#      testcorpus <- textfile('../data/zip/test.zip')
#      expect_that(
#          texts(testcorpus),
#          equals(c('Lorem ipsum.', 'The quick', 'Dolor sit', 'brown fox'))
#      )
#  })

test_that("test textfile() with docvarsfrom=filenames", {

    expect_that(
        docvars(textfile('../data/docvars/one/*', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), stringsAsFactors=F))
    )

    expect_that(
        docvars(textfile('../data/docvars/dash/*', docvarsfrom='filenames', dvsep='-')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), stringsAsFactors=F))
    )


    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )

    expect_that(
          docvars(textfile('../data/docvars/two/*json', docvarsfrom='filenames', textField='nonesuch')),
          equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )

    expect_that(
        docvars(textfile('../data/docvars/unequal/*', docvarsfrom='filenames')),
        throws_error("Filename elements are not equal in length.")
    )

    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id', 'fruit', 'colour')
        )),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), colour=c('red', 'orange'), stringsAsFactors=F))
    )


    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id', 'fruit')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 1 docvar given generic names.')
    )
    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id', 'fruit')
        )),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    )


    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='filenames',
            docvarnames=c('id')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 2 docvars given generic names.')
    )

    #TODO: What happens if you supply more docnames?

    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='nonesuch')),
        throws_error('docvarsfrom must be')
    )

    #  Docvars from both metadata and filename
    expect_equal(
        docvars(textfile('../data/docvars/csv/*', docvarsfrom=c('filenames', 'metadata'), docvarnames=c('id', 'fruit'), textField='text')),
        data.frame(list(id=c(1, 2), fruit=c('apple', 'orange'), shape=c('round', NA), texture=c(NA, 'rough')), stringsAsFactors=FALSE)
    )

    #  Docvars from both metadata and filename
    expect_equal(
        docvars(textfile('../data/docvars/json/*', docvarsfrom=c('filenames', 'metadata'), docvarnames=c('id', 'fruit'), textField='text')),
        data.frame(list(id=c(1, 2), fruit=c('apple', 'orange'), shape=c('round', NA), texture=c(NA, 'rough')), stringsAsFactors=FALSE)
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

# Currently, these encodings don't work for reasons that seem unrelated 
# to quanteda, and are either a problem in base R or on travis-ci
broken_encodings <- c(
    "437", "850", "852", "855", "857", "860", "861", "862", "863", "865", 
    "869", "BIG5-HKSCS", "CHINESE", "CP1251", "CP1255", "CP1256", "CP1361",
    "CP154", "CP737", "CP858", "CP864", "CP856", "CP932", "CP950", "EUC-JISX0213", 
    "EUC-JP", "EUC-KR", "GB18030", "HEBREW", "HZ","ISO-2022-JP-1", "ISO-2022-JP-2", 
    "ISO-2022-JP-3", "ISO-8859-11", "ISO-IR-166", "KOI8-R",
    "UNICODE-1-1-UTF-7",
    "MACCENTRALEUROPE", "MACCYRILLIC", "MACGREEK", "MACICELAND", "MACTURKISH",
    "MS_KANJI", "SHIFT_JISX0213"
)


FILEDIR <- '../data/encoding'

filenames <- list.files(FILEDIR, "*__characters.txt$")
parts <- strsplit(gsub(".txt$", "", filenames), "__")
fileencodings <- sapply(parts, "[", 1)

fileencodings <- fileencodings[!(fileencodings %in% broken_encodings)]
filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))

test_that("test textfile encoding parameter", {
   


  for (i in 1:length(fileencodings)) {
      filename <- filenames[[i]]
      encoding <- fileencodings[[i]]
      print(paste('Testing loading from encoding', encoding))
      characters <- as.numeric(charToRaw(
        texts(textfile(filename, encoding=fileencodings[[i]]))
      ))
      bytes <- data.table::fread(gsub('__characters.txt', '__bytes.tsv', filename))[[1]]
      expect_equal(characters, bytes)
  }
})

test_that("Test loading all these files at once with different encodings", {
  encodedTextfilesCorpus <- corpus(textfile(filenames, encoding=fileencodings))
})


#  test_that("test textfile encoding parameter: UTF-8 encoded file, read as UTF-16 (should not work)", {
#       print(file.path(FILEDIR, 'UTF-8__characters.txt'))
#       print(file.exists(file.path(FILEDIR, 'UTF-8__characters.txt')))
#       expect_warning(
#         misread_texts <- texts(textfile(file.path(FILEDIR, 'UTF-8__characters.txt'), encoding='utf-16'))
#       )
#       utf8_bytes <- data.table::fread(file.path(FILEDIR, 'UTF-8__bytes.tsv'))[[1]]
#       expect_false(
#              all(as.numeric(charToRaw(misread_texts)) == utf8_bytes)
#       )
#  })

test_that("test textfile encoding parameter: ASCII encoded file, read as UTF-8: (should work)", {
   utf8_bytes <- data.table::fread(file.path(FILEDIR, 'UTF-8__bytes.tsv'))[[1]]
   expect_that(
      as.numeric(charToRaw(
          texts(textfile(file.path(FILEDIR, 'UTF-8__characters.txt'), encoding='utf-8'),
      ))),
      equals(utf8_bytes)
   )
})

context('Loading a corpus from a zip file.')

test_that("A single-level zip file containing txt files can be loaded",{
    tf <- textfile('../data/zip/inauguralTopLevel.zip')
    qc <- corpus(tf)
    expect_equal(ndoc(qc), 57)
})


test_that("test reading structured text files with different columns", {
     testcorpus <- textfile(
         "../data/fruits/*.csv",
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


# TODO: XML warning test

test_that("Test mktemp function for test dirs",{
  filename <- quanteda:::mktemp()
  expect_true(file.exists(filename))
  filename2 <- quanteda:::mktemp()
  expect_true(file.exists(filename2))
  expect_false(filename == filename2)

  # test directory parameter
  dirname <- quanteda:::mktemp(directory=T)
  expect_true(dir.exists(dirname))
  
  # test prefix parameter
  filename <- quanteda:::mktemp(prefix='testprefix')
  expect_equal(
    substr(basename(filename), 1, 10),
    'testprefix'
  )

  # test that a new filename will be given if the original already exists
  set.seed(0)
  original_filename <- quanteda:::mktemp()
  set.seed(0)
  new_filename <- quanteda:::mktemp()
  expect_false(original_filename == new_filename)
  expect_true(file.exists(original_filename))
  expect_true(file.exists(new_filename))


})

context("Tests of new textfile internals. If these fail, it doesn't necessarily affect the exposed API")

context("Tests for listMatchingFiles")

test_that("Test function to list files", {
  expect_that(
    listMatchingFiles('nonesuch://example.org/test.txt'),
    throws_error('Unsupported URL scheme')
  )           

  testExistingFile <- mktemp()
  expect_equal(listMatchingFiles(testExistingFile), testExistingFile)
  expect_equal(listMatchingFiles(paste0('file://',testExistingFile)), testExistingFile)


  # Test vector of filenames
  testExistingFile2 <- mktemp()
  expect_equal(
    listMatchingFiles(c(testExistingFile, testExistingFile2)),
    c(testExistingFile, testExistingFile2)
  )

  # TODO Test vector of filename and URL
  #  expect_equal(
  #    listMatchingFiles(c(testExistingFile, testExistingFile2)),
  #    c(testExistingFile, testExistingFile2)
  #  )

  file.remove(testExistingFile)
  expect_that(
    listMatchingFiles(testExistingFile),
    throws_error('File does not exist')
   )
  expect_equal(
    listMatchingFiles(testExistingFile, ignoreMissing=T),
    character(0)
   )


  # Test globbing
  tempdir <- mktemp(directory=T)

  file.create(file.path(tempdir, '1.tsv'))
  file.create(file.path(tempdir, '2.tsv'))
  file.create(file.path(tempdir, '10.tsv'))

  expect_equal(
    length(listMatchingFiles(paste0(tempdir, '/', '*.tsv' ))),
    3
  )

  expect_equal(
    length(listMatchingFiles(paste0(tempdir, '/', '?.tsv' ))),
    2
  )

  expect_that(
    length(listMatchingFiles(paste0(tempdir, '/', '?.txt' ))),
    throws_error('File does not exist')
  )


  # Test globbing subdir

  tempsubdir1 <- mktemp(base_path=tempdir, directory=T)
  tempsubdir2 <- mktemp(base_path=tempdir, directory=T)

  file.create(file.path(tempsubdir1, '1.tsv'))
  file.create(file.path(tempsubdir1, '2.tsv'))
  file.create(file.path(tempsubdir2, '1.tsv'))

  expect_equal(
    length(listMatchingFiles(paste0(tempdir, '/', '*/', '?.tsv' ))),
    3
  )


  expect_that(
    listMatchingFiles('http://example.org/test.nonesuch'),
    throws_error('Remote URL does not end in known extension.')
  )


  expect_that(
    listMatchingFiles('http://www.google.com/404.txt'),
    throws_error('cannot open URL')
  )

  expect_true(
    is.null(listMatchingFiles('http://www.google.com/404.txt', ignoreMissing=T))
  )


})
