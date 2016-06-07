#TODO: merge json
#TODO: merge rbindall
# TODO: RTL
# TODO: bi-directional text
# TODO: encodings in filenames


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

test_that("test warning for unrecognized filetype", {
    expect_that(
        textfile('../data/empty/test.nonesuch'),
        throws_error('unrecognized fileType:unknown')
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

# TODO: This doesn't appear to work at all
#  test_that("test zip files", {
#      # TODO: Only supports zipfiles which contain txt files
#      testcorpus <- textfile('../data/zip/test.zip')
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

    #  docvarsfrom='filenames' only works with txt files, by design?
    #  expect_that(
    #      docvars(textfile('../data/docvars/two/*json', docvarsfrom='filenames', textField='nonesuch')),
    #      equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), stringsAsFactors=F))
    #  )

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
        docvars(textfile('../data/docvars/two/1_apple_red.txt', docvarsfrom='filenames')),
        throws_error('File type txt not supported with these arguments.')
    )

    expect_that(
        docvars(textfile('../data/docvars/two/*txt', docvarsfrom='nonesuch')),
        gives_warning('docvarsfrom=nonesuch not supported.')
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


test_that("test textfile encoding parameter", {
   
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

  FILEDIR <- quanteda:::mktemp(directory=T)
  unzip(system.file("extdata", "encodedExampleFiles.zip", package = "quanteda"), exdir = FILEDIR)

  filenames <- list.files(FILEDIR, "*__characters.txt$")
  parts <- strsplit(gsub(".txt$", "", filenames), "__")
  fileencodings <- sapply(parts, "[", 1)

  fileencodings <- fileencodings[!(fileencodings %in% broken_encodings)]
  filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))


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

  #  Test loading all these files at once with different encodings
  #encodedTextfilesCorpus <- corpus(textfile(filenames, encoding=fileencodings))

  # Test UTF-8 encoded file, read as UTF-16: should not work
   expect_warning(
     misread_texts <- texts(textfile(file.path(FILEDIR, 'UTF-8__characters.txt'), encoding='utf-16'))
   )
   utf8_bytes <- data.table::fread(file.path(FILEDIR, 'UTF-8__bytes.tsv'))[[1]]
   expect_false(
          all(as.numeric(charToRaw(misread_texts)) == utf8_bytes)
   )

   # Test ASCII encoded file, read as UTF-8:
   expect_that(
      as.numeric(charToRaw(
          texts(textfile(file.path(FILEDIR, 'UTF-8__characters.txt'), encoding='utf-8'),
      ))),
      equals(utf8_bytes)
   )

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
})

test_that("Test mktemp function for test dirs",{
  filename <- quanteda::mktemp()
  expect_true(file.exists(filename))
  filename2 <- quanteda::mktemp()
  expect_true(file.exists(filename2))
  print(paste(filename, filename2))
  expect_false(filename == filename2)

  # test directory parameter
  dirname <- quanteda::mktemp(directory=T)
  expect_true(dir.exists(dirname))
  
  # test prefix parameter
  filename <- quanteda::mktemp(prefix='testprefix')
  expect_equal(
    substr(basename(filename), 1, 10),
    'testprefix'
  )

})
