# ########## TESTS BELOW FAIL ON KEN'S OS X SYSTEM ################
# 
# Sys.setenv("R_TESTS" = "tests/testthat")
# 
# test_that("test globbed tar file",{
#     expect_equal(
#         sort(unname(texts(textfile('../data/tar/*')))),
#         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
#     )
# })
# 
# test_that("test json files", {
#     expect_equal(
#         sort(unname(texts(textfile('../data/json/*json', textField='text')))),
#         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'Now is the winter', 'The quick')
#     )
#     
#     #  test.json and test2.json are newline-delimited json
#     #  test3.json is a single json object
#     expected_docvars <- data.frame(list(
#         colour=c('green', 'red', 'orange', 'blue', NA), 
#         number=c(42, 99, 0, NA, 3)),
#         stringsAsFactors=F)
#     expected_docvars <- expected_docvars[order(expected_docvars$number),]
#     row.names(expected_docvars) <- NULL
#     actual_docvars <- docvars(textfile('../data/json/*json', textField='text'))
#     actual_docvars <- actual_docvars[order(actual_docvars$number),]
#     row.names(actual_docvars) <- NULL
#     row.names(actual_docvars)
#     row.names(expected_docvars)
#     expect_equal(
#         actual_docvars,
#         expected_docvars
#     )
#     
#     expect_that(
#         texts(textfile('../data/json/*json', textField=1)),
#         throws_error('Cannot use numeric textField with json file')
#     )
#     
#     expect_that(
#         texts(textfile('../data/json/test3.json', textField='nonesuch')),
#         throws_error('There is no field called nonesuch in file')
#     )
#     
#     
#     # Twitter json files
#     tweetSource <- textfile('../data/tweets/stream.json')
#     
#     expect_equal(
#         texts(tweetSource),
#         c(stream.json="I jumped over the lazy @dog", stream.json="Yawn")
#     )
#     
#     expect_equal(
#         docvars(tweetSource)$statuses_count,
#         c(16204, 200)
#     )
#     
#     expect_equal(
#         docvars(tweetSource)$screen_name,
#         c('foxxy', 'dog')
#     )
#     
#     
# })
# 
# test_that("test encoding handling (skipped on travis and CRAN", {
#     skip_on_cran()
#     skip_on_travis()
#     
#     # Currently, these encodings don't work for reasons that seem unrelated 
#     # to quanteda, and are either a problem in base R or on travis-ci
#     broken_encodings <- c(
#         "437", "850", "852", "855", "857", "860", "861", "862", "863", "865", 
#         "869", "BIG5-HKSCS", "CHINESE", "CP1251", "CP1255", "CP1256", "CP1361",
#         "CP154", "CP737", "CP858", "CP864", "CP856", "CP932", "CP950", "EUC-JISX0213", 
#         "EUC-JP", "EUC-KR", "GB18030", "HEBREW", "HZ","ISO-2022-JP-1", "ISO-2022-JP-2", 
#         "ISO-2022-JP-3", "ISO-8859-11", "ISO-IR-166", "KOI8-R",
#         "UNICODE-1-1-UTF-7",
#         "MACCENTRALEUROPE", "MACCYRILLIC", "MACGREEK", "MACICELAND", "MACTURKISH",
#         "MS_KANJI", "SHIFT_JISX0213"
#     )
#     
#     
#     FILEDIR <- '../data/encoding'
#     
#     filenames <- list.files(FILEDIR, "*__characters.txt$")
#     parts <- strsplit(gsub(".txt$", "", filenames), "__")
#     fileencodings <- sapply(parts, "[", 1)
#     
#     fileencodings <- fileencodings[!(fileencodings %in% broken_encodings)]
#     filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))
#     
#     for (i in 1:length(fileencodings)) {
#         filename <- filenames[[i]]
#         encoding <- fileencodings[[i]]
#         
#         test_that(paste("test textfile encoding parameter, encoding", encoding), {
#             characters <- as.numeric(charToRaw(
#                 texts(textfile(filename, encoding=fileencodings[[i]]))
#             ))
#             bytes <- data.table::fread(gsub('__characters.txt', '__bytes.tsv', filename))[[1]]
#             expect_equal(characters, bytes)
#         })
#     }
#     test_that("Test loading all these files at once with different encodings", {
#         encodedTextfilesCorpus <- corpus(textfile(filenames, encoding=fileencodings))
#     })
# })
# 
# test_that("test textfile encoding parameter: ASCII encoded file, read as UTF-8: (should work)", {
#     skip_on_cran()
#     skip_on_travis()
#     utf8_bytes <- data.table::fread(file.path(FILEDIR, 'UTF-8__bytes.tsv'))[[1]]
#     expect_that(
#         as.numeric(charToRaw(
#             texts(textfile(file.path(FILEDIR, 'UTF-8__characters.txt'), encoding='utf-8'),
#             ))),
#         equals(utf8_bytes)
#     )
# })
# 
# context('Loading a corpus from a tar archive')
# test_that("A single-level tar file containing txt files can be loaded",{
#     expect_equal(
#         unname(sort(texts(corpus(textfile('../data/tar/test.tar'))))),
#         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
#     )
# })
# 
# context('Loading a corpus from a gzipped tar archive')
# test_that("A single-level tar.gz file containing txt files can be loaded",{
#     expect_equal(
#         sort(unname(texts(corpus(textfile('../data/targz/test.tar.gz'))))),
#         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
#     )
# })
# 
# context('Loading a corpus from a bzipped tar archive')
# test_that("A single-level tar.bz file containing txt files can be loaded",{
#     skip_on_os("windows")
#     expect_equal(
#         sort(unname(texts(corpus(textfile('../data/tarbz/test.tar.bz'))))),
#         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
#     )
# })
