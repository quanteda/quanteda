context('test compressed corpus.R')

txt <- c(doc1 = "This is a sample text.\nIt has three lines.\nThe third line.",
         doc2 = "one\ntwo\tpart two\nthree\nfour.",
         doc3 = "A single sentence with an 😊 emoji.",
         doc4 = "A sentence with \"escaped quotes\".")
dv <- data.frame(varnumeric = 10:13, varfactor = factor(c("A", "B", "A", "B")), varchar = letters[1:4])
data_corpus_test <- corpus(txt, docvars = dv, metacorpus = list(source = "From test-corpuzip.R"))
data_corpuszip_test <- corpuszip(txt, docvars = dv, metacorpus = list(source = "From test-corpuzip.R"))

test_that("print method works for corpuszip", {
    expect_output(print(data_corpus_test), regexp = "^Corpus consisting of 4 documents and 3 docvars\\.$")
    expect_output(print(data_corpuszip_test), regexp = "^Corpus consisting of 4 documents and 3 docvars \\(compressed 61.5%\\)\\.$")
})

test_that("summary method works for corpuszip", {
    expect_output(summary(data_corpus_test), regexp = "^Corpus consisting of 4 documents\\.")
    expect_output(summary(data_corpuszip_test), regexp = "^Corpus consisting of 4 documents \\(compressed 61.5%\\)\\.")
})

test_that("is.corpus methods work", {
    expect_true(is.corpuszip(data_corpuszip_test))
    expect_true(is.corpus(data_corpuszip_test))
    expect_true(!is.corpuszip(data_corpus_test))
    expect_true(is.corpus(data_corpus_test))
})

test_that("+ and c() methods work for corpus and corpuszip", {
    expect_equal(docvars(data_corpuszip_test + data_corpuszip_test),
                 docvars(data_corpus_test + data_corpus_test))
    expect_equal(texts(data_corpuszip_test + data_corpuszip_test),
                 texts(data_corpus_test + data_corpus_test))
    expect_equal(docvars(c(data_corpuszip_test, data_corpuszip_test, data_corpuszip_test)),
                 docvars(c(data_corpus_test, data_corpus_test, data_corpus_test)))
    expect_equal(texts(c(data_corpuszip_test, data_corpuszip_test, data_corpuszip_test)),
                 texts(c(data_corpus_test, data_corpus_test, data_corpus_test)))
})



test_that("old corpus texts() and docvars() are same as new: data_corpus_inaugural", {
    expect_equal(docnames(data_corpus_test), docnames(data_corpuszip_test))
    expect_equal(docvars(data_corpus_test), docvars(data_corpuszip_test))
    expect_equal(texts(data_corpus_test), texts(data_corpuszip_test))
})

test_that("corpuszip: texts and as.character are the same", {
    expect_equal(as.character(data_corpuszip_test),
                 texts(as.character(data_corpuszip_test)))
    expect_equal(as.character(data_corpus_test),
                 texts(as.character(data_corpus_test)))
})

test_that("texts<- works with corpuszip", {
    texts(data_corpus_test)[c(2,4)] <- "REPLACEMENT TEXT."
    expect_equivalent(texts(data_corpus_test)[2], "REPLACEMENT TEXT.")
    expect_equivalent(texts(data_corpus_test)[1], "This is a sample text.\nIt has three lines.\nThe third line.")

    #### FAILS
    # texts(data_corpuszip_test)[c(2,4)] <- "REPLACEMENT TEXT."
    # expect_equivalent(texts(data_corpuszip_test)[2], "REPLACEMENT TEXT.")
    # expect_equivalent(texts(data_corpuszip_test)[1], "This is a sample text.\nIt has three lines.\nThe third line.")
})


test_that("old corpus texts() and docvars() are same as new: data_corpus_inaugural", {
    data_corpuszip_inaugural <- corpuszip(texts(data_corpus_inaugural),
                                          docvars = docvars(data_corpus_inaugural))

    expect_equal(docnames(data_corpus_inaugural), docnames(data_corpuszip_inaugural))
    expect_equal(texts(data_corpus_inaugural), texts(data_corpuszip_inaugural))
    expect_equal(docvars(data_corpus_inaugural), docvars(data_corpuszip_inaugural))
})

test_that("[[ and [ methods are the same for corpus and corpuszip", {
    expect_equal(data_corpus_test[[c("varfactor", "varchar")]],
                 data_corpuszip_test[[c("varfactor", "varchar")]])
    expect_equal(data_corpus_test[c(2,4)],
                 data_corpuszip_test[c(2,4)])

    # assignment using `[[`
    data_corpus_test[["newvar"]] <- 4:1
    data_corpuszip_test[["newvar"]] <- 4:1
    expect_equal(docvars(data_corpus_test),
                 docvars(data_corpuszip_test))

})
