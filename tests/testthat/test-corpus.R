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


test_that("test c.corpus", {
    concat.corpus <- c(data_corpus_inaugural, data_corpus_inaugural, data_corpus_inaugural)

    expected_docvars <-rbind(docvars(data_corpus_inaugural), docvars(data_corpus_inaugural), docvars(data_corpus_inaugural))
    rownames(expected_docvars) <- make.unique(rep(rownames(docvars(data_corpus_inaugural)), 3), sep='')

    expect_that(
        docvars(concat.corpus),
        equals(expected_docvars)
    )

    expect_that(
        docvars(concat.corpus),
        is_a('data.frame')
    )

    
    expected_texts <- c(texts(data_corpus_inaugural), texts(data_corpus_inaugural), texts(data_corpus_inaugural))
    names(expected_texts) <- make.unique(rep(names(texts(data_corpus_inaugural)), 3), sep='')
  
    expect_that(
        texts(concat.corpus),
        equals(expected_texts)
    )

    expect_that(
        texts(concat.corpus),
        is_a('character')
    )


    expect_true(
        grepl('Concatenation by c.corpus', metacorpus(concat.corpus)$source)
    )

})

test_that("test corpus constructors works for kwic", {
    
    kwiccorpus <- corpus(kwic(data_corpus_inaugural, "christmas"))
    expect_that(kwiccorpus, is_a("corpus"))
    expect_equal(sort(names(docvars(kwiccorpus))),
                 c("context", "docname", "from", "keyword", "to"))
})


test_that("test corpus constructors works for character", {

    expect_that(corpus(data_char_ukimmig2010), is_a("corpus"))

})

test_that("test corpus constructors works for data.frame", {
    
    mydf <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
                       some_ints = 1L:6L,
                       some_text = paste0("This is text number ", 1:6, "."),
                       some_logical = rep(c(TRUE, FALSE), 3),
                       stringsAsFactors = FALSE,
                       row.names = paste0("fromDf_", 1:6))
    mycorp <- corpus(mydf, text_field = "some_text", 
                     metacorpus = list(source = "From a data.frame called mydf."))
    expect_equal(docnames(mycorp), 
                 paste("fromDf", 1:6, sep = "_"))
    expect_equal(mycorp[["letter_factor"]][3,1],
                 factor("b", levels = c("a", "b", "c")))
    
    mydf2 <- mydf
    names(mydf2)[3] <- "text"
    expect_equal(corpus(mydf, text_field = "some_text"),
                 corpus(mydf2))
    
    mydf3 <- mydf2
    mydf3$doc_id <- row.names(mydf3)
    row.names(mydf3) <- NULL
    expect_equal(corpus(mydf, text_field = "some_text"),
                 corpus(mydf3))
    
    expect_error(
        corpus(mydf3, docid_field = paste0("d", 1:6)),
        "docid_field must refer to a single column"
    )
    
    # expect_error(
    #     corpus(mydf3, docnames = paste0("d", 1:5)),
    #     "user-supplied docnames must be the same as the number of documents"
    # )
    
    expect_equal(corpus(mydf, text_field = "some_text"),
                 corpus(mydf, text_field = 3))
    
    expect_error(corpus(mydf, text_field = "some_ints"),
                 "text_field must refer to a character mode column")
    expect_error(corpus(mydf, text_field = c(1,3)),
                 "only one text_field may be specified")
    expect_error(corpus(mydf, text_field = c("some_text", "letter_factor")),
                 "only one text_field may be specified")
    expect_error(corpus(mydf, text_field = 3.1),
                 "text_field index refers to an invalid column")
    expect_error(corpus(mydf, text_field = 0),
                 "text_field index refers to an invalid column")
    expect_error(corpus(mydf, text_field = -1),
                 "text_field index refers to an invalid column")
    expect_error(corpus(mydf, text_field = "notfound"),
                 "column name notfound not found")

})


test_that("test corpus constructor works for tm objects", {
    skip_if_not_installed("tm")
    require(tm)
    
    # VCorpus
    data(crude, package = "tm")    # load in a tm example VCorpus
    mytmCorpus <- corpus(crude)
    expect_equal(substring(texts(mytmCorpus)[1], 1, 21),
                 c("reut-00001.xml"  = "Diamond Shamrock Corp"))
    
    data(acq, package = "tm")
    mytmCorpus2 <- corpus(acq)
    expect_equal(dim(docvars(mytmCorpus2)), c(50,15))
    
    # SimpleCorpus
    txt <- system.file("texts", "txt", package = "tm")
    mytmCorpus3 <- SimpleCorpus(DirSource(txt, encoding = "UTF-8"),
                                control = list(language = "lat"))
    qcorpus3 <- corpus(mytmCorpus3)
    expect_equal(content(mytmCorpus3), texts(qcorpus3))
    expect_equal(unclass(meta(mytmCorpus3, type = "corpus")[1]),
                 metacorpus(qcorpus3)[names(meta(mytmCorpus3, type = "corpus"))])
    
    # any other type
    mytmCorpus4 <- mytmCorpus3
    class(mytmCorpus4)[1] <- "OtherCorpus"
    expect_error(
        corpus(mytmCorpus4),
        "Cannot construct a corpus from this tm OtherCorpus object"
    )
    
    detach("package:tm", unload = TRUE)
    detach("package:NLP", unload = TRUE)
})

test_that("test corpus constructor works for VCorpus with one document (#445)", {
    skip_if_not_installed("tm")
    tmCorpus_length1 <- tm::VCorpus(tm::VectorSource(data_corpus_inaugural[1]))
    expect_silent(qcorpus <- corpus(tmCorpus_length1))
    expect_equal(texts(qcorpus)[1], data_corpus_inaugural[1])
})

test_that("corpus_subset works", {
    txt <- c(doc1 = "This is a sample text.\nIt has three lines.\nThe third line.",
             doc2 = "one\ntwo\tpart two\nthree\nfour.",
             doc3 = "A single sentence.",
             doc4 = "A sentence with \"escaped quotes\".")
    dv <- data.frame(varnumeric = 10:13, varfactor = factor(c("A", "B", "A", "B")), varchar = letters[1:4])

    data_corpus_test    <- corpus(txt, docvars = dv, metacorpus = list(source = "From test-corpus.R"))
    expect_equal(ndoc(corpus_subset(data_corpus_test, varfactor == "B")), 2)
    expect_equal(docnames(corpus_subset(data_corpus_test, varfactor == "B")), c("doc2", "doc4"))
    
    data_corpus_test_nodv  <- corpus(txt, metacorpus = list(source = "From test-corpus.R"))
    expect_equal(ndoc(corpus_subset(data_corpus_test_nodv, LETTERS[1:4] == "B")), 1)
    expect_equal(docnames(corpus_subset(data_corpus_test_nodv, LETTERS[1:4] == "B")), c("doc2"))

})
    
test_that("corpus_segment works", {
    txt <- c(doc1 = "This is a sample text.\nIt has three lines.\nThe third line.",
             doc2 = "one\ntwo\tpart two\nthree\nfour.",
             doc3 = "A single sentence.",
             doc4 = "A sentence with \"escaped quotes\".")
    dv <- data.frame(varnumeric = 10:13, varfactor = factor(c("A", "B", "A", "B")), varchar = letters[1:4])
    
    data_corpus_test_nodv  <- corpus(txt, metacorpus = list(source = "From test-corpus.R"))
    expect_equal(ndoc(corpus_segment(data_corpus_test_nodv, "sentences")), 6)
    
    data_corpus_test <- corpus(txt, docvars = dv, metacorpus = list(source = "From test-corpus.R"))
    expect_equal(ndoc(corpus_segment(data_corpus_test, "sentences")), 6)
})

test_that("summary method works for corpus", {
    expect_output(summary(data_corpus_irishbudget2010, verbose = TRUE), regexp = "^Corpus consisting of 14 documents\\.")
})

test_that("corpus works for texts with duplicate filenames", {
    txt <- c(one = "Text one.", two = "text two", one = "second first text")
    cor <- corpus(txt)
    expect_equal(docnames(cor), c("one", "two", "one.1"))
})
