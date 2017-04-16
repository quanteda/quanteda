context("Testing corpus_segment and char_segment")

test_that("corpus_segment works for sentences", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    cseg <- corpus_segment(mycorp, "sentences")
    expect_equal(as.character(cseg)[4], c(d2.1 = "Only sentence of doc2?"))
})

test_that("corpus_segment works for paragraphs", {
    txt <- c(d1 = "Paragraph one.\n\nSecond paragraph is this one!  Here is the third sentence.",
             d2 = "Only paragraph of doc2?\n\nNo there is another.")
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    cseg <- corpus_segment(mycorp, "paragraphs")
    expect_equal(as.character(cseg)[2], c(d1.2 = "Second paragraph is this one!  Here is the third sentence."))
})

test_that("corpus_segment works when the delimiter is of glob pattern", {
    txt <- c(d1 = 
                 "Paragraph one.  

Second paragraph is this one!  Here is the third sentence.",
             d2 = "Only paragraph of doc2?  

No there is another.")
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    cseg <- corpus_segment(mycorp, "other", delimiter = "paragraph*")
    expect_equal(as.character(cseg)[2], c(d1.2 = "is this one!  Here is the third sentence."))
})

test_that("corpus_segment works for tags", {
    testCorpus <- corpus(c("##INTRO This is the introduction. 
                       ##DOC1 This is the first document.  
                           Second sentence in Doc 1.  
                           ##DOC3 Third document starts here.  
                           End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
    # add a docvar
    testCorpus[["serialno"]] <- paste0("textSerial", 1:ndoc(testCorpus))
    testCorpusSeg <- corpus_segment(testCorpus, "tags")

    expect_equal(
        docvars(testCorpusSeg, "tag"),
        c("##INTRO", "##DOC1",  "##DOC3",  "##INTRO", "##NUMBER", "##NUMBER")
    )

    expect_equal(
        as.character(testCorpusSeg)[5],
        c(text2.2 = "Two starts before")
    )
    
    # old segment.corpus
    testCorpusSeg <- suppressWarnings(segment(testCorpus, "tags"))
    expect_equal(
        as.character(testCorpusSeg)[5],
        c(text2.2 = "Two starts before")
    )
})

test_that("char_segment works for sentences", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
             Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    cseg <- char_segment(txt, "sentences")
    expect_equal(cseg[4], c(d2.1 = "Only sentence of doc2?"))
    expect_equal(unname(char_segment(txt, "sentences"))[4], "Only sentence of doc2?")
})

test_that("corpus_segment works for paragraphs", {
    txt <- c(d1 = 
"Paragraph one.

Second paragraph is this one!  Here is the third sentence.",
             d2 = "Only paragraph of doc2? 

No there is another.")
    cseg <- char_segment(txt, "paragraphs")
    expect_equal(cseg[2], c(d1.2 = "Second paragraph is this one!  Here is the third sentence."))
})

test_that("char_segment works for tags", {
    txt <- c("##INTRO This is the introduction. 
                           ##DOC1 This is the first document.  
                           Second sentence in Doc 1.  
                           ##DOC3 Third document starts here.  
                           End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three.")
    testCharSeg <- char_segment(txt, "tags")
    expect_equal(testCharSeg[5], "Two starts before")
    
    # old segment.character
    testCharSeg <- suppressWarnings(segment(txt, "tags"))
    expect_equal(testCharSeg[5], "Two starts before")
})

test_that("char_segment works for glob customized tags", {
    txt <- c("INTRO: This is the introduction. 
                           DOC1: This is the first document.  
                           Second sentence in Doc 1.  
                           DOC3: Third document starts here.  
                           End of third document.",
             "INTRO: Document NUMBER: Two starts before NUMBER: Three.")
    testCharSeg <- char_segment(txt, "tags", delimiter = "*:", valuetype = "glob")
    expect_equal(testCharSeg[4], "Three.")
})

test_that("char_segment tokens works", {
    expect_identical(as.character(tokens(data_char_ukimmig2010)), 
           as.character(char_segment(data_char_ukimmig2010, what = "tokens")))
})

test_that("corpus_segment works with blank before tag", {
    testCorpus <- corpus(c("\n##INTRO This is the introduction.
                        ##DOC1 This is the first document.  Second sentence in Doc 1.
                           ##DOC3 Third document starts here.  End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
    testCorpusSeg <- corpus_segment(testCorpus, "tags")
    summ <- summary(testCorpusSeg, verbose = FALSE)
    expect_equal(summ["text1.1", "Tokens"], 5)
    expect_equal(summ["text1.1", "tag"], "##INTRO")
})

test_that("corpus_segment works for end tag", {
    testCorpus <- corpus(c(d1 = "##INTRO This is the introduction.
                        ##DOC1 This is the first document.  Second sentence in Doc 1.
                           ##DOC3 Third document starts here.  End of third document.",
                           d2 = "##INTRO Document ##NUMBER Two starts before ##NUMBER Three. ##END"))
    testCorpusSeg <- corpus_segment(testCorpus, "tags")
    summ <- summary(testCorpusSeg, verbose = FALSE)
    expect_equal(summ["d2.4", "tag"], "##END")
    expect_equal(summ["d2.4", "Tokens"], 0)
})

test_that("corpus_segment works for end tag, test 2", {
    testCorpus <- corpus(c("First line\n##INTRO This is the introduction.
                           ##DOC1 This is the first document.  Second sentence in Doc 1.
                           ##DOC3 Third document starts here.  End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three. ##END"))
    testCorpusSeg <- corpus_segment(testCorpus, "tags")
    summ <- summary(testCorpusSeg, verbose = FALSE)
    expect_equal(summ["text2.4", "tag"], "##END")
    expect_equal(summ["text2.4", "Tokens"], 0)
})

test_that("char_segment works with blank before tag", {
    txt <- c("\n##INTRO This is the introduction.
                        ##DOC1 This is the first document.  Second sentence in Doc 1.
                           ##DOC3 Third document starts here.  End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three.")
    testSeg <- char_segment(txt, "tags")
    expect_equal(testSeg[6], "Three.")
})

test_that("char_segment works for end tag", {
    txt <- c("##INTRO This is the introduction.
                        ##DOC1 This is the first document.  Second sentence in Doc 1.
             ##DOC3 Third document starts here.  End of third document.",
             "##INTRO Document ##NUMBER Two starts before ##NUMBER Three. ##END")
    testSeg <- char_segment(txt, "tags")
    expect_equal(testSeg[length(testSeg)], "Three.")
})

test_that("corpus_segment works with use_docvars T or F", {
    corp <- corpus(c(d1 = "##TEST One two ##TEST2 Three",
                     d2 = "##TEST3 Four"),
                   docvars = data.frame(test = c("A", "B"), stringsAsFactors = FALSE))
    summ <- summary(corpus_segment(corp, what = "tags"), verbose = FALSE)
    expect_equal(summ$test, c("A", "A", "B"))
    
    summ <- summary(corpus_segment(corp, what = "tags", use_docvars = FALSE), 
                    verbose = FALSE)
    expect_equal(names(summ), c("Text", "Types", "Tokens", "Sentences", "tag", "docid"))
})

test_that("char_segment works with Japanese texts", {
    
    txt <- "日本語の終止符は.ではない。しかし、最近は．が使われることある。"
    expect_equal(char_segment(txt, what = 'sentences'),
                 c("日本語の終止符は.", "ではない。", "しかし、最近は．", "が使われることある。"))
    
    expect_equal(char_segment(txt, what = 'other', delimiter = '。'),
                 c("日本語の終止符は.ではない", "しかし、最近は．が使われることある"))
})