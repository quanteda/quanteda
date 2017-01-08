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
    txt <- c(d1 = 
"Paragraph one.  

Second paragraph is this one!  Here is the third sentence.",
             d2 = "Only paragraph of doc2?  

No there is another.")
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    cseg <- corpus_segment(mycorp, "paragraphs")
    expect_equal(as.character(cseg)[2], c(d1.2 = "Second paragraph is this one!  Here is the third sentence."))
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
})

test_that("char_segment tokens works", {
    expect_identical(as.character(tokens(data_char_ukimmig2010)), 
           as.character(char_segment(data_char_ukimmig2010, what = "tokens")))
})

