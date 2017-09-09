context("Testing corpus_segment and char_segment")

# test_that("corpus_segment works for sentences", {
#     txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
#                    Here is the third sentence.",
#              d2 = "Only sentence of doc2?  No there is another.")
#     mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
#     cseg <- corpus_segment(mycorp, "sentences")
#     expect_equal(as.character(cseg)[4], c(d2.1 = "Only sentence of doc2?"))
# })
# 
# test_that("corpus_segment works for paragraphs", {
#     txt <- c(d1 = "Paragraph one.\n\nSecond paragraph is this one!  Here is the third sentence.",
#              d2 = "Only paragraph of doc2?\n\nNo there is another.")
#     mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
#     cseg <- corpus_segment(mycorp, "paragraphs")
#     expect_equal(as.character(cseg)[2], c(d1.2 = "Second paragraph is this one!  Here is the third sentence."))
# })

test_that("corpus_segment works when the delimiter is of glob pattern", {
    txt <- c(d1 = 
                 "Paragraph one.  

Second paragraph is this one!  Here is the third sentence.",
             d2 = "Only paragraph of doc2?  

No there is another.")
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    cseg <- corpus_segment(mycorp, "paragraph*")
    expect_equal(as.character(cseg)[2], c(d1.2 = "is this one!  Here is the third sentence."))
})

# test_that("corpus_segment works for tags", {
#     testCorpus <- corpus(c("##INTRO This is the introduction. 
#                        ##DOC1 This is the first document.  
#                            Second sentence in Doc 1.  
#                            ##DOC3 Third document starts here.  
#                            End of third document.",
#                            "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
#     # add a docvar
#     testCorpus[["serialno"]] <- paste0("textSerial", 1:ndoc(testCorpus))
#     testCorpusSeg <- corpus_segment(testCorpus, "tags")
# 
#     expect_equal(
#         docvars(testCorpusSeg, "tag"),
#         c("##INTRO", "##DOC1",  "##DOC3",  "##INTRO", "##NUMBER", "##NUMBER")
#     )
# 
#     expect_equal(
#         as.character(testCorpusSeg)[5],
#         c(text2.2 = "Two starts before")
#     )
#     
#     # old segment.corpus
#     testCorpusSeg <- suppressWarnings(segment(testCorpus, "tags"))
#     expect_equal(
#         as.character(testCorpusSeg)[5],
#         c(text2.2 = "Two starts before")
#     )
# })

test_that("char_segment works with punctuations", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
             Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    cseg <- char_segment(txt, "\\p{P}", valuetype = "regex", 
                         pattern_remove = FALSE, pattern_position = "after")
    expect_equal(cseg[4], c(d2.1 = "Only sentence of doc2?"))
})

# test_that("corpus_segment works for paragraphs", {
#     txt <- c(d1 = 
# "Paragraph one.
# 
# Second paragraph is this one!  Here is the third sentence.",
#              d2 = "Only paragraph of doc2? 
# 
# No there is another.")
#     cseg <- char_segment(txt, "paragraphs")
#     expect_equal(cseg[2], c(d1.2 = "Second paragraph is this one!  Here is the third sentence."))
# })

test_that("char_segment works for tags", {
    txt <- c("##INTRO This is the introduction.
                           ##DOC1 This is the first document.
                           Second sentence in Doc 1.
                           ##DOC3 Third document starts here.
                           End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three.")
    testCharSeg <- char_segment(txt, "##[A-Z0-9]+", valuetype = "regex",
                                pattern_position = "before", pattern_remove = TRUE)
    expect_equal(testCharSeg[5], "Two starts before")
})

test_that("char_segment works for glob customized tags", {
    txt <- c("INTRO: This is the introduction. 
                           DOC1: This is the first document.  
                           Second sentence in Doc 1.  
                           DOC3: Third document starts here.  
                           End of third document.",
             "INTRO: Document NUMBER: Two starts before NUMBER: Three.")
    testCharSeg <- char_segment(txt, "*:", valuetype = "glob", 
                                pattern_position = "before", pattern_remove = TRUE)
    expect_equal(testCharSeg[6], "Three.")
})

test_that("char_segment works for glob customized tags, test 2", {
    txt <- c("[INTRO] This is the introduction. 
                           [DOC1] This is the first document.  
                           Second sentence in Doc 1.  
                           [DOC3] Third document starts here.  
                           End of third document.",
             "[INTRO] Document [NUMBER] Two starts before [NUMBER] Three.")
    testCharSeg <- char_segment(txt, "[*]", valuetype = "glob", 
                                pattern_position = "before", pattern_remove = TRUE)
    expect_equal(testCharSeg[6], "Three.")
})

test_that("corpus_segment works with blank before tag", {
    corp <- corpus(c("\n##INTRO This is the introduction.
                        ##DOC1 This is the first document.  Second sentence in Doc 1.
                           ##DOC3 Third document starts here.  End of third document.",
                           "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
    corp_seg <- corpus_segment(corp, "##[A-Z0-9]+", valuetype = "regex", 
                               pattern_position = "before", pattern_remove = TRUE)
    summ <- summary(corp_seg)
    expect_equal(summ["text1.1", "Tokens"], 5)
})



# test_that("corpus_segment works for end tag, test 2", {
#     testCorpus <- corpus(c("First line\n##INTRO This is the introduction.
#                            ##DOC1 This is the first document.  Second sentence in Doc 1.
#                            ##DOC3 Third document starts here.  End of third document.",
#                            "##INTRO Document ##NUMBER Two starts before ##NUMBER Three. ##END"))
#     testCorpusSeg <- corpus_segment(testCorpus, pattern = "##*")
#     summ <- summary(testCorpusSeg)
#     expect_equal(summ["text2.4", "tag"], "##END")
#     expect_equal(summ["text2.4", "Tokens"], 0)
# })

# test_that("char_segment works with blank before tag", {
#     txt <- c("\n##INTRO This is the introduction.
#                         ##DOC1 This is the first document.  Second sentence in Doc 1.
#                            ##DOC3 Third document starts here.  End of third document.",
#                            "##INTRO Document ##NUMBER Two starts before ##NUMBER Three.")
#     testSeg <- char_segment(txt, "tags")
#     expect_equal(testSeg[6], "Three.")
# })
 
# test_that("char_segment works for end tag", {
#     txt <- c("##INTRO This is the introduction.
#                         ##DOC1 This is the first document.  Second sentence in Doc 1.
#              ##DOC3 Third document starts here.  End of third document.",
#              "##INTRO Document ##NUMBER Two starts before ##NUMBER Three. ##END")
#     testSeg <- char_segment(txt, "tags")
#     expect_equal(testSeg[length(testSeg)], "Three.")
# })

test_that("corpus_segment works with use_docvars TRUE or FALSE", {
    corp <- corpus(c(d1 = "##TEST One two ##TEST2 Three",
                     d2 = "##TEST3 Four"),
                   docvars = data.frame(test = c("A", "B"), stringsAsFactors = FALSE))
    corp_seg1 <- corpus_segment(corp, "##[A-Z0-9]+", valuetype = "regex", 
                           pattern_position = "before", pattern_remove = TRUE, use_docvars = TRUE)
    summ1 <- summary(corp_seg1)
    expect_equal(summ1$test, c("A", "A", "B"))
    
    corp_seg2 <- corpus_segment(corp, "##[A-Z0-9]+", valuetype = "regex", 
                                pattern_position = "before", pattern_remove = TRUE, use_docvars = FALSE)
    summ2 <- summary(corp_seg2)
    expect_equal(names(summ2), c("Text", "Types", "Tokens", "Sentences", "pattern"))
})

test_that("char_segment works with Japanese texts", {
    
    skip_on_os("windows")
    txt <- "日本語の終止符は.ではない。しかし、最近は．が使われることある。"
    expect_equal(char_segment(txt, '\\p{P}', valuetype = "regex", 
                              pattern_remove = FALSE, pattern_position = "after"),
                 c("日本語の終止符は.", "ではない。", "しかし、", "最近は．", "が使われることある。"))
    
    expect_equal(char_segment(txt, '。', valuetype = "fixed", pattern_remove = FALSE,
                              pattern_position = "after"),
                 c("日本語の終止符は.ではない。", "しかし、最近は．が使われることある。"))
})

test_that("corpus_segment works with position argument", {
    
    corp1 <- corpus(c(d1 = "##TEST One two ##TEST2 Three",
                      d2 = "##TEST3 Four"))
    corp1_seg <- corpus_segment(corp1, '##', valuetype = 'fixed', 
                                pattern_position = "before", pattern_remove = FALSE)
    expect_equal(texts(corp1_seg), c("d1.1" = "##TEST One two",
                                     "d1.2" = "##TEST2 Three",
                                     "d2.1" = "##TEST3 Four"))
    
    corp2 <- corpus(c(d1 = "TEST One two; TEST2 Three;",
                      d2 = "TEST3 Four;"))
    corp2_seg <- corpus_segment(corp2, ';', valuetype = 'fixed', 
                                pattern_position = 'after', pattern_remove = FALSE)
    expect_equal(texts(corp2_seg), c("d1.1" = "TEST One two;",
                                     "d1.2" = "TEST2 Three;",
                                     "d2.1" = "TEST3 Four;"))
    
    corp3 <- corpus(c(d1 = "**TEST One two ##TEST2 Three",
                      d2 = "??TEST3 Four"))
    corp3_seg <- corpus_segment(corp3, '[*#?]{2}', valuetype = 'regex', 
                                pattern_position = 'before', pattern_remove = FALSE)
    expect_equal(texts(corp3_seg), c("d1.1" = "**TEST One two",
                                     "d1.2" = "##TEST2 Three",
                                     "d2.1" = "??TEST3 Four"))
    
    corp4 <- corpus(c(d1 = "TEST One two; TEST2 Three?",
                      d2 = "TEST3 Four!"))
    corp4_seg <- corpus_segment(corp4, '[!?;]', valuetype = 'regex', pattern_position = 'after',
                                pattern_remove = FALSE)
    expect_equal(texts(corp4_seg), c("d1.1" = "TEST One two;",
                                     "d1.2" = "TEST2 Three?",
                                     "d2.1" = "TEST3 Four!"))
    
})


test_that("corpus_segment works for delimiter with remove_pattern", {

    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")

    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    mycorp_seg1 <- corpus_segment(mycorp, '[.!?]', valuetype = 'regex',
                                  pattern_position = "after",
                                  pattern_remove = FALSE)

    expect_equal(texts(mycorp_seg1),
                 c(d1.1 = "Sentence one.",
                   d1.2 = "Second sentence is this one!",
                   d1.3 = "Here is the third sentence.",
                   d2.1 = "Only sentence of doc2?",
                   d2.2 = "No there is another."))

    mycorp_seg2 <- corpus_segment(mycorp, pattern = '[.!?]', valuetype = 'regex',
                                  pattern_position = "after",
                                  pattern_remove = TRUE)
    expect_equal(texts(mycorp_seg2),
                 c(d1.1 = "Sentence one",
                   d1.2 = "Second sentence is this one",
                   d1.3 = "Here is the third sentence",
                   d2.1 = "Only sentence of doc2",
                   d2.2 = "No there is another"))
})

test_that("tag extraction works", {
    corp <- corpus(c("#tag1 Some text. #tag2 More text."))
    
    corpseg <- corpus_segment(corp, pattern = "#*", pattern_remove = TRUE)
    expect_equal(
        docvars(corpseg, "pattern"),
        c("#tag1", "#tag2")
    )
    
    corpseg <- corpus_segment(corp, pattern = "#*", pattern_remove = FALSE)
    expect_error(
        docvars(corpseg, "pattern"),
        "field\\(s\\) pattern not found"
    )
})

test_that("corpus_segment works for begining and end tags", {
    testCorpus <- corpus(c(d1 = "##INTRO This is the introduction.
                           ##DOC1 This is the first document.  Second sentence in Doc 1.
                           ##DOC3 Third document starts here.  End of third document.",
                           d2 = "##INTRO Document ##NUMBER Two starts before ##NUMBER Three. ##END"))
    
    # there is no empty document after ##END
    testCorpusSeg <- corpus_segment(testCorpus, "##*", pattern_position = "after")
    expect_equal(docvars(testCorpusSeg, "pattern")[ndoc(testCorpusSeg)], "##NUMBER")
    expect_equal(texts(testCorpusSeg[1]), c(d1.1 = "This is the introduction."))
    # the first document is after ##INTRO
    expect_equal(docvars(testCorpusSeg, "pattern")[1], "##INTRO")
    expect_equal(texts(testCorpusSeg[1]), c(d1.1 = "This is the introduction."))
    
    # there is no empty document after ##INTRO, with pattern_position = "before
    testCorpusSeg <- corpus_segment(testCorpus, "##*", pattern_position = "before")
    expect_equal(docvars(testCorpusSeg, "pattern")[1], "##DOC1")
    expect_equal(texts(testCorpusSeg[1]), c(d2.3 = "This is the introduction."))
    # the last document is before ##END
    expect_equal(docvars(testCorpusSeg, "pattern")[ndoc(testCorpusSeg)] == "##END")
    expect_equal(texts(testCorpusSeg[ndoc(testCorpusSeg)]), c(d2.3 = "Three."))
})
