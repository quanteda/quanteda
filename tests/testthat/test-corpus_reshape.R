test_that("corpus_reshape works for sentences", {
    corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
                     texttwo = "Premiere phrase.  Deuxieme phrase.",
                     textthree = ""), 
                   docvars = data.frame(country = factor(c("UK", "US", "ZA")), year=c(1990, 2000, 2020)))
    corp_reshaped <- corpus_reshape(corp, to = "sentences")
    expect_equal(docid(corp_reshaped),
                 factor(c("textone", "textone", "textone", "texttwo", "texttwo", "textthree"),
                        levels = c("textone", "texttwo", "textthree")))
    expect_equal(as.character(corp_reshaped)[4], c(texttwo.1 = "Premiere phrase."))
    expect_equal(as.character(corp_reshaped)[6], c(textthree.1 = ""))
    expect_equal(docvars(corp_reshaped, "country"), 
                 factor(c("UK", "UK", "UK", "US", "US", "ZA"), levels = c("UK", "US", "ZA")))
})

test_that("corpus_reshape works for paragraphs", {
    corp <- corpus(c(d1 = "Paragraph one.  

Second paragraph is this one!  Here is the third sentence.",
                                      d2 = "Only paragraph of doc2?  

No there is another."),
             docvars = data.frame(document = c("one", "two")))
    corp_reshaped <- corpus_reshape(corp, to = "paragraphs")
    
    expect_equal(as.character(corp_reshaped)[4],
                 c(d2.2 = "No there is another."))
})

test_that("corpus_reshape works to sentences and back", {
    corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
                     texttwo = "Premiere phrase.  Deuxieme phrase."), 
                     docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)))
    corp_reshaped <- corpus_reshape(corp, to = "sentences")
    expect_error(
        corpus_reshape(corp_reshaped, to = "paragraphs"),
        "reshape to paragraphs only goes from documents or segments"
    )
    corp_unshaped <- corpus_reshape(corp_reshaped, to = "documents")
    expect_equal(as.character(corp),
                 as.character(corp_unshaped))
    expect_equal(docvars(corp),
                 docvars(corp_unshaped))
})

test_that("corpus_reshape works to paragraphs and back", {
    corp <- corpus(c(textone = "This is a paragraph.\n\nAnother paragraph.\n\nYet paragraph.", 
                     texttwo = "Premiere phrase.\n\nDeuxieme phrase."), 
                     docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)))
    corp_reshaped <- corpus_reshape(corp, to = "paragraphs")
    corp_unshaped <- corpus_reshape(corp_reshaped, to = "documents")
    expect_equal(as.character(corp),
                 as.character(corp_unshaped))
    expect_equal(docvars(corp),
                 docvars(corp_unshaped))
})

test_that("corpus_reshape works with empty documents (#670)", {
    corp <- corpus(c(textone = "This is a paragraph.\n\nAnother paragraph.\n\nYet paragraph.", 
                         texttwo = "Premiere phrase.\n\nDeuxieme phrase.",
                         textthree = ""), 
                       docvars = data.frame(country=c("UK", "USA", "Japan"), year=c(1990, 2000, 2010)))
    corp_reshaped <- corpus_reshape(corp, to = "paragraphs")
    corp_unshaped <- corpus_reshape(corp_reshaped, to = "documents")
    expect_equal(as.character(corp),
                 as.character(corp_unshaped))
    expect_equal(docvars(corp),
                 docvars(corp_unshaped))
})

test_that("corpus_reshape works with segmented corpus", {
    corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
                     texttwo = "Premiere phrase.  Deuxieme phrase."))
    corp_segmented <- corpus_segment(corp, ".", pattern_position = "after")
    corp_reshaped <- corpus_reshape(corp_segmented, to = "sentences")
    corp_reshaped <- corpus_reshape(corp_reshaped, to = "documents")
    expect_equal(as.character(corp_reshaped),
                 c(textone = "This is a sentence  Another sentence  Yet another",
                   texttwo = "Premiere phrase  Deuxieme phrase"))
})

test_that("corpus_reshape preserve empty documents (#1978)", {
    corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
                     texttwo = "Premiere phrase.  Deuxieme phrase.",
                     textthree = ""))
    corp_reshaped <- corpus_reshape(corp, to = "sentences")
    expect_identical(
        docnames(corp),
        docnames(corpus_reshape(corp_reshaped, to = "documents"))
    )
})

test_that("corpus_reshape works with paragraphs (#2468)", {
    corp <- corpus(c(textone = "TITLE\n\nThis is a sentence.  Another sentence.\n\nYet another.", 
                     texttwo = "URGENT\n\nPremiere phrase.  Deuxieme phrase."))
    
    expect_equivalent(
        corpus_reshape(corp, to = "sentence"),
        corpus(
            c(textone.1 = "TITLE", 
              textone.2 = "This is a sentence.", 
              textone.3 = "Another sentence.",  
              textone.4 = "Yet another.",
              texttwo.1 = "URGENT", 
              texttwo.2 = "Premiere phrase.",  
              texttwo.3 = "Deuxieme phrase.")
        )
    )
    
    expect_equivalent(
        corpus_reshape(corp, to = "paragraph"),
        corpus(
            c(textone.1 = "TITLE", 
              textone.2 = "This is a sentence.  Another sentence.",  textone.3 = "Yet another.", 
              texttwo.1 = "URGENT", 
              texttwo.2 = "Premiere phrase.  Deuxieme phrase." )
        )
    )
})

