context("tokens_segment works")

test_that("tokens_segment works for sentences", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    mytoks <- tokens(mycorp)
    mytoks_sent <- tokens_segment(mytoks, "sentences")
    
    expect_equal(ndoc(mytoks_sent), 5)
    
    expect_equal(as.list(mytoks_sent)[4], 
                 list(d2.1 = c("Only", "sentence", "of", "doc2", "?")))
    
    expect_equal(rownames(docvars(mytoks_sent)),
                 c('d1.1', 'd1.2', 'd1.3', 'd2.1', 'd2.2'))
    expect_equal(docvars(mytoks_sent, 'title'),
                 as.factor(c('doc1', 'doc1', 'doc1', 'doc2', 'doc2')))
    
    expect_warning(tokens_segment(mytoks, "sentences", delimiter = "."),
                   "delimiter is only used for 'other'")
    
})


test_that("tokens_segment works for delimiter", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    mytoks <- tokens(mycorp)
    mytoks_sent <- tokens_segment(mytoks, what = "other", delimiter = '[.!?]', valuetype = 'regex')
    
    expect_equal(ndoc(mytoks_sent), 5)
    
    expect_equal(as.list(mytoks_sent)[1], 
                 list(d1.1 = c("Sentence", "one", '.')))
    
    expect_equal(as.list(mytoks_sent)[4], 
                 list(d2.1 = c("Only", "sentence", "of", "doc2", "?")))
    
    expect_equal(rownames(docvars(mytoks_sent)),
                 c('d1.1', 'd1.2', 'd1.3', 'd2.1', 'd2.2'))
    expect_equal(docvars(mytoks_sent, 'title'),
                 as.factor(c('doc1', 'doc1', 'doc1', 'doc2', 'doc2')))
    
})

test_that("tokens_segment works for delimiter with remove_delimiter = TRUE", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    mytoks <- tokens(mycorp)
    mytoks_sent <- tokens_segment(mytoks, what = "other", delimiter = '[.!?]', valuetype = 'regex',
                                  remove_delimiter = TRUE)
    
    expect_equal(ndoc(mytoks_sent), 5)
    
    expect_equal(as.list(mytoks_sent)[1], 
                 list(d1.1 = c("Sentence", "one")))
    
    expect_equal(as.list(mytoks_sent)[4], 
                 list(d2.1 = c("Only", "sentence", "of", "doc2")))
    
    expect_equal(rownames(docvars(mytoks_sent)),
                 c('d1.1', 'd1.2', 'd1.3', 'd2.1', 'd2.2'))
    expect_equal(docvars(mytoks_sent, 'title'),
                 as.factor(c('doc1', 'doc1', 'doc1', 'doc2', 'doc2')))
    
})

test_that("tokens_segment includes left-over text", {
    txt <- c("This is the main. this is left-over")
    mytoks <- tokens(txt)
    mytoks_seg1 <- tokens_segment(mytoks, what = "other", delimiter = '[.!?]', valuetype = 'regex',
                                  remove_delimiter = FALSE)
    
    expect_equal(as.list(mytoks_seg1)[2], 
                 list(text1.2 = c("this", "is", "left-over")))
    
    mytoks_seg2 <- tokens_segment(mytoks, what = "other", delimiter = '[.!?]', valuetype = 'regex',
                                  remove_delimiter = TRUE)
    
    expect_equal(as.list(mytoks_seg2)[2], 
                 list(text1.2 = c("this", "is", "left-over")))
    
    
})
