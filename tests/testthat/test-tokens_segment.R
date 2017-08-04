context("tokens_segment works")

test_that("corpus_segment works for sentences", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    
    mycorp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    mytoks <- tokens(mycorp)
    mytoks_sent <- tokens_segment(mytoks, '[.!?]', valuetype = 'regex')
    
    expect_equal(ndoc(mytoks_sent), 5)
    
    expect_equal(as.list(mytoks_sent)[4], 
                 list(d2.1 = c("Only", "sentence", "of", "doc2", "?")))
    
    expect_equal(rownames(docvars(mytoks_sent)),
                 c('d1.1', 'd1.2', 'd1.3', 'd2.1', 'd2.2'))
    expect_equal(docvars(mytoks_sent, 'title'),
                 as.factor(c('doc1', 'doc1', 'doc1', 'doc2', 'doc2')))
    
})