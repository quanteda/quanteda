context("test attributes")

test_that("unit attributes are set correctly", {
    
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")
    corp <- corpus(txt, docvars = data.frame(title = c("doc1", "doc2")))
    
    corp_para <- corpus_reshape(corp, "paragraphs")
    expect_equal(attr(corp_para, "meta")$object$unit, "paragraphs")
    
    corp_sent <- corpus_reshape(corp, "sentences")
    expect_equal(attr(corp_sent, "meta")$object$unit, "sentences")
    
    corp_doc <- corpus_reshape(corp_sent, "documents")
    expect_equal(attr(corp_doc, "meta")$object$unit, "documents")
    
    corp_seg <- corpus_segment(corp, "\\p{Sterm}", valuetype = "regex")
    expect_equal(attr(corp_seg, "meta")$object$unit, "segments")
    
    toks <- tokens(corp)
    toks_seg <- tokens_segment(toks, "\\p{Sterm}", valuetype = "regex")
    expect_equal(attr(toks_seg, "meta")$object$unit, "segments")
    
    toks_chunk <- tokens_chunk(toks, 2)
    expect_equal(attr(toks_chunk, "meta")$object$unit, "segments")
    
    expect_equal(dfm(corp_sent)@meta$object$unit, "sentences")
    expect_equal(dfm(corp_para)@meta$object$unit, "paragraphs")
    expect_equal(dfm(toks_chunk)@meta$object$unit, "segments")
    expect_equal(fcm(dfm(toks_chunk))@meta$object$unit, "segments")
    expect_equal(fcm(toks_chunk)@meta$object$unit, "segments")
    
})

