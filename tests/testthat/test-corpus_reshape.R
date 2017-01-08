context("Testing corpus_reshape")

test_that("corpus_reshape works for sentences", {
    mycorpus <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
                         texttwo = "Premiere phrase.  Deuxieme phrase."), 
                       docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
                       metacorpus = list(notes = "Example showing how corpus_reshape() works."))
    mycorpus_reshaped <- corpus_reshape(mycorpus, to = "sentences")
    expect_equal(as.character(mycorpus_reshaped)[4], c(texttwo1 = "Premiere phrase."))
    expect_equal(docvars(mycorpus_reshaped, "country"), factor(c("UK", "UK", "UK", "USA", "USA")))
})

test_that("corpus_reshape works for paragraphs", {
    mycorpus <- corpus(c(d1 = "Paragraph one.  

Second paragraph is this one!  Here is the third sentence.",
                                      d2 = "Only paragraph of doc2?  

No there is another."),
             docvars = data.frame(document = c("one", "two")))
    mycorpus_reshaped <- corpus_reshape(mycorpus, to = "paragraphs")
    
    expect_equal(as.character(mycorpus_reshaped)[4],
                 c(d22 = "No there is another."))
})
