context("test bootstrapping functions")

test_that("bootstrap_dfm works as planned", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
             texttwo = "Premiere phrase.  Deuxieme phrase.")
    mycorpus <- corpus(txt, 
                       docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
                       metacorpus = list(notes = "Example showing how corpus_reshape() works."))
    set.seed(10)
    dfmresamp <- bootstrap_dfm(mycorpus, n = 3, verbose = FALSE)
    expect_identical(dfmresamp[[1]], 
                     dfm(mycorpus))
    
    dfmresamp <- bootstrap_dfm(txt, n = 3, verbose = FALSE)
    expect_identical(dfmresamp[[1]], 
                     dfm(mycorpus))

})
