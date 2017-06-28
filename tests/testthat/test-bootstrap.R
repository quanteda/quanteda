context("test bootstrapping functions")

test_that("bootstrap_dfm works as planned", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
             texttwo = "Premiere phrase.  Deuxieme phrase.")
    mycorpus <- corpus(txt, 
                       docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
                       metacorpus = list(notes = "Example showing how corpus_reshape() works."))
    set.seed(10)
    dfmresamp1 <- bootstrap_dfm(mycorpus, n = 3, verbose = FALSE)
    expect_identical(dfmresamp1[[1]], 
                     dfm(mycorpus))
    
    dfmresamp2 <- bootstrap_dfm(txt, n = 3, verbose = FALSE)
    expect_identical(dfmresamp2[[1]], 
                     dfm(mycorpus, include_docvars = FALSE))

    
    # are feature names of resamples identical?
    expect_identical(
        featnames(dfmresamp2[[1]]),
        featnames(dfmresamp2[[2]])
    )

})

test_that("bootstrap_dfm works as planned with dfm", {
    txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.", 
             texttwo = "Premiere phrase.  Deuxieme phrase.")
    mycorpus <- corpus(txt, 
                       docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
                       metacorpus = list(notes = "Example showing how corpus_reshape() works."))
    mydfm <- dfm(corpus_reshape(mycorpus, to = "sentences"))
    
    set.seed(10)
    dfmresamp1 <- bootstrap_dfm(mydfm, n = 3, verbose = FALSE)
    expect_identical(dfmresamp1[[1]], 
                     dfm(mycorpus))
    
    dfmresamp2 <- bootstrap_dfm(txt, n = 3, verbose = FALSE)
    expect_identical(dfmresamp2[[1]], 
                     dfm(mycorpus, include_docvars = FALSE))
    
    
    # are feature names of resamples identical?
    expect_identical(
        featnames(dfmresamp2[[1]]),
        featnames(dfmresamp2[[2]])
    )
    
})
