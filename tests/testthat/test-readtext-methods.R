test_that("docvars methods work", {
    load("../data/corpora/data_readtext_example.rda")
    
    expect_identical(
        docnames(data_readtext_example),
        docnames(data_corpus_inaugural)[1:2]
    )

    expect_identical(
        docvars(data_readtext_example),
        structure(list(Year = c(1789L, 1793L), 
                       President = c("Washington", "Washington"), 
                       FirstName = c("George", "George"), 
                       Party = c("none", "none")), 
                  class = "data.frame", row.names = c(NA, -2L))
    )
    
    expect_identical(
        ndoc(data_readtext_example), 2L
    )
})
