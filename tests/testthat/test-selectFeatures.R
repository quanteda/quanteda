context('selectFeatures.R')

row_in_df <- function(df, row) {
    nrow(merge(df, row)) > 0
}

test_that("test that selectFeatures.collocations", {
    myCollocs <- collocations(data_char_inaugural[1:3], n=20)
    selected <- data.frame(selectFeatures(myCollocs, stopwords("english"), "remove"))
    
    expect_false(
        row_in_df(
            selected,
            data.frame(word1='if', word2='an')
        )
    )
    
    expect_true(
        row_in_df(
            selected,
            data.frame(word1='american', word2='people')
        )
    )
    
    selected <- data.frame(removeFeatures(myCollocs, stopwords("english")))
    expect_true(
        row_in_df(
            selected,
            data.frame(word1='american', word2='people')
        )
    )
})

test_that("case_insensitive works as expected", {
    myCollocs <- collocations(data_char_inaugural[1:3], n=20, tolower = F)
    selected <- data.frame(selectFeatures(myCollocs, stopwords("english"), case_insensitive = F, "remove"))
    
    expect_false(
        row_in_df(
            selected,
            data.frame(word1='if', word2='an')
        )
    )
    
    expect_equal(nrow(merge(selected,
            data.frame(word1=c('American', 'United'), word2=c('people', 'States')) )), 2)        
     
})

test_that("test that selectFeatures.collocations give expected message when verbose is True", {
    myCollocs <- collocations(data_char_inaugural[1:3], n=20)
    expect_message(selectFeatures(myCollocs, stopwords("english"), "remove", verbose = T),
                   "Removed 15 \\(75%\\) of 20 collocations containing one of 174 stopwords.")
})

test_that("test that selectFeatures.collocations give expected error messages", {
    myCollocs <- collocations(data_char_inaugural[1:3], n=20)
    expect_error(selectFeatures(myCollocs, stopwords("english"), "keep"),
                   "keep not currently supported for selectFeatures.collocations")
    
    expect_error(selectFeatures(myCollocs, stopwords("english"), valuetype = "regex"),
                 "regex not currently supported for selectFeatures.collocations")
    
    expect_error(selectFeatures(myCollocs, stopwords("english"), "remove", pos = 4),
                 "pos for collocation position can only be 1, 2, and/or 3")
    
    expect_error(selectFeatures(myCollocs, c("*is*", "?and"), "remove"),
                 "glob not currently supported for selectFeatures.collocations")
    
    expect_error(removeFeatures(myCollocs, stopwords("english"), selection="keep"),
                 "cannot override selection argument in removeFeatures")
})
