##
## tokenizer tests
##

test_that("tokenizer counts tokens as expected", {
    txt <- "Sentence: with punctation, Capital letters not capital."
    expect_equal(length(tokenize(txt, simplify=TRUE)), 10)
    expect_equal(length(tokenize(txt, simplify=TRUE, removePunct = TRUE)), 7)
    expect_equal(length(unique(tokenize(char_tolower(txt), simplify=TRUE, removePunct = TRUE))), 6)
})

test_that("tokenizer for corpus counts tokens as expected", {
    corp <- corpus("Sentence: with punctation, Capital letters not capital.")
    expect_equal(length(tokenize(corp, simplify = TRUE)), 10)
    expect_equal(length(tokenize(corp, simplify = TRUE, removePunct = TRUE)), 7)
    expect_equal(length(unique(tokenize(char_tolower(texts(corp)), simplify = TRUE, removePunct = TRUE))), 6)
})
