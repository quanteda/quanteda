##
## tokenizer tests
##

require(quanteda)

test_that("tokenizer counts tokens as expected", {
    txt <- "Sentence: with punctation, Capital letters not capital."
    expect_equal(length(tokenize(txt, simplify=TRUE)), 10)
    expect_equal(length(tokenize(txt, simplify=TRUE, removePunct = TRUE)), 7)
    expect_equal(length(unique(tokenize(toLower(txt), simplify=TRUE, removePunct = TRUE))), 6)
})

test_that("tokenizer for corpus counts tokens as expected", {
    txt <- corpus("Sentence: with punctation, Capital letters not capital.")
    expect_equal(length(tokenize(txt, simplify=TRUE)), 10)
    expect_equal(length(tokenize(txt, simplify=TRUE, removePunct = TRUE)), 7)
    expect_equal(length(unique(tokenize(toLower(txt), simplify=TRUE, removePunct = TRUE))), 6)
})
