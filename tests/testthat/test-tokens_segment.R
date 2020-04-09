context("test tokens_segment")

test_that("tokens_segment works for sentences", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")

    corp <- corpus(txt, docvars = data.frame(title = factor(c("doc1", "doc2"))))
    toks <- tokens(corp)
    toks_sent <- tokens_segment(toks, "\\p{Sterm}", valuetype = "regex", pattern_position = "after")

    expect_equal(ndoc(toks_sent), 5)

    expect_equal(as.list(toks_sent)[4],
                 list(d2.1 = c("Only", "sentence", "of", "doc2", "?")))
    expect_equal(docvars(toks_sent, "title"),
                 as.factor(c("doc1", "doc1", "doc1", "doc2", "doc2")))

})


test_that("tokens_segment works for delimiter", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")

    corp <- corpus(txt, docvars = data.frame(title = factor(c("doc1", "doc2"))))
    toks <- tokens(corp)
    toks_sent <- tokens_segment(toks, "[.!?]", valuetype = "regex", pattern_position = "after")

    expect_equal(ndoc(toks_sent), 5)

    expect_equal(as.list(toks_sent)[1],
                 list(d1.1 = c("Sentence", "one", ".")))

    expect_equal(as.list(toks_sent)[4],
                 list(d2.1 = c("Only", "sentence", "of", "doc2", "?")))

    expect_equal(docvars(toks_sent, "title"),
                 as.factor(c("doc1", "doc1", "doc1", "doc2", "doc2")))

})

test_that("tokens_segment works for delimiter with extract_pattern = TRUE", {
    txt <- c(d1 = "Sentence one.  Second sentence is this one!\n
                   Here is the third sentence.",
             d2 = "Only sentence of doc2?  No there is another.")

    corp <- corpus(txt, docvars = data.frame(title = factor(c("doc1", "doc2"))))
    toks <- tokens(corp)
    toks_sent <- tokens_segment(toks, "[.!?]", valuetype = "regex",
                                  extract_pattern = TRUE, pattern_position = "after")

    expect_equal(ndoc(toks_sent), 5)

    expect_equal(as.list(toks_sent)[1],
                 list(d1.1 = c("Sentence", "one")))

    expect_equal(as.list(toks_sent)[4],
                 list(d2.1 = c("Only", "sentence", "of", "doc2")))

    expect_equal(docvars(toks_sent, "title"),
                 as.factor(c("doc1", "doc1", "doc1", "doc2", "doc2")))

})

test_that("tokens_segment includes left-over text", {
    txt <- c("This is the main. this is left-over")
    toks <- tokens(txt)
    toks_seg1 <- tokens_segment(toks, "[.!?]", valuetype = "regex",
                                  extract_pattern = FALSE, pattern_position = "after")

    expect_equal(as.list(toks_seg1)[2],
                 list(text1.2 = c("this", "is", "left-over")))

    toks_seg2 <- tokens_segment(toks, "[.!?]", valuetype = "regex",
                                  extract_pattern = FALSE, pattern_position = "after")

    expect_equal(as.list(toks_seg2)[2],
                 list(text1.2 = c("this", "is", "left-over")))


})

test_that("tokens_segment works when removing punctuation match, remove_delimiter tests", {
    toks1 <- tokens(c("This: is a test", "Another test"))
    toks2 <- tokens(c("This is a test", "Another test."))
    toks3 <- tokens(c("This is a test", "Another test"))

    # extract_pattern = TRUE
    expect_equal(
        as.list(tokens_segment(toks1, "^\\p{P}$", valuetype = "regex",
                               extract_pattern = TRUE, pattern_position = "after")),
        list(text1.1 = "This", text1.2 = c("is", "a", "test"), text2.1 = c("Another", "test"))
    )
    expect_equal(
        as.list(tokens_segment(toks2, "^\\p{P}$", valuetype = "regex",
                               extract_pattern = TRUE, pattern_position = "after")),
        list(text1 = c("This", "is", "a", "test"), text2 = c("Another", "test"))
    )
    expect_equal(
        as.list(tokens_segment(toks3, "^\\p{P}$", valuetype = "regex",
                               extract_pattern = TRUE, pattern_position = "after")),
        list(text1 = c("This", "is", "a", "test"), text2 = c("Another", "test"))
    )

    # extract_pattern = FALSE
    expect_equal(
        as.list(tokens_segment(toks1, "^\\p{P}$", valuetype = "regex",
                               extract_pattern = FALSE, pattern_position = "after")),
        list(text1.1 = c("This", ":"), text1.2 = c("is", "a", "test"), text2.1 = c("Another", "test"))
    )
    expect_equal(
        as.list(tokens_segment(toks2, "^\\p{P}$", valuetype = "regex",
                               extract_pattern = FALSE, pattern_position = "after")),
        list(text1 = c("This", "is", "a", "test"), text2 = c("Another", "test", "."))
    )
    expect_silent(as.list(tokens_segment(toks2, "^\\p{P}$", valuetype = "regex",
                                         extract_pattern = FALSE, pattern_position = "after")))
    expect_equal(
        as.list(tokens_segment(toks3,  "^\\p{P}$", valuetype = "regex",
                               extract_pattern = FALSE, pattern_position = "after")),
        list(text1 = c("This", "is", "a", "test"), text2 = c("Another", "test"))
    )
})


test_that("tokens_segment works with tags", {
    corp <- corpus(c(d1 = "__TEST__ One two __TEST2__ Three",
                     d2 = "__TEST3__ Four"),
                   docvars = data.frame(test = c("A", "B"), stringsAsFactors = FALSE))
    toks <- tokens(corp, what = "word")
    toks_seg1 <- tokens_segment(toks, "__[A-Z0-9]+__", valuetype = "regex",
                                pattern_position = "before", extract_pattern = TRUE, use_docvars = TRUE)
    vars1 <- docvars(toks_seg1)
    expect_equal(vars1$test, c("A", "A", "B"))
    expect_equal(vars1$pattern, c("__TEST__", "__TEST2__", "__TEST3__"))
    expect_equal(as.list(toks_seg1),
                 list(d1.1 = c("One", "two"), d1.2 = "Three", d2.1 = "Four"))

    toks_seg2 <- tokens_segment(toks, "__[A-Z0-9]+__", valuetype = "regex",
                                pattern_position = "before", extract_pattern = FALSE, use_docvars = TRUE)
    vars2 <- docvars(toks_seg2)
    expect_equal(vars2$test, c("A", "A", "B"))
    expect_equal(vars2$pattern, NULL)
    expect_equal(as.list(toks_seg2),
                 list(d1.1 = c("__TEST__", "One", "two"), 
                      d1.2 = c("__TEST2__", "Three"), 
                      d2.1 = c("__TEST3__", "Four")))

    toks_seg3 <- tokens_segment(toks, "__[A-Z0-9]+__", valuetype = "regex",
                                pattern_position = "before", extract_pattern = TRUE, use_docvars = FALSE)
    vars3 <- docvars(toks_seg3)
    expect_equal(vars3$test, NULL)
    expect_equal(vars3$pattern, c("__TEST__", "__TEST2__", "__TEST3__"))
    expect_equal(as.list(toks_seg3),
                 list(d1.1 = c("One", "two"), d1.2 = "Three", d2.1 = "Four"))

    toks_seg4 <- tokens_segment(toks, "__[A-Z0-9]+__", valuetype = "regex",
                                pattern_position = "before", extract_pattern = FALSE, use_docvars = FALSE)
    vars4 <- docvars(toks_seg4)
    expect_equal(vars4$test, NULL)
    expect_equal(vars4$pattern, NULL)
    expect_equal(as.list(toks_seg4),
                 list(d1.1 = c("__TEST__", "One", "two"), 
                      d1.2 = c("__TEST2__", "Three"), 
                      d2.1 = c("__TEST3__", "Four")))
})
