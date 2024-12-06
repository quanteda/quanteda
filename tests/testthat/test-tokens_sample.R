corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                 two = "First sentence, doc2.  Second sentence, doc2."))
corp_sent <- corpus_reshape(corp, to = "sentences")
toks <- tokens(corp)
toks_sent <- tokens(corp_sent)
    
test_that("test tokens_sample works", {
    
    docnames(tokens_sample(toks_sent, 5, replace = FALSE))
    expect_true(
       setequal(
            docnames(tokens_sample(toks_sent, 5, replace = FALSE)),
            c("one.2", "one.3", "two.2", "two.1", "one.1")
       )
    )
    
    expect_error(
        tokens_sample(toks_sent, 10, replace = FALSE),
        "size cannot exceed the number of items when replace = FALSE", fixed = TRUE
    )
    
    expect_silent(
        tokens_sample(toks_sent, 10, replace = TRUE)
    )
    
    expect_message(
        tokens_sample(toks_sent, replace = TRUE, verbose = TRUE),
        "tokens_sample() changed", fixed = TRUE
    )
})

test_that("test tokens_sample to see if with grouping, documents can be oversampled", {
    for (i in 1:10) {
        toks_samp <- tokens_sample(toks_sent, replace = TRUE, by = docid(toks_sent))
        expect_equal(
            sum(stringi::stri_detect_regex(docnames(tokens_sample(toks_samp, replace = TRUE, by = docid_)), "^one")),
            3
        )
    }
})

test_that("tokens_sample by group works", {
    toks <- tokens(corpus(
        paste("Document number", seq_len(10)),
        docvars = data.frame(id = paste0("id", seq_len(10)),
                             grp = c(rep("A", 5), rep("B", 5)), 
                             stringsAsFactors = FALSE)
    ))
    expect_equal(
        docvars(tokens_sample(toks, size = 2, by = grp), "grp"),
        rep(LETTERS[1:2], each = 2)
    )
    expect_equal(
        docvars(tokens_sample(toks, size = 25, by = grp, replace = TRUE), "grp"),
        rep(LETTERS[1:2], each = 25)
    )
    expect_equal(
        docvars(tokens_sample(toks, by = grp), "grp"),
        rep(LETTERS[1:2], each = 5)
    )
})

test_that("tokens_sample works with one docvar", {
    toks <- tokens(corpus(LETTERS[1:5], 
                   docvars = data.frame(int = 1:5)))
    expect_true(
        setequal(docvars(tokens_sample(toks), "int"), 1:5)
    )
    expect_true(
        all(docvars(tokens_sample(toks, 4, replace = TRUE), "int") %in% 1:5),
    )
})
