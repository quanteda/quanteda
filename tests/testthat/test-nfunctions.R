context("test nfunctions")

test_that("test nsentence", {
    txt <- c(doc1 = "This is Mr. Smith.  He is married to Dr. Jones.",
             doc2 = "Never, before: a colon!  Gimme a break.")
    expect_equal(nsentence(txt), c(doc1 = 2, doc2 = 2))
    expect_equal(nsentence(corpus(txt)), c(doc1 = 2, doc2 = 2))
    expect_equal(
        nsentence(tokens(txt, what = "sentence")),
        c(doc1 = 2, doc2 = 2)
    )
})


    