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

test_that("test ntype with dfm (#748)", {
    d <- dfm(c(doc1 = "one two three",
               doc2 = "one one one"))
    expect_equal(
        ntype(d),
        c(doc1 = 3, doc2 = 1)
    )
    expect_equal(
        ntoken(d),
        c(doc1 = 3, doc2 = 3)
    )
})
