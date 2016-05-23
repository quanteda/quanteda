context('Loading a corpus from a zip file.')

test_that("A single-level zip file containing txt files can be loaded",{
    tf <- textfile('data/inauguralTopLevel.zip')
    qc <- corpus(tf)
    expect_equal(ndoc(qc), 57)
})
