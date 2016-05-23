context('Loading a corpus from a zip file.')

tf <- textfile('data/inauguralTopLevel.zip')
qc <- corpus(tf)
test_that("A single-level zip file containing txt files can be loaded",{expect_equal(ndoc(qc), 57)})
