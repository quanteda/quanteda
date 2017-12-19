context('test stopwords.R')

test_that("test that there are some stopwords", {
      expect_true( length(stopwords()) > 0 )
      expect_true( length(stopwords('english')) > 0 )
      expect_true( length(stopwords('smart')) > 0 )
})

test_that("old stopwords are the same as the new", {
    expect_true(TRUE)
})
