context('test stopwords.R')

test_that("test that there are some stopwords", {

      expect_true( length(stopwords()) > 0 )
      expect_true( length(stopwords('english')) > 0 )
      expect_true( length(stopwords('SMART')) > 0 )
      
})
