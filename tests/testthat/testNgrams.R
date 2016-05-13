context('test ngrams.R')

test_that("test that ngrams warns that skip!=0 with n=1 does not skip", {
      toks <- c('The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog')
      expect_that(
          unskipped_ngrams <- ngrams(toks, n=1, skip=42),
          gives_warning('skip argument ignored for n = 1')
      )
      expect_identical(
          unskipped_ngrams,
          ngrams(toks, n=1, skip=0)
      )
})
