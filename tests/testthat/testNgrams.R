library(quanteda)
library(testthat)

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

test_that("test that ngrams produces the results from Guthrie 2006", {
      toks <- c('insurgents', 'killed', 'in', 'ongoing', 'fighting')

      
      bi_grams <- c('insurgents_killed', 'killed_in', 'in_ongoing', 
         'ongoing_fighting')
      two_skip_bi_grams <-  c('insurgents_killed', 'insurgents_in', 
         'insurgents_ongoing', 'killed_in', 'killed_ongoing', 'killed_fighting',
         'in_ongoing', 'in_fighting', 'ongoing_fighting')
      tri_grams <- c('insurgents_killed_in', 'killed_in_ongoing', 
         'in_ongoing_fighting')
      two_skip_tri_grams <-  c('insurgents_killed_in', 
          'insurgents_killed_ongoing', 'insurgents_killed_fighting',
          'insurgents_in_ongoing', 'insurgents_in_fighting', 
          'insurgents_ongoing_fighting', 'killed_in_ongoing', 
          'killed_in_fighting', 'killed_ongoing_fighting', 
          'in_ongoing_fighting')

      expect_that(
          setdiff(ngrams(toks, n=2, skip=0), bi_grams),
          equals(character(0))
      )

      expect_that(
          setdiff(ngrams(toks, n=2, skip=0:2), two_skip_bi_grams),
          equals(character(0))
      )
      # Same function called via `skipgrams`
      expect_that(
          setdiff(skipgrams(toks, n=2, skip=0:2), two_skip_bi_grams),
          equals(character(0))
      )


      expect_that(
          setdiff(ngrams(toks, n=3, skip=0), tri_grams),
          equals(character(0))
      )
      # Same function called via `skipgrams`
      expect_that(
          setdiff(skipgrams(toks, n=3, skip=0), tri_grams),
          equals(character(0))
      )


      expect_that(
          setdiff(ngrams(toks, n=3, skip=0:2), two_skip_tri_grams),
          equals(character(0))
      )


})

test_that("test `ngrams` on tokenized texts", {
      testtokenized <- tokenize(c('insurgents killed in ongoing fighting', 'insurgents killed in ongoing fighting'))
      testngrams <- ngrams(testtokenized, 2, 0)
      expectedngrams <- list(
          c('insurgents_killed', 'killed_in', 'in_ongoing', 'ongoing_fighting'),
          c('insurgents_killed', 'killed_in', 'in_ongoing', 'ongoing_fighting')
      )

      expect_that(
          testngrams,
          is_equivalent_to(expectedngrams)
      )
      expect_that(
          testngrams,
          is_a('tokenizedTexts')
      )
      expect_that(
          attributes(testngrams),
          equals(attributes(testtokenized))
      )

      expect_that(
          skipgrams(testtokenized, 2, 0),
          is_equivalent_to(expectedngrams)
      )

      print('testngrams')
      print(testngrams)
      str(testngrams)
      print('skipgrams')
      str(skipgrams(testtokenized, 2, 0))
})
