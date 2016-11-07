library(quanteda)
library(testthat)

context('test ngrams.R')

test_that("test that ngrams produces the results from Guthrie 2006", {
      toks <- tokens('insurgents killed in ongoing fighting')

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
      
      expect_equivalent(
          ngrams(toks, n=2, skip=0),
          list(bi_grams)
      )
      expect_equivalent(
          ngrams(toks, n=2, skip=0:2), 
          list(two_skip_bi_grams)
      )
      # Same function called via `skipgrams`
      expect_equivalent(
          skipgrams(toks, n=2, skip=0:2), 
          list(two_skip_bi_grams)
      )
      expect_equivalent(
          ngrams(toks, n=3, skip=0), 
          list(tri_grams)
      )
      # Same function called via `skipgrams`
      expect_equivalent(
          skipgrams(toks, n=3, skip=0), 
          list(tri_grams)
      )
      expect_equivalent(
          ngrams(toks, n=3, skip=0:2), 
          list(two_skip_tri_grams)
      )
      
      toks2 <- tokens(c('a b c d e', 'c d e f g'))
      expect_equivalent(
        as.list(ngrams(toks2, n = 2:3)),
        list(c("a_b", "b_c", "c_d", "d_e", "a_b_c", "b_c_d", "c_d_e"),
             c("c_d", "d_e", "e_f", "f_g", "c_d_e", "d_e_f", "e_f_g"))
      )

})
