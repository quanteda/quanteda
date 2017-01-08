library(quanteda)
library(testthat)

context('test ngrams.R')

test_that("test that ngrams produces the results from Guthrie 2006", {
      toks <- tokens(c('insurgents killed in ongoing fighting'))

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
      
      expect_equivalent(setdiff(
          as.list(tokens_ngrams(toks, n=2, skip=0))[[1]],
          bi_grams
          ), character(0)
      )
      
      expect_equivalent(setdiff(
          as.list(tokens_ngrams(toks, n=2, skip=0:2))[[1]],
          two_skip_bi_grams
          ), character(0)
      )
      
      expect_equivalent(setdiff(
          as.list(tokens_ngrams(toks, n=3, skip=0))[[1]],
          tri_grams
          ), character(0)
      )
      
      expect_equivalent(setdiff(
          as.list(tokens_ngrams(toks, n=3, skip=0:2))[[1]],
          two_skip_tri_grams
          ), character(0)
      )
      
      expect_equivalent(setdiff(
        as.list(tokens_ngrams(toks, n = 2:3))[[1]],
        c(bi_grams, tri_grams)
        ), character(0)
      )

})

test_that("test `ngrams` on tokenized texts", {
      toks <- tokens(c('insurgents killed in ongoing fighting', 'insurgents killed in ongoing fighting'))
      ngms <- tokens_ngrams(toks, 2, 0)
      ngms_true <- list(
          c('insurgents_killed', 'killed_in', 'in_ongoing', 'ongoing_fighting'),
          c('insurgents_killed', 'killed_in', 'in_ongoing', 'ongoing_fighting')
      )
        
      expect_that(
          ngms,
          is_a('tokenizedTexts')
      )
      
      expect_equivalent(
          as.list(ngms),
          ngms_true
      )

})

# FAILLING
# test_that("tokens_ngrams(x, n = ...) works when ntokens(x) < n", {
#     ## issue #392
#     expect_equivalent(unclass(tokens_ngrams(tokens("a"), n = 2))[[1]],
#                       char_ngrams("a", n = 2))
# })

