test_that("tokens_ngrams works", {
    toks <- tokens(c('insurgents killed in ongoing fighting'))
    expect_message(
        tokens_ngrams(toks, n = 1:2, verbose = TRUE),
        "tokens_ngrams() changed", fixed = TRUE
    )
})

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
      
      expect_setequal(
          as.list(tokens_ngrams(toks, n=2, skip=0))[[1]],
          bi_grams
      )
      
      expect_setequal(
          as.list(tokens_ngrams(toks, n=2, skip=0:2))[[1]],
          two_skip_bi_grams
      )
      
      expect_setequal(
          as.list(tokens_ngrams(toks, n=3, skip=0))[[1]],
          tri_grams
      )
      
      expect_setequal(
          as.list(tokens_ngrams(toks, n=3, skip=0:2))[[1]],
          two_skip_tri_grams
      )
      
      expect_setequal(
        as.list(tokens_ngrams(toks, n = 2:3))[[1]],
        c(bi_grams, tri_grams)
      )

      expect_setequal(
          as.list(suppressWarnings(tokens_ngrams(toks, n = 2:3)))[[1]],
          c(bi_grams, tri_grams)
      )
})

test_that("char_ngrams works", {
    expect_equivalent(
        char_ngrams(c('insurgents','killed', 'in', 'ongoing', 'fighting')),
        c('insurgents_killed', 'killed_in', 'in_ongoing', 'ongoing_fighting')
    )
    
    expect_warning(char_ngrams(c('insurgents killed', 'in', 'ongoing', 'fighting')), 
                 "whitespace detected: you may need to run tokens\\(\\) first")
})

test_that("token_skipgrams works", {
    toks <- tokens("insurgents killed in ongoing fighting")
    # ASCII concatenator
    ngms1 <- tokens_skipgrams(toks, n = 2, skip = 0:1, concatenator = " ") 
    expect_equivalent(
        as.list(ngms1)[[1]],
        c('insurgents killed', "insurgents in",     "killed in" ,        "killed ongoing" , 
          "in ongoing",        "in fighting",       "ongoing fighting")
    )
    expect_equal(Encoding(types(ngms1)), rep("unknown", 7))
    
    # Unicode concatenator
    ngms2 <- tokens_skipgrams(toks, n = 2, skip = 0:1, concatenator = "\u00a0") 
    expect_equivalent(
      as.list(ngms2)[[1]],
      c('insurgents\u00a0killed', "insurgents\u00a0in",     "killed\u00a0in" ,        "killed\u00a0ongoing" , 
        "in\u00a0ongoing",        "in\u00a0fighting",       "ongoing\u00a0fighting")
    )
    expect_equal(Encoding(types(ngms2)), rep("UTF-8", 7))
})

test_that("raises error for invalid inputs", {
    toks <- tokens("insurgents killed in ongoing fighting")
    expect_error(
        tokens_ngrams(toks, n = 0),
        "The value of n must be between 1 and Inf"
    )
    expect_error(
        tokens_ngrams(toks, skip = -1),
        "The value of skip must be between 0 and Inf"
    )
    expect_error(
        tokens_ngrams(toks, concatenator = character()),
        "The length of concatenator must be 1"
    )
})

test_that("tokens_ngrams does nothing when n = 1 and skip = 0 (#1395)", {
    
    toks <- tokens("insurgents killed in ongoing fighting")
    expect_identical(tokens_ngrams(toks, n = 1, skip = 0, concatenator = " "), toks)
    
})

test_that("test there is no competition between threads", {
    
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    
    # increase the chance to generate the same ngram by duplicating texts
    txt <- char_tolower(rep("Insurgents killed in ongoing fighting.", 10))
    toks <- tokens(txt, remove_punct = TRUE)
    
    ngrs <- rep(list(c("insurgents_killed", "killed_in", "in_ongoing", "ongoing_fighting",
                       "insurgents_killed_in", "killed_in_ongoing", "in_ongoing_fighting")), 10)
    names(ngrs) <- names(toks)
    
    # needs to be repeated because thread competition happen at low chance 
    expect_true(
        all(replicate(1000, identical(as.list(tokens_ngrams(toks, n = 2:3)), ngrs)))
    )
})
    
test_that("tokens_ngrams(x, n = ...) works when ntokens(x) < n", {
    ## issue #392
    expect_equivalent(unclass(as.list(tokens_ngrams(tokens("a"), n = 2)))[[1]],
                      char_ngrams("a", n = 2))
})

