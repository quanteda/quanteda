context("test textstat_frequency")

test_that("test textstat_frequency without groups", {
    dfm1 <- dfm(c("a a b b c d", "a d d d", "a a a"))
    expect_equivalent(
        textstat_frequency(dfm1, ties_method = "random"),
        data.frame(feature = c("a", "d", "b", "c"),
                   frequency = c(6,4,2,1),
                   rank = 1:4,
                   docfreq = c(3,2,1,1), 
                   group = rep('all', 4),
                   stringsAsFactors = FALSE)
    )
    expect_equivalent(
      textstat_frequency(dfm1, n = 2, ties_method = "random"),
      data.frame(feature = c("a", "d", "b", "c"),
                 frequency = c(6,4,2,1),
                 rank = 1:4,
                 docfreq = c(3,2,1,1), 
                 group = rep('all', 4),
                 stringsAsFactors = FALSE)[1:2, ]
    )
})

test_that("test textstat_frequency without groups", {
    txt <- c("a a b b c d", "a d d d", "a a a")
    grp1 <- c("one", "two", "one")
    corp1 <- corpus(txt, docvars = data.frame(grp2 = grp1))
    
    expect_identical(
        textstat_frequency(dfm(corp1), groups = grp1, ties_method = "dense"),
        textstat_frequency(dfm(corp1), groups = "grp2", ties_method = "dense")
    )

    set.seed(10)
    expect_equivalent(
        textstat_frequency(dfm(corp1), groups = grp1, ties_method = "random"),
        data.frame(feature = c("a", "b", "c", "d", "d", "a"),
                   frequency = c(5,2,1,1,3,1),
                   rank = c(1:4, 1:2),
                   docfreq = c(2,1,1,1,1,1),
                   group = c("one", "one", "one", "one", "two", "two"),
                   stringsAsFactors = FALSE)
    )
    
    expect_equivalent(
      textstat_frequency(dfm(corp1), groups = grp1, n = 2, ties_method = "random"),
      data.frame(feature = c("a", "b", "d", "a"),
                 frequency = c(5, 2, 3, 1),
                 rank = c(1:2, 1:2),
                 docfreq = c(2, 1, 1, 1),
                 group = c("one", "one", "two", "two"),
                 stringsAsFactors = FALSE)
    )
    
})

test_that("test textstat_frequency works with weights", {
    txt <- c("a a b b c d", "a d d d", "a a a")
    grp1 <- c("one", "two", "one")
    corp1 <- corpus(txt, docvars = data.frame(grp2 = grp1))
    
    dfm1 <- dfm(corp1)
    dfm1weighted <- dfm_weight(dfm1, "prop")
    
    set.seed(10)
    expect_equivalent(
        textstat_frequency(dfm1weighted, ties_method = "random"),
        data.frame(feature = c("a", "d", "b", "c"),
                   frequency = c(1.58, .916, .333, .1666),
                   rank = 1:4,
                   docfreq = c(3,2,1,1), 
                   group = rep('all', 4),
                   stringsAsFactors = FALSE),
        tolerance = .01
    )
})

test_that("raises error when dfm is empty (#1419)", {
    mx <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_error(textstat_frequency(mx),
                 quanteda:::message_error("dfm_empty"))
})

test_that("test textstat_frequency ties methods defaults work (min)", {
    txt <- c("a a b b c d", "b b b d d d", "a a a")
    dfmat <- dfm(txt)
    expect_equivalent(
        textstat_frequency(dfmat)[, c("feature", "rank")],
        data.frame(feature = c("a", "b", "d", "c"),
                   frequency = c(1, 1, 3, 4),
                   stringsAsFactors = FALSE)
    )
})

test_that("test textstat_frequency with groups and weighted dfm (#1646)", {
    dfmat <- dfm(c("a a b b c d", "a d d d", "a a a")) %>%
        dfm_tfidf()
    
    expect_error(
        textstat_frequency(dfmat, groups = c(1, 2, 2)),
        "will not group a weighted dfm; use force = TRUE to override",
        fixed = TRUE
    )
    expect_equivalent(
        textstat_frequency(dfmat, groups = c(1, 2, 2), force = TRUE),
        data.frame(feature = c("b", "c", "d", "a", "d", "a"),
                   frequency = c(.95, .48, .18, 0, .53, .00),
                   rank = c(1, 2, 3, 4, 1, 2),
                   docfreq = c(1, 1, 1, 0, 1, 0),
                   group = as.character(c(rep(1, 4), 2, 2)),
                   stringsAsFactors = FALSE),
        tolerance = .01
    )
})

test_that("textstat_frequency does not return NAs when n > nfeat", {
  dfmat <- dfm(c("a a b c d", "a d d e", "a b b"))
  
  # should not have NA
  expect_identical(nrow(textstat_frequency(dfmat, n = 6)), 5L)
  
  # should not have NA
  expect_identical(nrow(textstat_frequency(dfmat, n = 6, groups = c(1, 2, 2))), 8L)
})
