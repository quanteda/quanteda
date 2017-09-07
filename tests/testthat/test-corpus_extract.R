context("Testing corpus_extracttags")

test_that("corpus_extracttags is working with fixed", {
    
    corp <- corpus(c(d1 = "##TAG One",
                     d2 = "##TAG Two"))
    corp_ext1 <- corpus_extracttags(corp, '##TAG', valuetype = 'fixed', position = 'before')
    expect_equal(texts(corp_ext1), c("d1.1" = "One",
                                     "d2.1" = "Two"))
    expect_equal(docvars(corp_ext1, 'tag'), c('##TAG', '##TAG'))
    
    # corp_ext1 <- corpus_extracttags(corp, pattern = '##TAG', valuetype = 'fixed', remove_pattern = FALSE)
    # expect_equal(texts(corp_ext1), c("d1" = "##TAG One",
    #                                  "d2" = "##TAG Two"))
    # expect_equal(docvars(corp_ext1, 'tag'), c('##TAG', '##TAG'))
    # 
    # corp_ext2 <- corpus_extracttags(corp, pattern = '##TAG', valuetype = 'fixed', remove_pattern = TRUE)
    # expect_equal(texts(corp_ext2), c("d1" = " One",
    #                                  "d2" = " Two"))
    # expect_equal(docvars(corp_ext2, 'tag'), c('##TAG', '##TAG'))
    
})

test_that("corpus_extracttags is working with glob", {
    
    corp <- corpus(c(d1 = "##TEST1 One",
                     d2 = "##TEST2 Two"))
    
    corp_ext1 <- corpus_extracttags(corp, '##*', valuetype = 'glob', position = 'before')
    expect_equal(texts(corp_ext1), c("d1.1" = "One",
                                     "d2.1" = "Two"))
    expect_equal(docvars(corp_ext1, 'tag'), c('##TEST1', '##TEST2'))
    
    
    # corp_ext1 <- corpus_extracttags(corp, pattern = '##*', valuetype = 'glob', remove_pattern = FALSE)
    # expect_equal(texts(corp_ext1), c("d1" = "##TEST1 One",
    #                                  "d2" = "##TEST2 Two"))
    # expect_equal(docvars(corp_ext1, 'tag'), c('##TEST1', '##TEST2'))
    # 
    # corp_ext2 <- corpus_extracttags(corp, pattern = '##*', valuetype = 'glob', remove_pattern = TRUE)
    # expect_equal(texts(corp_ext2), c("d1" = " One",
    #                                  "d2" = " Two"))
    # expect_equal(docvars(corp_ext2, 'tag'), c('##TEST1', '##TEST2'))
    
})

test_that("corpus_extracttags is working with regex", {
    
    corp <- corpus(c(d1 = "##TEST1 One",
                     d2 = "##TEST2 Two"))
    
    corp_ext1 <- corpus_extracttags(corp, '##[A-Z0-9]+', valuetype = 'regex', position = 'before')
    expect_equal(texts(corp_ext1), c("d1.1" = "One",
                                     "d2.1" = "Two"))
    expect_equal(docvars(corp_ext1, 'tag'), c('##TEST1', '##TEST2'))
    
    # corp_ext1 <- corpus_extracttags(corp, pattern = '##[A-Z0-9]+', valuetype = 'regex', remove_pattern = FALSE)
    # expect_equal(texts(corp_ext1), c("d1" = "##TEST1 One",
    #                                  "d2" = "##TEST2 Two"))
    # expect_equal(docvars(corp_ext1, 'tag'), c('##TEST1', '##TEST2'))
    # 
    # corp_ext2 <- corpus_extracttags(corp, pattern = '##[A-Z0-9]+', valuetype = 'regex', remove_pattern = TRUE)
    # expect_equal(texts(corp_ext2), c("d1" = " One",
    #                                  "d2" = " Two"))
    # expect_equal(docvars(corp_ext2, 'tag'), c('##TEST1', '##TEST2'))
    # 
    # corp_ext3 <- corpus_extracttags(corp, pattern = '##[A-Z0-9]+ ', valuetype = 'regex', remove_pattern = TRUE)
    # expect_equal(texts(corp_ext3), c("d1" = "One",
    #                                  "d2" = "Two"))
    # expect_equal(docvars(corp_ext3, 'tag'), c('##TEST1', '##TEST2'))
    
})



