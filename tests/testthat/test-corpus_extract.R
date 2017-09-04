context("Testing corpus_extract")

test_that("corpus_extract is working", {
    
    corp <- corpus(c(d1 = "##TEST1 One",
                      d2 = "##TEST2 Two"))
    corp_ext1 <- corpus_extract(corp, pattern = '##[A-Z0-9]+', field = 'tag', remove = FALSE)
    expect_equal(texts(corp_ext1), c("d1" = "##TEST1 One",
                                     "d2" = "##TEST2 Two"))
    expect_equal(docvars(corp_ext1, 'tag'), c('##TEST1', '##TEST2'))
    
    corp_ext2 <- corpus_extract(corp, pattern = '##[A-Z0-9]+', field = 'tag', remove = TRUE)
    expect_equal(texts(corp_ext2), c("d1" = " One",
                                     "d2" = " Two"))
    expect_equal(docvars(corp_ext2, 'tag'), c('##TEST1', '##TEST2'))
    
})


