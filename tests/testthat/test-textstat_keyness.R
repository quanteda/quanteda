
# correlation
test_that("test textstat_simil method = \"correlation\" against proxy simil(): documents", {
    period <- as.factor(ifelse(docvars(data_corpus_inaugural)$Year < 1945, 'post-war', 'pre-war'))
    testdfm <- dfm(data_corpus_inaugural, groups = period)
    testdfm <- dfm_remove(testdfm, stopwords('english'))
    testdfm <- dfm_select(testdfm, min_count = 2, min_nchar = 2)
    testdfm <- dfm_smooth(testdfm, smoothing = 1)
    expect_equal(head(names(textstat_keyness(testdfm)), 5),
                 c('states', 'constitution', 'government', 'upon', 'public'))
})
