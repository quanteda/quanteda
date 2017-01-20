
# correlation
test_that("test textstat_simil method = \"correlation\" against proxy simil(): documents", {
    period <- as.factor(ifelse(docvars(data_corpus_inaugural)$Year < 1945, 'post-war', 'pre-war'))
    mydfm <- dfm(data_corpus_inaugural, groups = period)
    mydfm <- dfm_remove(mydfm, stopwords('english'))
    mydfm <- dfm_select(mydfm, min_count = 2, min_nchar = 2)
    mydfm <- dfm_smooth(mydfm, smoothing = 1)
    expect_equal(head(names(textstat_keyness(mydfm)), 5),
                 c('states', 'constitution', 'government', 'upon', 'public'))
})
