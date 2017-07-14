context('test textstat_collocations.R')

test_that("test that collocations do not span texts", {
    toks <- tokens(c('this is a test', 'this also a test'))
    cols <- rbind(textstat_collocations(toks, size = 2, min_count = 1, method = "lr"),
                  textstat_collocations(toks, size = 3, min_count = 1, method = "lr"))
    
    expect_false('test this' %in% cols$collocation)
    expect_false('test this also' %in% cols$collocation)
    expect_true('this also a' %in% cols$collocation)
})

test_that("test that collocations only include selected features", {
    toks <- tokens(c('This is a Twitter post to @someone on #something.'), what = 'fastest')
    toks <- tokens_select(toks, "^([a-z]+)$", valuetype="regex")
    cols <- textstat_collocations(toks, 'lr', min_count = 1, size = 2)
    
    expect_true('This is' %in% cols$collocation)
    expect_true('a Twitter' %in% cols$collocation)
    
    expect_false('to @someone' %in% cols$collocation)
    expect_false('on #something' %in% cols$collocation)
})

test_that("test that collocations and sequences are counting the same features", {
    toks <- tokens(data_corpus_inaugural, remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords(), padding = TRUE)
    seqs <- textstat_collocations(toks, method = 'lambda1', size = 2)
    cols <- textstat_collocations(toks, method = 'lr', size = 2)  # now is equal to `lambda`
    both <- merge(seqs, cols, by = 'collocation')
    expect_true(all(both$count.x == both$count.y))
})

# test_that("test that extractor works with collocation", {
#     
#     toks <- tokens(data_corpus_inaugural, remove_punct = TRUE)
#     toks <- tokens_remove(toks, stopwords(), padding = TRUE)
#     cols <- textstat_collocations(toks, method = 'lr', size = 2)
#     cols <- cols[1:5,]
#     expect_equal(nrow(cols), length(as.tokens(cols)))
#     
# })

test_that("bigrams and trigrams are all sorted correctly, issue #385", {
    
    toks <- tokens(data_corpus_inaugural, remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords(), padding = TRUE)
    cols <- textstat_collocations(toks, method = 'lr', size = 2:3)
    expect_equal(order(cols$z, decreasing = TRUE), seq_len(nrow(cols)))
    
})

test_that("collocation is counted correctly in racing conditions, issue #381", {

    toks <- tokens(rep(texts(data_corpus_inaugural)[1], 100))
    out1 <- textstat_collocations(toks[1], method = 'chi2', size = 2, min_count = 1)
    out100 <- textstat_collocations(toks, method = 'chi2', size = 2, min_count = 1)
    out1 <- out1[order(out1$collocation),]
    out100 <- out100[order(out100$collocation),]
    expect_true(all(out1$count * 100 == out100$count))

})
