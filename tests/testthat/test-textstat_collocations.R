context('test textstat_collocations.R')

test_that("test that collocations do not span texts", {
    toks <- tokens(c('this is a test', 'this also a test'))
    cols <- rbind(textstat_collocations(toks, max_size = 2, min_count = 1),
                  textstat_collocations(toks, max_size = 3, min_count = 1))
    
    expect_false('test this' %in% cols$collocation)
    expect_false('test this also' %in% cols$collocation)
    expect_true('this also a' %in% cols$collocation)
})

test_that("test that collocations only include selected features", {
    toks <- tokens(c('This is a Twitter post to @someone on #something.'), what = 'fastest')
    cols <- textstat_collocations(toks, 'lr', features = "^([a-z]+)$", valuetype = 'regex', min_count = 1, max_size = 2)
    
    expect_true('This is' %in% cols$collocation)
    expect_true('a Twitter' %in% cols$collocation)
    
    expect_false('to @someone' %in% cols$collocation)
    expect_false('on #something' %in% cols$collocation)
})

test_that("test that collocations and sequences are counting the same features", {
    toks <- tokens(data_char_inaugural, remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords(), padding = TRUE)
    seqs <- textstat_collocations(toks, method = 'bj', max_size = 2)
    cols <- textstat_collocations(toks, method = 'lr', max_size = 2)
    both <- merge(seqs, cols, by = 'collocation')
    expect_true(all(both$count.x == both$count.x))
})

test_that("test that extractor works with collocation", {
    
    toks <- tokens(data_char_inaugural, remove_punct = TRUE)
    toks <- tokens_remove(toks, stopwords(), padding = TRUE)
    cols <- textstat_collocations(toks, method = 'lr', max_size = 2)
    cols <- cols[1:5,]
    expect_equal(nrow(cols), length(as.tokens(cols)))
    
})    