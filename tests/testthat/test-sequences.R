context('test sequences.R')

toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)

test_that("test that ordered argument is working", {
    
    seqs <- sequences(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE,
                      nested = TRUE, ordered = FALSE)
    expect_equal(seqs$sequence[1:4],
                c('United States', 'Federal Government', 'General Government', 'National Government'))
    
    seqs_ordered <- sequences(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE,
                      nested = TRUE, ordered = TRUE)
    expect_equal(seqs_ordered$sequence[1:4],
                 c('United States', 'Federal Government', 'General Government', 'Almighty God'))
    
})

test_that("test that nested arguement is working", {
    
    toks <- tokens('E E a b c E E G G f E E f f G G')
    seqs <- sequences(toks, "^[A-Z]$", valuetype="regex", case_insensitive = FALSE,
                      min_count = 1, nested = FALSE, ordered = FALSE)
    expect_equal(seqs$sequence, c('G G', 'E E', 'E E G G'))
    expect_equal(seqs$count, c(1, 2, 1))
    
    seqs_nested <- sequences(toks, "^[A-Z]$", valuetype="regex", case_insensitive = FALSE,
                                min_count = 1, nested = TRUE, ordered = FALSE)
    expect_equal(seqs_nested$sequence, c('G G', 'E E G G', 'E E', 'E G G'))
    expect_equal(seqs_nested$count, c(2, 1, 2, 1))
    
})


