context("test dfm_lookup")

test_that("test dfm_lookup, issue #389", {

    toks <- tokens(data_corpus_inaugural[1:5])
    dict <- dictionary(list(country = "united states",
                            HOR = c("House of Re*"),
                            law=c('law*', 'constitution'), 
                            freedom=c('free*', 'libert*')))
    # expect_equivalent(dfm(tokens_lookup(toks, dictionary = dict)),
    #                   dfm(toks, dictionary = dict),
    #                   dfm_lookup(dfm(toks), dictionary = dict))
    
})

