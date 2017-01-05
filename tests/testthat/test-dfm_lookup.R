context("test dfm_lookup")

test_that("test dfm_lookup, issue #389", {

    toks <- tokens(data_corpus_inaugural[1:5])
    dict <- dictionary(list(Country = "united states",
                            HOR = c("House of Re*"),
                            law = c('law*', 'constitution'), 
                            freedom = c('free*', 'libert*')))
    expect_equal(featnames(dfm(tokens_lookup(toks, dictionary = dict), tolower = FALSE)),
                 c("Country", "HOR", "law", "freedom"))
    expect_error(dfm(toks, dictionary = dict),
                  "dfm_lookup not currently implemented for ngrams > 1 and multi-word dictionary values")

    dict2 <- dictionary(list(Country = "united",
                             HOR = c("House"),
                             law = c('law*', 'constitution'), 
                             freedom = c('free*', 'libert*')))
    expect_equal(as.numeric(dfm(toks, dictionary = dict2)[, "Country"]),
                 c(4, 1, 3, 0, 1))
    
})

