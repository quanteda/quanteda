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

test_that("#459 apply a hierarchical dictionary to a dfm", {
    
    txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
             d2 = "The Supreme Court of the United States is seldom in a united state.")
    testdfm <- dfm(txt)
    dict <- dictionary(list('geo'=list(
        Countries = c("States"),
        oceans = c("Atlantic", "Pacific")),
        'other'=list(
            gameconsoles = c("Xbox", "Nintendo"),
            swords = c("States"))))
    
    expect_equal(
        as.matrix(dfm_lookup(testdfm, dict, valuetype = "fixed", levels = 1)),
        matrix(c(3, 1, 1, 1), ncol = 2, dimnames = list(docs = c("d1", "d2"), features = c("geo", "other")))
    )

    expect_equal(
        as.matrix(dfm_lookup(testdfm, dict, valuetype = "fixed", levels = 1:2)),
        matrix(c(1, 1, 2, 0, 0, 0, 1, 1), ncol = 4, 
               dimnames = list(docs = c("d1", "d2"), features = c("geo.Countries", "geo.oceans", "other.gameconsoles", "other.swords")))
    )


    expect_equal(
        as.matrix(dfm_lookup(testdfm, dict, valuetype = "fixed", levels = 2)),
        matrix(c(1, 1, 2, 0, 0, 0, 1, 1), ncol = 4, 
               dimnames = list(docs = c("d1", "d2"), features = c("Countries", "oceans", "gameconsoles", "swords")))
    )

})

test_that("#459 extract the lower levels of a dictionary using a dfm", {
    txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
             d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
    dict <- dictionary(list('US'=list(
        Countries = c("States"),
        oceans = c("Atlantic", "Pacific")),
        'Europe'=list(
            Countries = c("Britain", "Ireland"),
            oceans = list(west = "Sea", east = "Channel"))))
    testdfm <- dfm(txt)
    dfm_lookup(testdfm, dict, levels = 1)
    dfm_lookup(testdfm, dict, levels = 2)
    dfm_lookup(testdfm, dict, levels = 1:2)
    dfm_lookup(testdfm, dict, levels = 3)
    dfm_lookup(testdfm, dict, levels = c(1,3))
    dfm_lookup(testdfm, dict, levels = c(2,3))
    dfm_lookup(testdfm, dict, levels = c(1,4))
    dfm_lookup(testdfm, dict, levels = 4)
})

test_that("dfm_lookup raises error when dictionary has multi-word entries", {
    
    toks <- tokens(data_corpus_inaugural[1:5])
    dict <- dictionary(list(Country = "united states"), concatenator = ' ')
    expect_error(dfm_lookup(dfm(toks), dictionary = dict), tolower = FALSE)
    
})

