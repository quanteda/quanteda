context('test tokens_lookup')


txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
         d2 = "The Supreme Court of the United States is seldom in a united state.",
         d3 = "It's Arsenal versus Manchester United, states the announcer.",
         d4 = "We need Manchester Unity in the Federal Republic of Germany today.",
         d5 = "United statehood is a good state.",
         d6 = "luv the united states XXOO!")
toks <- tokens(txt, removePunct = TRUE)

test_that("multi-word dictionary keys are counted correctly", {
    
    dict_mw_fixed <- dictionary(list(Countries = c("United States", "Federal Republic of Germany"),
                                     oceans = c("Atlantic Ocean", "Pacific Ocean"),
                                     Institutions = c("federal government", "Supreme Court"),
                                     team = c("Manchester United", "Arsenal")))
    tokens_case_asis <- 
        tokens_lookup(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    
    dfm_case_asis <- dfm(tokens_case_asis, tolower = FALSE)
    expect_equal(as.vector(dfm_case_asis[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis[, "team"]), c(0, 0, 2, 0, 0, 0))
    
    expect_equal(as.vector(dfm_case_asis["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_asis["d3", "Countries"]), 0)
    
    tokens_case_ignore <- 
        tokens_lookup(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = TRUE)
    dfm_case_ignore <- dfm(tokens_case_ignore, tolower = FALSE)
    expect_equal(as.vector(dfm_case_ignore[, "Countries"]), c(1, 1, 1, 1, 0, 1))

    expect_equal(as.vector(dfm_case_ignore["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_ignore["d3", "Countries"]), 1)
    
    dict_mw_glob <- dictionary(list(Countries = c("United States", "Federal * of *"),
                                    oceans = c("* Ocean"),
                                    Institutions = c("federal gover*", "Supreme Court"),
                                    team = c("Manchester *", "Arsenal")))
    tokens_case_asis_glob <- 
        tokens_lookup(toks, dict_mw_glob, valuetype = "glob", case_insensitive = FALSE)
    
    dfm_case_asis_glob <- dfm(tokens_case_asis_glob, tolower = FALSE)
    expect_equal(as.vector(dfm_case_asis_glob[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis_glob[, "oceans"]), c(2, 0, 0, 0, 0, 0))
    expect_equal(as.vector(dfm_case_asis_glob[, "team"]), c(0, 0, 2, 1, 0, 0))
    
})

test_that("entirely single-word dictionary keys are counted correctly", {
    
    dict_sw_fixed <- dictionary(list(Countries = c("States", "Germany"),
                                     oceans = c("Atlantic", "Pacific"),
                                     Institutions = c("government", "Court"),
                                     team = c("Manchester", "Arsenal")))    
    
    tokens_case_asis <- 
        tokens_lookup(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    dfm_case_asis <- dfm(tokens_case_asis, tolower = FALSE)
    expect_equal(as.vector(dfm_case_asis[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis[, "team"]), c(0, 0, 2, 1, 0, 0))
    
    expect_equal(as.vector(dfm_case_asis["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_asis["d3", "Countries"]), 0)
    
    tokens_case_ignore <- 
        tokens_lookup(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = TRUE)
    dfm_case_ignore <- dfm(tokens_case_ignore, tolower = FALSE)
    expect_equal(as.vector(dfm_case_ignore[, "Countries"]), c(1, 1, 1, 1, 0, 1))
    
    expect_equal(as.vector(dfm_case_ignore["d3", "team"]), 2)
    expect_equal(as.vector(dfm_case_ignore["d3", "Countries"]), 1)
    
})


test_that("multi-word dictionary behavior is not sensitive to the order of dictionary entries", {

    txt <- c(d1 = "The United States is a country.", 
             d2 = "Arsenal v Manchester United, states the announcer.")
    toks <- tokens(txt, removePunct = TRUE)
    dict1 <- dictionary(list(Countries = c("United States"),
                             team = c("Manchester United", "Arsenal")))
    dict2 <- dictionary(list(team = c("Arsenal", "Manchester United"),
                             Countries = c("United States")))
    expect_equal(
        lapply(as.list(tokens_lookup(toks, dictionary = dict1, valuetype = "fixed")), sort),
        lapply(as.list(tokens_lookup(toks, dictionary = dict2, valuetype = "fixed")), sort)
    )
    
})

test_that("#388 issue about overlapping key values is resolved: fixed matches", {
    txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
             d2 = "The Supreme Court of the United States is seldom in a united state.")
    toks <- tokens(txt)
    dict_fixed <- dictionary(list(Countries = c("States"),
                                  oceans = c("Atlantic", "Pacific"),
                                  gameconsoles = c("Xbox", "Nintendo"),
                                  swords = c("States")))
    
    expect_equal(as.list(tokens_lookup(toks, dict_fixed, valuetype = "fixed")),
                 list(d1 = c("Countries", "oceans", "oceans", "swords"),
                      d2 = c("Countries", "swords")))
})

test_that("#388 issue about overlapping key values is resolved: glob matches", {
    txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
             d2 = "The Supreme Court of the United States is seldom in a united state.")
    toks <- tokens(txt)
    dict_glob <- dictionary(list(Countries = c("Stat*"),
                                 oceans = c("*ic"),
                                 gameconsoles = c("?box", "Nintendo*"),
                                 swords = "*s"))

    expect_equal(as.list(tokens_lookup(toks, dict_glob, valuetype = "glob")),
              list(d1 = c("Countries", "oceans", "oceans", "swords", "swords"),
                   d2 = c("Countries", "Countries", "swords", "swords")))
    expect_equal(as.list(tokens_lookup(toks, dict_glob, valuetype = "glob", case_insensitive = FALSE)),
                 list(d1 = c("Countries", "oceans", "oceans", "swords", "swords"),
                      d2 = c("Countries", "swords", "swords")))
})

test_that("#388 issue about overlapping key values is resolved: regex matches", {
    txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
             d2 = "The Supreme Court of the United States is seldom in a united state.")
    toks <- tokens(txt)
    dict_regex <- dictionary(list(Countries = c("Stat.*$"),
                                  oceans = c("[A-Z][a-z]+ic"),
                                  gameconsoles = c("Xbox"),
                                  swords = "s$"))

    expect_equal(as.list(tokens_lookup(toks, dict_regex, valuetype = "regex")),
                 list(d1 = c("Countries", "oceans", "oceans", "swords", "swords"),
                      d2 = c("Countries", "Countries", "swords", "swords")))
    expect_equal(as.list(tokens_lookup(toks, dict_regex, valuetype = "regex", case_insensitive = FALSE)),
                 list(d1 = c("Countries", "oceans", "oceans", "swords", "swords"),
                      d2 = c("Countries", "swords", "swords")))
    
})

test_that("non-exclusive lookup is working",{
    
    toks <- tokens(c(d1 = "Mexico signed a new libertarian law with Canada.",
                     d2 = "Let freedom ring in the United States!"),
                   removePunct = TRUE)
    dict <- dictionary(list(country = c("united states", "mexico", "canada"), 
                            "law words" = c('law*', 'constitution'), 
                            freedom = c('free', "freedom", 'libertarian'),
                            overlap = "United"))
    
    expect_equal(as.list(tokens_lookup(toks, dict, exclusive = FALSE, capkeys = TRUE)),
                 list(d1=c("COUNTRY", "signed", "a", "new", "FREEDOM", "LAW WORDS", "with", "COUNTRY"),
                      d2=c("Let", "FREEDOM", "ring", "in", "the", "COUNTRY")))
})

test_that("tokens_lookup preserves case on keys", {
    ## issue #393
    toks <- tokens(data_corpus_inaugural[1:5])
    dict <- dictionary(list(Country = "united states",
                            HOR = c("House of Re*")))
    expect_identical(featnames(dfm(tokens_lookup(toks, dict), tolower = FALSE)),
                     c("Country", "HOR"))
})



test_that("multi-word dictionary behavior is not affected by padding", {
    
    toks <- tokens(c(d1 = "Mexico signed a new libertarian law with Canada.",
                     d2 = "Let freedom ring in the United States!"),
                   removePunct = TRUE)
    toks <- tokens(txt, removePunct = TRUE)
    toks2 <- tokens_remove(toks, stopwords('english'), padding = TRUE)
    dict <- dictionary(list(country = c("united states", "mexico", "canada"), 
                            "law words" = c('law*', 'constitution'), 
                            freedom = c('free', "freedom", 'libertarian'),
                            overlap = "United"))
    expect_equal(
        as.list(tokens_lookup(toks, dictionary = dict, valuetype = "fixed")),
        as.list(tokens_lookup(toks2, dictionary = dict, valuetype = "fixed"))
    )
    
})


test_that("#459 apply only certain levels in a hierarchical dictionary", {
    
    txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
             d2 = "The Supreme Court of the United States is seldom in a united state.")
    toks <- tokens(txt)
    dict <- dictionary(list('geo'=list(
                              Countries = c("States"),
                              oceans = c("Atlantic", "Pacific")),
                            'other'=list(
                              gameconsoles = c("Xbox", "Nintendo"),
                              swords = c("States"))))
    
    expect_equal(as.list(tokens_lookup(toks, dict, valuetype = "fixed", levels=1)),
                 list(d1 = c("geo", "geo", "geo", "other"),
                      d2 = c("geo", "other")))
    
    expect_equal(as.list(tokens_lookup(toks, dict, valuetype = "fixed", levels=1:2)),
                 list(d1 = c("geo.Countries", "geo.oceans", "geo.oceans", "other.swords"),
                      d2 = c("geo.Countries", "other.swords")))
    
    expect_equal(as.list(tokens_lookup(toks, dict, valuetype = "fixed", levels=2)),
                 list(d1 = c("Countries", "oceans", "oceans", "swords"),
                      d2 = c("Countries", "swords")))
})