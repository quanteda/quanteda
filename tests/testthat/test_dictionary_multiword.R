context('test multi-word dictionaries')


txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
         d2 = "The Supreme Court of the United States is seldom in a united state.",
         d3 = "It's Arsenal versus Manchester United, states the announcer.",
         d4 = "We need Manchester Unity in the Federal Republic of Germany today.",
         d5 = "United statehood is a good state.",
         d6 = "luv the united states XXOO!")
toks <- tokenize(txt, removePunct = TRUE)
toks_hash <- tokens(txt, removePunct = TRUE)

test_that("multi-word dictionary keys are counted correctly", {
    
    dict_mw_fixed <- dictionary(list(Countries = c("United States", "Federal Republic of Germany"),
                                     oceans = c("Atlantic Ocean", "Pacific Ocean"),
                                     Institutions = c("federal government", "Supreme Court"),
                                     team = c("Manchester United", "Arsenal")))
    tokens_case_asis <- 
        applyDictionary(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    
    dfm_case_asis <- dfm(tokens_case_asis)
    expect_equal(as.vector(dfm_case_asis[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis[, "team"]), c(0, 0, 2, 0, 0, 0))
    
    expect_equal(as.vector(dfm_case_asis["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_asis["d3", "Countries"]), 0)
    
    tokens_case_ignore <- 
        applyDictionary(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = TRUE)
    dfm_case_ignore <- dfm(tokens_case_ignore)
    expect_equal(as.vector(dfm_case_ignore[, "Countries"]), c(1, 1, 1, 1, 0, 1))

    expect_equal(as.vector(dfm_case_ignore["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_ignore["d3", "Countries"]), 1)
    
    
    dfm_case_asis_hash <- applyDictionary(toks_hash, dict_mw_fixed, valuetype = "fixed", case_insensitive = FALSE, concatenator = ' ')
    expect_equal(as.vector(dfm_case_asis_hash[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis_hash[, "team"]), c(0, 0, 2, 0, 0, 0))
    
    dfm_case_ignore_hash <- applyDictionary(toks_hash, dict_mw_fixed, valuetype = "fixed", case_insensitive = TRUE, concatenator = ' ')
    expect_equal(as.vector(dfm_case_ignore_hash[, "Countries"]), c(1, 1, 1, 1, 0, 1))
    
    expect_equal(as.vector(dfm_case_ignore_hash["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_ignore_hash["d3", "Countries"]), 1)
})

test_that("entirely single-word dictionary keys are counted correctly", {
    
    dict_sw_fixed <- dictionary(list(Countries = c("States", "Germany"),
                                     oceans = c("Atlantic", "Pacific"),
                                     Institutions = c("government", "Court"),
                                     team = c("Manchester", "Arsenal")))    
    
    tokens_case_asis <- 
        applyDictionary(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    dfm_case_asis <- dfm(tokens_case_asis)
    expect_equal(as.vector(dfm_case_asis[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis[, "team"]), c(0, 0, 2, 1, 0, 0))
    
    expect_equal(as.vector(dfm_case_asis["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_asis["d3", "Countries"]), 0)
    
    tokens_case_ignore <- 
        applyDictionary(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = TRUE)
    dfm_case_ignore <- dfm(tokens_case_ignore)
    expect_equal(as.vector(dfm_case_ignore[, "Countries"]), c(1, 1, 1, 1, 0, 1))
    
    expect_equal(as.vector(dfm_case_ignore["d3", "team"]), 2)
    expect_equal(as.vector(dfm_case_ignore["d3", "Countries"]), 1)
    
})

test_that("selection of tokens from multi-word dictionaries works", {
    
    dict_mw_fixed <- dictionary(list(Countries = c("United States", "Federal Republic of Germany"),
                                     oceans = c("Atlantic Ocean", "Pacific Ocean"),
                                     Institutions = c("federal government", "Supreme Court"),
                                     team = c("Manchester United", "Arsenal")))

    # does not work for multi-word dictionary keys
    selectFeatures(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    selectFeatures(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = TRUE)

})

test_that("selection of tokens from single-word dictionaries works", {
    
    dict_sw_fixed <- dictionary(list(Countries = c("States", "Germany"),
                                     oceans = c("Atlantic", "Pacific"),
                                     Institutions = c("government", "Court"),
                                     team = c("Manchester", "Arsenal")))    
    
    # works ok for single word dictionary keys
    selectFeatures(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    selectFeatures(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = TRUE)
    
})


test_that("multi-word dictionary behavior is not sensitive to the order of dictionary entries", {

    txt <- c(d1 = "The United States is a country.", 
             d2 = "Arsenal v Manchester United, states the announcer.")
    toks <- tokens(txt, removePunct = TRUE)
    toks_old <- tokenize(txt, removePunct = TRUE)
    dict1 <- dictionary(list(Countries = c("United States"),
                             team = c("Manchester United", "Arsenal")))
    dict2 <- dictionary(list(team = c("Manchester United", "Arsenal"),
                             Countries = c("United States")))
    expect_equal(
        as.list(applyDictionary(toks, dictionary = dict1, valuetype = "fixed", 
                        case_insensitive = TRUE, concatenator = " ")),
        as.list(applyDictionary(toks, dictionary = dict2, valuetype = "fixed", 
                        case_insensitive = TRUE, concatenator = " "))
                 )

    expect_equal(
        as.list(applyDictionary(toks_old, dictionary = dict1, valuetype = "fixed", 
                                case_insensitive = TRUE, concatenator = " ")),
        as.list(applyDictionary(toks_old, dictionary = dict2, valuetype = "fixed", 
                                case_insensitive = TRUE, concatenator = " "))
    )
    
})

test_that("tokenizedTexts and tokens behave the same", {
    
    txt <- c(d1 = "The United States is bordered by the Atlantic Ocean and the Pacific Ocean.",
             d2 = "The Supreme Court of the United States is seldom in a united state.",
             d3 = "It's Arsenal versus Manchester United, states the announcer.",
             d4 = "We need Manchester Unity in the Federal Republic of Germany today.",
             d5 = "United statehood is a good state.",
             d6 = "luv the united states XXOO!")
    toks <- tokenize(txt, removePunct = TRUE)
    toks_hashed <- tokens(txt, removePunct = TRUE)
    dict_mw_fixed <- dictionary(list(Countries = c("United States", "Federal Republic of Germany"),
                                     oceans = c("Atlantic Ocean", "Pacific Ocean"),
                                     Institutions = c("federal government", "Supreme Court"),
                                     team = c("Manchester United", "Arsenal")))

    expect_equal(
        as.tokenizedTexts(applyDictionary(toks_hashed, dictionary = dict_mw_fixed, 
                                          valuetype = "fixed", 
                                          case_insensitive = TRUE, concatenator = " ")),
        applyDictionary(toks, dictionary = dict_mw_fixed, valuetype = "fixed", 
                                case_insensitive = TRUE, concatenator = " ")
    )
})

# not desired output - looks more like joinTokens
applyDictionary(tokens("The United States is big."), 
                dictionary = dictionary(list(COUNTRY = "United States")), 
                valuetype = "fixed", concatenator = " ")
## tokenizedTexts from 1 document.
## Component 1 :
## [1] "The"           "United States" "is"            "big"           "."            

# desired output
applyDictionary(tokens("The United States is big.", hash = FALSE), 
                dictionary = dictionary(list(COUNTRY = "United States")), 
                valuetype = "fixed", concatenator = " ")
## tokenizedTexts from 2 documents.
## Component 1 :
## [1] "COUNTRY"
##
## Component 2 :
## character(0)
