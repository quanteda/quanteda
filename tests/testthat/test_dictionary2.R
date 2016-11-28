context('test applyDictionary2')


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
        applyDictionary2(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    
    dfm_case_asis <- dfm(tokens_case_asis, tolower = FALSE)
    expect_equal(as.vector(dfm_case_asis[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis[, "team"]), c(0, 0, 2, 0, 0, 0))
    
    expect_equal(as.vector(dfm_case_asis["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_asis["d3", "Countries"]), 0)
    
    tokens_case_ignore <- 
        applyDictionary2(toks, dict_mw_fixed, valuetype = "fixed", case_insensitive = TRUE)
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
        applyDictionary2(toks, dict_mw_glob, valuetype = "glob", case_insensitive = FALSE)
    
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
        applyDictionary2(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = FALSE)
    dfm_case_asis <- dfm(tokens_case_asis, tolower = FALSE)
    expect_equal(as.vector(dfm_case_asis[, "Countries"]), c(1, 1, 0, 1, 0, 0))
    expect_equal(as.vector(dfm_case_asis[, "team"]), c(0, 0, 2, 1, 0, 0))
    
    expect_equal(as.vector(dfm_case_asis["d3", "team"]), 2)
    # note the overlap of Manchester United states in d3
    expect_equal(as.vector(dfm_case_asis["d3", "Countries"]), 0)
    
    tokens_case_ignore <- 
        applyDictionary2(toks, dict_sw_fixed, valuetype = "fixed", case_insensitive = TRUE)
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
        lapply(as.list(applyDictionary2(toks, dictionary = dict1, valuetype = "fixed")), sort),
        lapply(as.list(applyDictionary2(toks, dictionary = dict2, valuetype = "fixed")), sort)
    )
    
})

test_that("multi-word dictionary preserves the original orders of features", {
    
    txt <- c(d1 = "The United States is a country.", 
             d2 = "Arsenal v Manchester United, states the announcer.")
    toks <- tokens(txt, removePunct = TRUE)
    dict <- dictionary(list(Countries = c("United States"),
                            team = c("Manchester United", "Arsenal")))
    expect_equal(
        unname(as.list(applyDictionary2(toks, dictionary = dict, valuetype = "fixed"))),
        list(c("Countries"), c("team", "team", "Countries"))
    )
    
    
})
