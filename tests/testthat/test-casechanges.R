test_that("tolower works", {
    txt <- c("According to NATO", "There is G7 meeting")
    expect_equal(char_tolower(txt), c("according to nato", "there is g7 meeting"))
    expect_error(char_tolower(txt, logical()), 
                 "The length of keep_acronyms must be 1")
    expect_error(char_tolower(txt, c(TRUE, FALSE)), 
                 "The length of keep_acronyms must be 1")
})

test_that("char_tolower/char_toUpper works", {
    txt <- c("According to NATO", "There is G7 meeting")
    expect_equal(char_tolower(txt[1]), "according to nato")
    expect_equal(char_toupper(txt[1]), "ACCORDING TO NATO")
})

test_that("char_tolower keeps acronyms", {
    txt <- c("According to NATO", "There is G7 meeting")
    expect_equal(char_tolower(txt, keep_acronyms = TRUE),
                 c("according to NATO", "there is G7 meeting"))
})

test_that("tokens_tolower/tokens_toupper works", {
    txt <- c("According to NATO", "There is G7 meeting")
    toks <- tokens(txt)
    expect_equal(as.list(tokens_tolower(toks)),
                  list(text1 = c("according", "to", "nato"),
                       text2 = c("there", "is", "g7", "meeting")))
    expect_equal(as.list(tokens_tolower(toks, keep_acronyms = TRUE)),
                 list(text1 = c("according", "to", "NATO"),
                      text2 = c("there", "is", "G7", "meeting")))
    expect_equal(as.list(tokens_toupper(toks)),
                 list(text1 = c("ACCORDING", "TO", "NATO"),
                      text2 = c("THERE", "IS", "G7", "MEETING")))
    expect_error(tokens_tolower(toks, logical()), 
                 "The length of keep_acronyms must be 1")
    expect_error(tokens_tolower(toks, c(TRUE, FALSE)), 
                 "The length of keep_acronyms must be 1")
})

test_that("tokens_tolower/tokens_toupper works", {
    txt <- c("According to NATO", "There is G7 meeting")
    dfmat <- dfm(tokens(txt), tolower = FALSE)
    expect_equal(featnames(dfm_tolower(dfmat)),
                 c("according", "to", "nato", "there", "is", "g7", "meeting"))
    expect_equal(featnames(dfm_tolower(dfmat, keep_acronyms = TRUE)),
                 c("according", "to", "NATO", "there", "is", "G7", "meeting"))
    expect_equal(featnames(dfm_toupper(dfmat)),
                 c("ACCORDING", "TO", "NATO", "THERE", "IS", "G7", "MEETING"))
    
    expect_error(dfm_tolower(dfmat, logical()), 
                 "The length of keep_acronyms must be 1")
    expect_error(dfm_tolower(dfmat, c(TRUE, FALSE)), 
                 "The length of keep_acronyms must be 1")
})

test_that("set encoding when no gap or duplication is found, #1387", {
    toks <- tokens("привет tschüß bye")
    toks <- tokens_tolower(toks)
    expect_equal(Encoding(types(toks)), 
                 c("UTF-8", "UTF-8", "unknown")) 
})

test_that("works with empty objects (#2142)", {
    
    dfmat <- as.dfm(matrix(nrow = 0, ncol = 0))
    toks <- as.tokens(list())
    
    expect_identical(types(tokens_tolower(toks)),
                     character())
    expect_identical(types(tokens_toupper(toks)),
                     character())
    
    expect_identical(featnames(dfm_tolower(dfmat)), 
                     character())
    expect_identical(featnames(dfm_toupper(dfmat)), 
                     character())

})

test_that("tokens_toupper() and tokens_tolower() work with verbose", {
    toks <- tokens(c("b A A", "C C a b B"))
    expect_message(
        tokens_tolower(toks, verbose = TRUE),
        "Return .* documents \\(.* types, .* tokens\\) by tokens_tolower\\(\\)"
    )
    expect_message(
        tokens_toupper(toks, verbose = TRUE),
        "Return .* documents \\(.* types, .* tokens\\) by tokens_toupper\\(\\)"
    )
})

test_that("dfm_toupper() and dfm_tolower() work with verbose", {
    dfmat <- dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE)
    expect_message(
        dfm_tolower(dfmat, verbose = TRUE),
        "Return .* documents \\(.* features, .* occurrences\\) by dfm_tolower\\(\\)"
    )
    expect_message(
        dfm_toupper(dfmat, verbose = TRUE),
        "Return .* documents \\(.* features, .* occurrences\\) by dfm_toupper\\(\\)"
    )
})

test_that("fcm_toupper() and fcm_tolower() work with verbose", {
    fcmat <- fcm(dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE))
    expect_message(
        fcm_tolower(fcmat, verbose = TRUE),
        "Return .* and .* features \\(.* co-occurrences\\) by fcm_tolower\\(\\)"
    )
    expect_message(
        fcm_toupper(fcmat, verbose = TRUE),
        "Return .* and .* features \\(.* co-occurrences\\) by fcm_toupper\\(\\)"
    )
})

