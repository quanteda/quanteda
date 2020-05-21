context("test textstat_sentiment")

test_that("textstat_sentiment works on all object types", {
    skip("until code is final")
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great!",
             d5 = "good good")

    smooth <- 0.5
    logit <- c(log(3 + smooth) - log(2 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(0 + smooth) - log(0 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(2 + smooth) - log(0 + smooth))

    expect_equivalent(
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015),
        data.frame(doc_id = names(txt), sentiment = logit, stringsAsFactors = FALSE)
    )
    expect_identical(
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015),
        textstat_sentiment(corpus(txt), dictionary = data_dictionary_LSD2015)
    )
    expect_identical(
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015),
        textstat_sentiment(tokens(txt), dictionary = data_dictionary_LSD2015)
    )
    expect_identical(
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015),
        textstat_sentiment(dfm(txt), dictionary = data_dictionary_LSD2015)
    )
})

test_that("different sentiment functions work as expected", {
    skip("until code is final")
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great!",
             d5 = "good good")

    # logit scale
    smooth <- 0.5
    logit <- c(log(3 + smooth) - log(2 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(0 + smooth) - log(0 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(2 + smooth) - log(0 + smooth))
    expect_equal(
        logit,
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015)$sentiment
    )

    # relative proportional difference
    rpd <- c(3 - 2,
             1 - 0,
             0 - 0,
             1 - 0,
             2 - 0) / c(5, 1, 0, 1, 2)
    expect_equal(
        rpd,
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015,
                           fun = sent_relpropdiff)$sentiment
    )

    # absolute proportional difference
    apd <- c(3 - 2,
             1 - 0,
             0 - 0,
             1 - 0,
             2 - 0) / unname(ntoken(txt))
    expect_equal(
        apd,
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015,
                           fun = sent_abspropdiff)$sentiment
    )
})

test_that("textstat_sentiment error conditions work", {
    skip("until code is final")
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))
    expect_error(
        textstat_sentiment("Happy, sad, neutral.", dictionary = dict),
        "polarity is not set for this dictionary; see ?polarity", 
        fixed = TRUE
    )
    
})

test_that("polarity functions work", {
    skip("until code is final")
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))

    expect_equal(polarity(dict), NULL)

    polarity(dict) <- list(pos = "happy", neg = "sad")
    expect_identical(
        polarity(dict),
        list(pos = "happy", neg = "sad")
    )

    polarity(dict) <- list(pos = "happy", neg = "sad", neut = "okay")
    expect_identical(
        polarity(dict),
        list(pos = "happy", neg = "sad", neut = "okay")
    )

    polarity(dict) <- list(pos = c("happy", "okay"), neg = "sad")
    expect_identical(
        polarity(dict),
        list(pos = c("happy", "okay"), neg = "sad")
    )

    expect_error(
        polarity(dict) <- list(blank = "happy", neg = "sad"),
        "value must be a list of 'pos', 'neg', and (optionally) 'neut'",
        fixed = TRUE
    )
    expect_error(
        polarity(dict) <- list(pos = "happy", neg = "sad", neutr = "okay"),
        "value must be a list of 'pos', 'neg', and (optionally) 'neut'",
        fixed = TRUE
    )
    
    # this should generate an error
    expect_error(
        polarity(dict) <- list(pos = "notfound", neg = "sad"),
        "'notfound' key not found in this dictionary"
    )

    # should test that both pos and neg are assigned ?

})

test_that("get_polarity_dictionary() works", {
    skip("until code is final")
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))
    polarity(dict) <- list(pos = "happy", neg = "sad", neut = "okay")

    expect_identical(
        quanteda:::get_polarity_dictionary(dict) %>% as.list,
        list(pos = c("happy", "jubilant", "exuberant"),
             neg = c("sad", "morose", "down"),
             neut = "just okay")
    )

    expect_identical(
        quanteda:::get_polarity_dictionary(dict) %>% polarity(),
        list(pos = "pos", neg = "neg", neut = "neut")
    )
    
    polarity(dict) <- list(pos = "happy", neg = "sad", neut = "okay")
    dict["okay"] <- NULL
    expect_error(
        quanteda:::get_polarity_dictionary(dict),
        "'okay' key not found in this dictionary"
    )
})
