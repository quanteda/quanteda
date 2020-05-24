context("test textstat_sentiment")

test_that("textstat_sentiment works for logit scale", {
    skip("until we can figure this out")
    
    dict <- dictionary(list(positive = c("good", "great"),
                            negative = c("bad"),
                            neg_positive = "not good",
                            neg_negative = "not bad"))
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great! Not bad.",
             d5 = "good good not good bad")
    smooth <- 0.5
    logit <- c(log(3 + smooth) - log(2 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(0 + smooth) - log(0 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(2 + smooth) - log(1 + smooth))
    
    # for two categories
    polarity(dict) <- list(positive = 1, negative = -1)
    dfmat <- tokens(txt) %>%
        tokens_lookup(dictionary = dict, nested_scope = "dictionary") %>%
        dfm()
    dfmat <- as.dfm(log(dfmat + smooth))
    expect_equivalent(
        textstat_sentiment(dfmat, dictionary = dict),
        data.frame(doc_id = names(txt), sentiment = logit, stringsAsFactors = FALSE)
    )
    
    # for multiple categories within one polarity
    polarity(dict) <- list(positive = 1, negative = -1, 
                           neg_negative = 1, neg_positive = -1)
    
    ##
    ## will fail because the zero categories of the neg_* get 0.5 added to them
    ##
})

test_that("relative proportional differences works as expected", {
    skip("until this is fixed")
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great!",
             d5 = "good good")
    
    # relative proportional difference
    rpd <- c(3 - 2,
             1 - 0,
             0 - 0,
             1 - 0,
             2 - 0) / c(5, 1, 0, 1, 2)
    rpd[3] <- 0
    expect_equal(
        rpd,
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015)$sentiment
    )
})

test_that("relative proportional differences works as expected", {
    skip("until this is fixed")
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great!",
             d5 = "good good")
    
    # absolute proportional difference
    apd <- c(3 - 2,
             1 - 0,
             0 - 0,
             1 - 0,
             2 - 0) / unname(ntoken(txt))
    expect_equal(
        apd,
        textstat_sentiment(txt, dictionary = data_dictionary_LSD2015)$sentiment
    )
})

test_that("teststat_sentiment works for polarity categories", {
    
})

test_that("textstat_sentiment error conditions work", {
    skip("until fixed in code")
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))
    valence(dict) <- list(
        happy = c("happy" = 1, "jubilant" = 2, "exuberant" = 2),
        sad = c("sad" = -1, "morose" = -2, "down" = -1),
        okay = c("just okay" = 0.5)
    )
    txt <- c(d1 = "sad word happy word exuberant",
             d2 = "down sad just okay",
             d3 = "sad happy word word")
    
    dfmat <- tokens(txt) %>%
        tokens_lookup(dict, nested_scope = "dictionary", nomatch = "other") %>%
        dfm()
    
    sent <- c((-1 + 1 + 2) / 5,
              (-1 - 1 + 0.5) / 3,
              (-1 + 1) / 4)
    expect_equal(
        textstat_sentiment(txt, dict),
        data.frame(doc_id = docnames(dfmat),
                   sentiment = sent)
    )
    
    sent2 <- c((-1 + 1 + 2) / 3,
               (-1 - 1 + 0.5) / 3,
               (-1 + 1) / 2)
    expect_equal(
        textstat_sentiment(txt, dict),
        data.frame(doc_id = docnames(dfmat),
                   sentiment = sent2)
    )
})
    
test_that("textstat_sentiment error conditions work", {
    dict <- dictionary(list(
            happy = c("happy", "jubilant", "exuberant"),
            sad = c("sad", "morose", "down"),
            okay = "just okay"
    ))
    expect_error(
        textstat_sentiment("Happy, sad, neutral.", dictionary = dict),
        "no valence or polarity keys found"
    )
})

test_that("polarity functions work", {
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))
    
    expect_equal(polarity(dict), NULL)

    expect_error(
        polarity(dict) <- list(pos = "happy", neg = "sad"),
        "'pos' is not a dictionary key"
    )
    expect_error(
        polarity(dict) <- list(happy = "a", sad = -1),
        "valence values must be numeric"
    )
    
    polarity(dict) <- list(happy = 1, sad = -1, okay = 0)
    expect_identical(
        polarity(dict),
        list(happy = 1, sad = -1, okay = 0)
    )

    expect_identical(
        valence(dict),
        list(happy = c(happy = 1, jubilant = 1, exuberant = 1), 
             sad = c(sad = -1, morose = -1, down = -1), 
             okay = c(`just okay` = 0))
    )
})

test_that("valence error checks work", {
    dict <- dictionary(list(top = c("top1", "top2"),
                            nested = list(nest1 = c("a", "one"),
                                          nest2 = c("b", "two"))))
    expect_error(
        valence(dict) <- list(top = c(1, 2), nested = -5),
        "valenced dictionaries cannot be nested"
    )
})

test_that("dictionary print method shows valence and polarity", {
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down")
    ))
    polarity(dict) <- c(happy = 1, sad = -1)
    expect_output(print(dict),
        "Dictionary object with 2 key entries.
Polarities: happy 1, sad -1 \b.
- [happy]:
  - happy, jubilant, exuberant
- [sad]:
  - sad, morose, down", fixed = TRUE)
    
    dict <- dictionary(list(
        happiness = c("happy", "jubilant", "exuberant", "content"),
        anger = c("mad", "peeved", "irate", "furious", "livid")
    ))
    valence(dict) <- list(happiness = c(3, 4, 5, 2),
                          anger = c(3.1, 2.4, 2.9, 4.1, 5.0))
    expect_output(print(dict),
        "Dictionary object with 2 key entries.
Valences set for keys: happiness, anger \b.
- [happiness]:
  - happy, jubilant, exuberant, content
- [anger]:
  - mad, peeved, irate, furious, livid", fixed = TRUE)
})
