context("test stopwords.R")

test_that("old stopwords are the same as the new", {

    load("../data/data_char_stopwords.rda")
    stopwords_old <- function(kind = "english", swdata) {
        if (!(kind %in% names(swdata)))
            stop(paste0("\"", kind, "\" is not a recognized stopword list name."))
        swdata[[kind]]
    }

    expect_equal(
        stopwords_old("english", data_char_stopwords),
        stopwords("english", source = "snowball")
    )
    expect_equal(
        stopwords_old("SMART", data_char_stopwords),
        stopwords("english", source = "smart")
    )
})

