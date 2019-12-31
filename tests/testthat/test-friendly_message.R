context("test friendly_class_undefined_message")

test_that("friendly_class_undefined_message for as.tokes()", {
    expect_error(
        as.tokens(data_dfm_lbgexample),
        "as.tokens\\(\\) only works on.*list.*spacyr_parsed.*objects"
    )
})

test_that("friendly_class_undefined_message for tokens_tortl()", {
    expect_error(
        tokens_tortl(data_dfm_lbgexample),
        "tokens_tortl\\(\\) only works on.*tokens.*objects"
    )
})

test_that("friendly_class_undefined_message for char_tortl()", {
    expect_error(
        char_tortl(data_dfm_lbgexample),
        "char_tortl\\(\\) only works on.*character.*objects"
    )
})

test_that("friendly_class_undefined_message for featfreq()", {
    expect_error(
        featfreq(tokens(data_char_sampletext)),
        "featfreq\\(\\) only works on dfm objects"
    )
})

test_that("friendly_class_undefined_message for textstat_simil_old()", {
    expect_error(
        textstat_simil_old(tokens(data_char_sampletext)),
        "textstat_simil_old\\(\\) only works on dfm objects"
    )
})
