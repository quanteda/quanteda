
test_that("tokens works for strange spaces (#796)", {
    txt <- "space tab\t newline\n non-breakingspace\u00A0, variationselector16 \uFE0F."
    toks <- tokens(txt, what = "word1", remove_punct = FALSE, remove_separators = FALSE)
    expect_identical(ntoken(toks), c(text1 = 15L))
    expect_identical(
        as.character(tokens(txt, what = "word1", remove_punct = FALSE, remove_separators = FALSE))[13:15],
        c("variationselector16", " ", ".")
    )
    expect_identical(
        suppressWarnings(
            ntoken(txt, remove_punct = TRUE, remove_separators = FALSE, what = "word1")
        ),
        c(text1 = 13L)
    )
    expect_identical(
        as.character(tokens(txt, remove_punct = TRUE, remove_separators = FALSE,
                            what = "word1"))[12:13],
        c("variationselector16", " ")
    )
})

test_that("output is correct for word1", {
    skip("the verbose message has been changed")
    expect_message(
        tmp <- tokens(data_char_ukimmig2010, what = "word1", split_hyphens = FALSE, verbose = TRUE),
        "preserving hyphens"
    )
    expect_message(
        tmp <- tokens(data_char_ukimmig2010, what = "word1", split_hyphens = FALSE, verbose = TRUE),
        "Finished constructing tokens from 9 documents"
    )
    expect_message(
        tmp <- tokens(data_char_ukimmig2010, what = "word1", split_hyphens = FALSE, verbose = TRUE),
        "^Creating a tokens object from a character input"
    )
})

test_that("symbols and punctuation are handled separately (#1445)", {
    txt <- "£ € 👏 Rock on❗ 💪️🎸"
    expect_identical(
        as.character(tokens(txt, what = "word1", remove_symbols = FALSE, remove_punct = TRUE)),
        as.character(tokens(txt, what = "word1", remove_symbols = FALSE, remove_punct = FALSE))
    )
    expect_identical(
        as.character(tokens(txt, what = "fasterword", remove_symbols = FALSE, remove_punct = TRUE)),
        as.character(tokens(txt, what = "fasterword", remove_symbols = FALSE, remove_punct = FALSE))
    )
    expect_identical(
        as.character(tokens(txt, what = "fastestword", remove_symbols = FALSE, remove_punct = TRUE)),
        as.character(tokens(txt, what = "fastestword", remove_symbols = FALSE, remove_punct = FALSE))
    )
})
