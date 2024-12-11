quanteda_options("tokens_tokenizer_word" = "word4")

test_that("tokens indexing works as expected", {
    toks <- tokens(c(d1 = "one two three",
                     d2 = "four five six",
                     d3 = "seven eight"))

    expect_equal(toks[[1]], c("one", "two", "three"))
    expect_equal(as.list(toks[c(FALSE, TRUE, FALSE)]), list(d2 = c("four", "five", "six")))
    expect_equal(as.list(toks["d2"]), list(d2 = c("four", "five", "six")))
    expect_equal(as.list(toks[2]), list(d2 = c("four", "five", "six")))
    expect_equal(as.list(toks[c(-1, -3)]), list(d2 = c("four", "five", "six"))) # issue #1830

    # issue #370
    expect_equal(attr(toks[1], "types"), c("one", "two", "three"))
    expect_equal(attr(toks[2], "types"), c("four", "five", "six"))

    # issue #1308
    expect_error(toks[4], "Subscript out of bounds")
    expect_error(toks[1:4], "Subscript out of bounds")
    expect_error(toks["d4"], "Subscript out of bounds")
    expect_error(toks[c("d1", "d4")], "Subscript out of bounds")
})

test_that("test `ngrams` with padding = FALSE: #428", {
    toks <- tokens(c(doc1 = "a b c d e f g"))
    toks2 <- tokens_remove(toks, c("b", "e"), padding = FALSE)

    expect_equal(as.list(tokens_ngrams(toks2, n = 2)),
                 list(doc1 = c("a_c", "c_d", "d_f", "f_g")))
    expect_equal(as.list(tokens_ngrams(toks2, n = 3)),
                 list(doc1 = c("a_c_d", "c_d_f", "d_f_g")))
    expect_equal(as.list(tokens_ngrams(toks2, n = 2, skip = 2)),
                 list(doc1 = c("a_f", "c_g")))
})

test_that("test `ngrams` with padding = TRUE: #428", {
    toks <- tokens(c(doc1 = "a b c d e f g"))
    toks3 <- tokens_remove(toks, c("b", "e"), padding = TRUE)

    expect_equal(as.list(tokens_ngrams(toks3, n = 2)),
                 list(doc1 = c("c_d", "f_g")))
    expect_equal(as.list(tokens_ngrams(toks3, n = 3)),
                 list(doc1 = character(0)))
    expect_equal(as.list(tokens_ngrams(toks3, n = 2, skip = 2)),
                 list(doc1 = c("a_d", "c_f", "d_g")))
})

test_that("test dfm with padded tokens, padding = FALSE", {
    toks <- tokens(c(doc1 = "a b c d e f g",
                     doc2 = "a b c g",
                     doc3 = ""))
    toks3 <- tokens_remove(toks, c("b", "e"), padding = FALSE)
    expect_equivalent(as.matrix(dfm(toks3)),
                      matrix(c(1, 1, 1, 1, 1,
                               1, 1, 0, 0, 1,
                               0, 0, 0, 0, 0), nrow = 3, byrow = TRUE))
})

test_that("test dfm with padded tokens, padding = TRUE", {
    toks <- tokens(c(doc1 = "a b c d e f g",
                     doc2 = "a b c g",
                     doc3 = ""))
    toks3 <- tokens_remove(toks, c("b", "e"), padding = TRUE)
    expect_equivalent(as.matrix(dfm(toks3)),
                      matrix(c(2, 1, 1, 1, 1, 1,
                               1, 1, 1, 0, 0, 1,
                               0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE))
})

test_that("docnames works for tokens", {
    expect_equal(names(data_char_ukimmig2010),
                 docnames(tokens(data_char_ukimmig2010)))
})

test_that("longer features longer than documents do not crash (#447)", {
    toks <- tokens(c(d1 = "a b", d2 = "a b c d e"))
    feat <- "b c d e"
    # bugs in C++ needs repeated tests
    expect_silent(replicate(10, tokens_select(toks, feat)))
    expect_equal(
        as.list(tokens_select(toks, feat)),
        list(d1 = character(0), d2 = character(0))
    )
    expect_equal(
        as.list(tokens_select(toks, phrase(feat))),
        list(d1 = character(0), d2 = c("b", "c", "d", "e"))
    )
})

test_that("tokens works as expected for what = character", {
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_separators = TRUE)),
        c("o", "n", "e", ",", "t", "w", "o", "t", "h", "r", "e", "e", ".")
    )
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_separators = FALSE)),
        c("o", "n", "e", ",", " ", "t", "w", "o", " ", "t", "h", "r", "e", "e", ".")
    )
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_punct = TRUE,
                            remove_separators = TRUE)),
        c("o", "n", "e", "t", "w", "o", "t", "h", "r", "e", "e")
    )
})

test_that("tokens works with unusual hiragana #554", {
    skip_on_cran()
    skip_on_os("windows")
    skip_on_os("mac")
    txts <- c("づいﾞ", "゛んﾞ", "たーﾟ")
    expect_equivalent(as.list(tokens(txts)),
                      list(c("づ", "いﾞ"), c("゛", "んﾞ"), c("た", "ーﾟ")))
})

test_that("types attribute is a character vector", {
    toks <- tokens("one two three")
    expect_true(is.character(attr(toks, "types")))
    expect_equal(length(attributes(attr(toks, "types"))), 0)
})

test_that("remove_url works as expected", {
    txt <- c(text1 = "The URL was http://t.co/something.",
             text2 = "The URL was http://quanteda.io",
             text2a = "The URL was irc://t.co/something.",
             text2b = "The URL was file://quanteda.io",
             text3 = "https://github.com/quanteda/quanteda/issue/1 is another URL",
             text4 = "www.r-project.org/about.html is a specific page without protocol",
             text5 = "https://www.google.com/search?q=quanteda+package is a google search",
             text6 = "ftp://user@host.org/foo/bar.txt is a FTP-hosted file",
             text7 = "kohei.watanabe@quanteda.org is an email address",
             text8 = "The U.S. is not an url")
    toks <- tokens(txt, remove_url = TRUE)
    expect_equal(
        as.list(toks),
        list(text1 = c("The", "URL", "was"),
             text2 = c("The", "URL", "was"),
             text2a = c("The", "URL", "was"),
             text2b = c("The", "URL", "was"),
             text3 = c("is", "another", "URL"),
             text4 = c("is", "a", "specific", "page", "without", "protocol"),
             text5 = c("is", "a", "google", "search"),
             text6 = c("is", "a", "FTP-hosted", "file"),
             text7 = c("is", "an", "email", "address"),
             text8 = c("The", "U.S", ".", "is", "not", "an", "url"))
    )
})

test_that("+ operator works with tokens", {
    txt1 <- c(d1 = "This is sample document one.",
              d2 = "Here is the second sample document.")
    txt2 <- c(d3 = "And the third document.")
    toks_added <- tokens(txt1) + tokens(txt2)
    expect_equal(
        length(unique(as.character(toks_added))),
        length(attr(toks_added, "types"))
    )
    expect_equal(ndoc(toks_added), 3)
    # expect_error(
    #     tokens(txt1, what = "word") + tokens(txt2, what = "sentence"),
    #     "Cannot combine tokens in different tokenization units"
    # )
})

test_that("+ works with empty padded tokens (#1695)", {
    toks1 <- tokens(c(d1 = "a b"))
    toks2 <- tokens(c(d2 = ""))
    toks3 <- tokens(c(d3 = "c"))
    toks4 <- tokens(c(d4 = "c d"))

    expect_identical(
        as.list(toks1 + toks2),
        list(d1 = c("a", "b"), d2 = character(0))
    )
    expect_identical(
        as.list(toks1 + toks3),
        list(d1 = c("a", "b"), d3 = "c")
    )
    expect_identical(
        as.list(toks1 + tokens_remove(toks3, pattern = "c", pad = FALSE)),
        list(d1 = c("a", "b"), d3 = character(0))
    )
    expect_identical(
        as.list(toks1 + tokens_remove(toks3, pattern = "c", pad = TRUE)),
        list(d1 = c("a", "b"), d3 = "")
    )
    expect_identical(
        as.list(tokens_remove(toks3, pattern = "c", pad = TRUE) + toks1),
        list(d3 = "", d1 = c("a", "b"))
    )
    expect_identical(
        as.list(toks1 + tokens_remove(toks4, pattern = "c", pad = FALSE)),
        list(d1 = c("a", "b"), d4 = "d")
    )
    expect_identical(
        as.list(toks1 + tokens_remove(toks4, pattern = "c", pad = TRUE)),
        list(d1 = c("a", "b"), d4 = c("", "d"))
    )
    expect_identical(
        as.list(tokens_remove(toks4, pattern = "c", pad = TRUE) +
                    tokens_remove(toks3, pattern = "c", pad = TRUE)),
        list(d4 = c("", "d"), d3 = "")
    )
})

test_that("c() works with tokens", {

    toks1 <- tokens(c(d1 = "This is sample document one.",
                      d2 = "Here is the second sample document."))
    toks2 <- tokens(c(d3 = "And the third document."))
    toks3 <- tokens(c(d4 = "This is sample document 4."))
    toks4 <- tokens(c(d1 = "This is sample document five!"))

    expect_equal(
        c(toks1),
        toks1
    )

    expect_equal(
        c(toks1, toks2),
        toks1 + toks2
    )

    expect_equal(
        c(toks1, toks2, toks3),
        toks1 + toks2 + toks3
    )

    expect_error(
        c(toks1, toks4),
        "Cannot combine tokens with duplicated document names"
    )

    # issue #1835
    toks <- c(tokens(data_corpus_inaugural[1:2]),
              tokens(data_corpus_inaugural[3:5]),
              tokens(data_corpus_inaugural[6:10]))

    expect_equivalent(
         as.list(toks),
         as.list(tokens(data_corpus_inaugural[1:10]))
    )

    expect_equal(
        docvars(toks),
        docvars(tokens(data_corpus_inaugural[1:10]))
    )
})

test_that("docvars are erased for tokens added", {
    corp <- corpus(c(d1 = "This is sample document one.",
                     d2 = "Here is the second sample document."),
                   docvars = data.frame(dvar1 = c("A", "B"), dvar2 = c(1, 2)))
    toks1 <- tokens(corp, include_docvars = TRUE)
    expect_equivalent(
        docvars(toks1),
        data.frame(dvar1 = c("A", "B"), dvar2 = c(1, 2))
    )
    toks2 <- tokens(c(d3 = "And the third sample document."))
    expect_equivalent(
        docvars(toks1 + toks2),
        data.frame(dvar1 = c("A", "B", NA), dvar2 = c(1, 2, NA))
    )
})

test_that("what = character works with @ and #, issue #637", {
    expect_identical(as.list(tokens("This: is, a @test! #tag", what = "character",
                                remove_punct = FALSE)),
                 list(text1 = c("T", "h", "i", "s", ":", "i", "s", ",",
                                "a", "@", "t", "e", "s", "t", "!", "#", "t", "a", "g")))
    expect_identical(as.list(tokens("This: is, a @test! #tag", what = "character",
                                remove_punct = TRUE)),
                 list(text1 = c("T", "h", "i", "s", "i", "s",
                                "a", "t", "e", "s", "t", "t", "a", "g")))
})

test_that("unlist retuns character vector, issue #716", {
    expect_equal(unlist(tokens(c(doc1 = "aaa bbb cccc", doc2 = "aaa bbb dddd"))),
                 c(doc11 = "aaa", doc12 = "bbb", doc13 = "cccc",
                   doc21 = "aaa", doc22 = "bbb", doc23 = "dddd"))
    expect_equal(unlist(tokens(c(doc1 = "aaa bbb cccc", doc2 = "aaa bbb dddd")), use.names = FALSE),
                 c("aaa", "bbb", "cccc", "aaa", "bbb", "dddd"))
})

test_that("unused argument warnings for tokens work as expected", {

    # for tokens
    expect_identical(
        as.character(tokens(c(d1 = "This: punctuation"), remove_punct = TRUE)),
        c("This", "punctuation")
    )
    expect_warning(
        tokens(c(d1 = "This: punctuation"), notarg1 = TRUE),
        "^notarg1 argument is not used"
    )
    expect_warning(
        tokens(c(d1 = "This: punctuation"), notarg1 = TRUE, notarg2 = FALSE),
        "^notarg1, notarg2 arguments are not used\\."
    )

})

test_that("tokens arguments works with values from parent frame (#721)", {
    expect_identical(
        tokens("This contains 99 numbers.", remove_numbers = T),
        tokens("This contains 99 numbers.", remove_numbers = TRUE),
    )

    suppressWarnings({
        expect_identical(
        dfm(tokens("This contains 99 numbers."), remove_numbers = T),
        dfm(tokens("This contains 99 numbers."), remove_numbers = TRUE)
    )
    })

    val <- FALSE
    expect_identical(
        tokens("This contains 99 numbers.", remove_numbers = val),
        tokens("This contains 99 numbers.", remove_numbers = FALSE)
    )
    suppressWarnings({
        expect_identical(
        dfm(tokens("This contains 99 numbers."), remove_numbers = val),
        dfm(tokens("This contains 99 numbers."), remove_numbers = FALSE)
    )
    })
})

test_that("tokens works for strange spaces (#796)", {
    txt <- "space tab\t newline\n non-breakingspace\u00A0, variationselector16 \uFE0F."
    expect_identical(ntoken(tokens(txt, remove_punct = FALSE, remove_separators = TRUE)),
                     c(text1 = 7L))
    expect_identical(
        as.character(tokens(txt, what = "word", remove_punct = TRUE, remove_separators = TRUE)),
        c("space", "tab", "newline", "non-breakingspace", "variationselector16")
    )
    expect_silent(
        tokens(txt, what = "word", remove_separators = FALSE)
        # "remove_separators is always TRUE for this type"
    )
})

test_that("tokens works with control characters", {
    txt <- "Left-to-Right Override \u202D Zero-Width Non-Breaking Space \ufeff"
    expect_equal(ntoken(txt), c(text1 = 5))
})

test_that("tokens remove whitespace with combining characters (#882)", {

    skip_on_travis()
    skip_on_cran()
    skip_on_appveyor()
    skip_on_os("windows")

    txt <- "( \u0361\u00b0 \u035c\u0296 \u0361\u00b0)"
    tok <- tokens(txt)
    expect_equal(as.list(tok)[[1]],
                 c("(", "°", "ʖ", "°", ")"))

})

test_that("split_hyphens is working correctly", {
    txt <- "a b-c d . !"
    expect_equal(as.character(tokens(txt, split_hyphens = FALSE, remove_punct = FALSE)[[1]]),
                 c("a", "b-c", "d", ".", "!"))
    expect_equal(as.character(tokens(txt, split_hyphens = FALSE, remove_punct = TRUE)[[1]]),
                 c("a", "b-c", "d"))
    expect_equal(as.character(tokens(txt, split_hyphens = TRUE, remove_punct = FALSE)[[1]]),
                 c("a", "b", "-", "c", "d", ".", "!"))
    expect_equal(as.character(tokens(txt, split_hyphens = TRUE, remove_punct = TRUE)[[1]]),
                 c("a", "b", "c", "d"))
})

test_that("tokens.tokens() does nothing by default", {
    toks <- tokens(data_corpus_inaugural,
                   remove_numbers = FALSE,
                   remove_punct = FALSE,
                   remove_symbols = FALSE,
                   remove_separators = TRUE,
                   split_hyphens = FALSE,
                   remove_url = FALSE)
    expect_equal(toks, tokens(toks))
})

test_that("tokens works as expected with NA, and blanks", {
    expect_equal(
        as.list(tokens(c("one", "two", ""))),
        list(text1 = "one", text2 = "two", text3 = character())
    )
    expect_equal(
        as.list(suppressWarnings(tokens(c("one", NA, "")))),
        list(text1 = "one", text2 = character(), text3 = character())
    )
    expect_equal(
        as.list(suppressWarnings(tokens(c(NA, "one", "")))),
        list(text1 = character(), text2 = "one", text3 = character())
    )
    expect_equal(
        as.list(tokens("")),
        list(text1 = character())
    )
    expect_equal(
        as.list(suppressWarnings(tokens(c(d1 = "", d2 = NA)))),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(suppressWarnings(tokens(c(d1 = NA, d2 = "")))),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.character(as.tokens(list(""))),
        character()
    )
})

test_that("assignment operators are disabled for tokens object", {
    toks <- tokens(c(d1 = "a b c d", d2 = "c d e"))

    try(toks[[1]] <- c(6, 100, "z"), silent = TRUE)
    expect_equal(as.list(toks),
                 list(d1 = c("a", "b", "c", "d"), d2 = c("c", "d", "e")))

    expect_error(toks[[1]] <- c(6, 100, "z"), "assignment to tokens objects is not allowed")
    expect_error(toks[1] <- list(c(6, 100, "z")), "assignment to tokens objects is not allowed")
})

test_that("assignment operators are disabled for tokens object", {
    toks <- tokens(c(d1 = "a b c d", d2 = "c d e"))

    try(toks[[1]] <- c(6, 100, "z"), silent = TRUE)
    expect_equal(as.list(toks),
                 list(d1 = c("a", "b", "c", "d"), d2 = c("c", "d", "e")))

    expect_error(toks[[1]] <- c(6, 100, "z"), "assignment to tokens objects is not allowed")
    expect_error(toks[1] <- list(c(6, 100, "z")), "assignment to tokens objects is not allowed")
})

test_that("empty tokens are removed correctly", {
    txt <- "a   b  c d e "
    tok <- c("a", "b", "c", "d", "e")
    expect_equal(as.list(tokens(txt, what = "word"))[[1]], tok)
})

test_that("combined tokens objects have all the attributes", {

    toks1 <- tokens(c(text1 = "a b c"))
    toks2 <- tokens(c(text2 = "d e f"), concatenator = "+")
    toks3 <- tokens(c(text3 = "d e f"), what = "sentence")
    expect_warning(
        toks4 <- tokens(c(text4 = "d e f"), ngram = 1:2, skip = 2),
        "ngram, skip arguments are not used."
    )
    toks5 <- tokens(c(text5 = "d e f"))

    expect_error(c(toks1, toks1),
                 "Cannot combine tokens with duplicated document names")
    expect_error(c(toks1, toks2),
                 "Cannot combine tokens with different concatenators")
    # expect_error(c(toks1, toks3),
    #              "Cannot combine tokens in different tokenization units")

    expect_identical(names(attributes(c(toks1, toks4))),
                     names(attributes(toks1)))
    expect_identical(attr(c(toks1, toks4), "meta")$object$what, "word")
    expect_identical(attr(c(toks1, toks4), "meta")$object$concatenator, "_")
    expect_identical(attr(c(toks1, toks4), "meta")$object$ngram, c(1L))
    expect_identical(attr(c(toks1, toks4), "meta")$object$skip, c(0L))

    expect_identical(docnames(dfm(c(toks1, toks4))), c("text1", "text4"))
    expect_identical(names(attributes(c(toks1, toks5))),
                     names(attributes(toks1)))
    expect_identical(attr(c(toks1, toks5), "meta")$object$what, "word")
    expect_identical(attr(c(toks1, toks5), "meta")$object$concatenator, "_")
    expect_identical(attr(c(toks1, toks5), "meta")$object$ngram, 1L)
    expect_identical(attr(c(toks1, toks5), "meta")$object$skip, 0L)
    expect_identical(docnames(dfm(c(toks1, toks5))), c("text1", "text5"))
})

test_that("tokens.tokens(x, split_hyphens = TRUE) behaves same as tokens.character(...)", {
    # issue #1498
    txt <- "Auto-immune system."
    expect_identical(
        as.character(tokens(txt, split_hyphens = FALSE) |> tokens(split_hyphens = TRUE)),
        c("Auto", "-", "immune", "system", ".")
    )

    txt <- c("There's shrimp-kabobs, shrimp creole. Deep-deep-fried, stir-fried.",
             "Stir-fried shrimp.")
    expect_identical(
        tokens(txt, split_hyphens = TRUE) |> as.list(),
        tokens(txt, split_hyphens = FALSE) |> tokens(split_hyphens = TRUE) |> as.list()
    )
})

test_that("types are encoded when necessarly", {
    toks <- tokens(c("まずは最初の文書。そして、次の文書。", "最後の文書"))
    expect_true(all(Encoding(types(toks)) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_wordstem(toks))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_sample(toks, 1))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_tolower(toks))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_toupper(toks))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_ngrams(toks))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_remove(toks, "の"))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_replace(toks, phrase("次 の"), phrase("次 は")))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_split(toks, "は"))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_chunk(toks, 2))) == "UTF-8"))
    expect_true(all(Encoding(types(tokens_subset(toks, c(TRUE, FALSE)))) == "UTF-8"))

})

test_that("tokens verbose = TRUE produces expected messages", {
    expect_message(
        tokens(c("one two three", "four five."), verbose = TRUE),
        "starting tokenization"
    )
})

test_that("types<- with wrong value generates error", {
    toks <- tokens(c("one two three", "four five."))
    expect_error(
        # quanteda:::`types<-.tokens`(toks, value = 1:6),
        quanteda:::`types<-`(toks, value = 1:6),
        "replacement value must be character"
    )
})

test_that("tokens.tokens warns about unused arguments", {
    expect_warning(
        tokens(tokens("one two three"), notanarg = TRUE),
        "^notanarg argument is not used"
    )
})

test_that("tokens.tokens(x, split_hyphens = TRUE, verbose = TRUE) works as expected  (#1683)", {
    expect_message(
        tokens(tokens("No hyphens here."), split_hyphens = TRUE, verbose = TRUE),
        "splitting hyphens"
    )
    expect_message(
        tokens(tokens("Hyphens oft-cited here."), split_hyphens = TRUE, verbose = TRUE),
        "splitting hyphens"
    )
    expect_identical(
        as.character(tokens(tokens("Hyphens oft-cited here."), split_hyphens = TRUE)),
        c("Hyphens", "oft", "-", "cited", "here", ".")
    )
})

test_that("tokens.tokens(x, split_tags = TRUE, verbose = TRUE) works as expected  (#1683)", {
    expect_warning(
        tokens(tokens("No Twitter."), split_tags = TRUE),
        "split_tags argument is not used"
    )
    expect_message(
        tokens(tokens("Removing #hashtags.", what = "word", verbose = TRUE)),
        "preserving social media tags"
    )
})

test_that("tokens.tokens(x, remove_numbers = TRUE, verbose = TRUE) works as expected (#1683)", {
    expect_message(
        tokens(tokens("Removing no number words."), remove_numbers = TRUE, verbose = TRUE),
        "...removing separators, numbers"
    )
    expect_message(
        tokens(tokens("Removing 1 number words."), remove_numbers = TRUE, verbose = TRUE),
        "...removing separators, numbers"
    )
    expect_identical(
        as.character(tokens(tokens("Removing 1 number words."), remove_numbers = TRUE)),
        c("Removing", "number", "words", ".")
    )
})

test_that("tokens.tokens(x, remove_punct = TRUE, verbose = TRUE) works as expected (#1683)", {
    expect_message(
        tokens(tokens("Removing no £ punctuation"), remove_punct = TRUE, verbose = TRUE),
        "...removing separators, punctuation"
    )
    expect_message(
        tokens(tokens("Removing £ punctuation."), remove_symbols = TRUE, verbose = TRUE),
        "removing separators, symbols"
    )
    expect_message(
        tokens(tokens("Removing £ punctuation."), remove_symbols = TRUE, remove_separators = TRUE, verbose = TRUE),
        "removing separators, symbols"
    )
    expect_identical(
        as.character(tokens(tokens("Removing £ punctuation."), remove_punct = TRUE,
                            remove_symbol = FALSE)),
        c("Removing", "£", "punctuation")
    )
})

test_that("tokens.tokens(x, remove_symbols = TRUE, verbose = TRUE) works as expected (#1683)", {
    expect_message(
        tokens(tokens("Removing no symbols."), remove_symbols = TRUE, verbose = TRUE),
        "removing separators, symbols"
    )
    expect_message(
        tokens(tokens("Removing € symbols."), remove_symbols = TRUE, verbose = TRUE),
        "removing separators, symbols"
    )
    expect_identical(
        as.character(tokens(tokens("Removing € symbols."), remove_symbols = TRUE)),
        c("Removing", "symbols", ".")
    )
})

test_that("tokens.tokens(x, remove_separators = TRUE, verbose = TRUE) works as expected (#1683)", {
    skip("the verbose message has been changed")
    expect_message(
        tokens(tokens("Removing separators", remove_separators = FALSE, what = "word"),
               remove_separators = TRUE, verbose = TRUE),
        "...removing separators"
    )
    expect_message(
        tokens(tokens("Removing no separators", remove_separators = TRUE), remove_separators = TRUE, verbose = TRUE),
        "removing separators"
    )
    expect_identical(
        as.character(
            tokens(tokens("Removing separators", remove_separators = FALSE, what = "word"), remove_separators = TRUE)
        ),
        c("Removing", "separators")
    )
    expect_message(
        tokens(tokens("Removing separators", remove_separators = TRUE), verbose = TRUE),
        c("elapsed time: .+ seconds")
    )
})

test_that("tokens printing works", {
    toks <- tokens(data_corpus_inaugural[1:14])
    expect_silent(
        print(toks, max_ndoc = 0, max_ntoken = 0, show_summary = FALSE)
    )
    expect_output(
        print(toks, max_ndoc = 0, max_ntoken = 0, show_summary = TRUE),
        "Tokens consisting of 14 documents and 4 docvars.",
        fixed = TRUE
    )
    expect_output(
        print(toks, max_ndoc = 2, max_ntoken = 3, show_summary = TRUE),
        paste0('Tokens consisting of 14 documents and 4 docvars.\n',
               '1789-Washington :\n',
               '[1] "Fellow-Citizens" "of"              "the"            \n',
               '[ ... and 1,534 more ]\n\n',
               '1793-Washington :\n',
               '[1] "Fellow"   "citizens" ","       \n',
               '[ ... and 144 more ]\n\n',
               '[ reached max_ndoc ... 12 more documents ]'),
        fixed = TRUE
    )
    expect_output(
        print(toks, max_ndoc = 2, max_ntoken = 3, show_summary = FALSE),
        paste0('1789-Washington :\n',
               '[1] "Fellow-Citizens" "of"              "the"            \n',
               '[ ... and 1,534 more ]\n\n',
               '1793-Washington :\n',
               '[1] "Fellow"   "citizens" ","       \n',
               '[ ... and 144 more ]\n\n',
               '[ reached max_ndoc ... 12 more documents ]'),
        fixed = TRUE
    )
    expect_output(
        print(toks[1:2], max_ndoc = 2, max_ntoken = 3, show_summary = FALSE),
        paste0('1789-Washington :\n',
               '[1] "Fellow-Citizens" "of"              "the"            \n',
               '[ ... and 1,534 more ]\n\n',
               '1793-Washington :\n',
               '[1] "Fellow"   "citizens" ","       \n',
               '[ ... and 144 more ]\n'),
        fixed = TRUE
    )

    expect_output(
        print(tokens("a b c d"), max_ndoc = -1, max_ntoken = 2),
        paste0('Tokens consisting of 1 document.\n',
               'text1 :\n',
               '[1] "a" "b"\n',
               '[ ... and 2 more ]\n'),
        fixed = TRUE
    )
    expect_output(
        print(tokens("a b c d"), max_ndoc = -1, max_ntoken = 4),
        paste0('Tokens consisting of 1 document.\n',
               'text1 :\n',
               '[1] "a" "b" "c" "d"'),
        fixed = TRUE
    )
    expect_output(
        print(tokens("a b c d"), max_ndoc = -1, max_ntoken = -1),
        paste0('Tokens consisting of 1 document.\n',
               'text1 :\n',
               '[1] "a" "b" "c" "d"'),
        fixed = TRUE
    )
})

test_that("tokens.list() works", {
    lis <- list(d1 = c("one", "two-three", "@test"), d2 = c("four", "."))
    expect_identical(as.list(tokens(lis)), lis)
    expect_identical(as.list(tokens(lis, split_hyphens = TRUE)),
                     list(d1 = c("one", "two", "-", "three", "@test"),
                          d2 = c("four", ".")))
})

test_that("tokens.character(x, padding = TRUE) works", {
    txt <- c(doc1 = "One 2, £ https://qunteda.org one-two.")

    # punct
    expect_identical(
        as.list(tokens(txt, what = "word", remove_punct = TRUE, padding = TRUE)),
        list(doc1 = c("One", "2", "", "£", "https://qunteda.org", "one-two", ""))
    )
    expect_identical(
        as.list(tokens(txt, what = "word", remove_punct = TRUE, padding = FALSE)),
        list(doc1 = c("One", "2", "£", "https://qunteda.org", "one-two"))
    )

    # symbols
    expect_identical(
        as.list(tokens(txt, what = "word", remove_symbols = TRUE, padding = TRUE)),
        list(doc1 = c("One", "2", ",", "", "https://qunteda.org", "one-two", "."))
    )
    expect_identical(
        as.list(tokens(txt, what = "word", remove_symbols = TRUE, padding = FALSE)),
        list(doc1 = c("One", "2", ",", "https://qunteda.org", "one-two", "."))
    )

    # numbers
    expect_identical(
        as.list(tokens(txt, what = "word", remove_numbers = TRUE, padding = TRUE)),
        list(doc1 = c("One", "", ",", "£", "https://qunteda.org", "one-two", "."))
    )
    expect_identical(
        as.list(tokens(txt, what = "word", remove_numbers = TRUE, padding = FALSE)),
        list(doc1 = c("One", ",", "£", "https://qunteda.org", "one-two", "."))
    )

    # url
    expect_identical(
        as.list(tokens(txt, what = "word", remove_url = TRUE, padding = TRUE)),
        list(doc1 = c("One", "2", ",", "£", "", "one-two", "."))
    )
    expect_identical(
        as.list(tokens(txt, what = "word", remove_url = TRUE, padding = FALSE)),
        list(doc1 = c("One", "2", ",", "£", "one-two", "."))
    )
})

test_that("tokens.tokens(x, padding = TRUE) works", {
    txt <- c(doc1 = "One 2, £ https://qunteda.org one-two.")
    toks <- tokens(txt, what = "word")

    # punct
    expect_identical(
        as.list(tokens(toks, what = "word", remove_punct = TRUE, padding = TRUE)),
        list(doc1 = c("One", "2", "", "£", "https://qunteda.org", "one-two", ""))
    )
    expect_identical(
        as.list(tokens(toks, what = "word", remove_punct = TRUE, padding = FALSE)),
        list(doc1 = c("One", "2", "£", "https://qunteda.org", "one-two"))
    )

    # symbols
    expect_identical(
        as.list(tokens(toks, what = "word", remove_symbols = TRUE, padding = TRUE)),
        list(doc1 = c("One", "2", ",", "", "https://qunteda.org", "one-two", "."))
    )
    expect_identical(
        as.list(tokens(toks, what = "word", remove_symbols = TRUE, padding = FALSE)),
        list(doc1 = c("One", "2", ",", "https://qunteda.org", "one-two", "."))
    )

    # numbers
    expect_identical(
        as.list(tokens(toks, what = "word", remove_numbers = TRUE, padding = TRUE)),
        list(doc1 = c("One", "", ",", "£", "https://qunteda.org", "one-two", "."))
    )
    expect_identical(
        as.list(tokens(toks, what = "word", remove_numbers = TRUE, padding = FALSE)),
        list(doc1 = c("One", ",", "£", "https://qunteda.org", "one-two", "."))
    )

    # url
    expect_identical(
        as.list(tokens(toks, what = "word", remove_url = TRUE, padding = TRUE)),
        list(doc1 = c("One", "2", ",", "£", "", "one-two", "."))
    )
    expect_identical(
        as.list(tokens(toks, what = "word", remove_url = TRUE, padding = FALSE)),
        list(doc1 = c("One", "2", ",", "£", "one-two", "."))
    )
})

test_that("tokenizing Japanese with URLs works", {
    txt <- c(d1 = "私のユーザー名は@quantedainitです。")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("私", "の", "ユーザー", "名", "は", "@quantedainit", "です", "。"))
    )

    txt <- c(d1 = "私のウェブサイトはhttps://www.nichibenren.or.jp/です。")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("私", "の", "ウェブサイト", "は", "https://www.nichibenren.or.jp/", "です", "。"))
    )

    txt <- c(d1 = "10,000人のフォロワーがいます。")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("10,000", "人", "の", "フォロワー", "がい", "ます", "。"))
    )

    txt <- c(d1 = "私のウェブサイトはhttps://www.nichibenren.or.jp/です。10,000人のフォロワーがいます。")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("私", "の", "ウェブサイト", "は", "https://www.nichibenren.or.jp/", "です", "。", "10,000",
                     "人", "の", "フォロワー", "がい", "ます", "。"))
    )
})

test_that("Non-ASCII hashtags are preserved", {
    txt <- c(d1 = "オリンピック延期決定！ #政治 #安部政権")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("オリンピック", "延期", "決定", "！", "#政治", "#安部政権"))
    )
})

test_that("Weibo-style hashtags are preserved", {
    txt <- c(d1 = "#英国首相#仍在ICU")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("#英国首相#", "仍在", "ICU"))
    )
})

test_that("email addresses are preserved", {
    txt <- c(d1 = "support-team@e-mail.quanteda.io SupportTeam@quanteda.org",
             d2 = "K.Watanabe@qi1234.co.jp K_Watanabe@qi1234.com",
             d3 = "support+noreply@qi-japan.tokyo")
    expect_identical(
        as.list(tokens(txt, what = "word")),
        list(d1 = c("support-team@e-mail.quanteda.io", "SupportTeam@quanteda.org"),
             d2 = c("K.Watanabe@qi1234.co.jp", "K_Watanabe@qi1234.com"),
             d3 = "support+noreply@qi-japan.tokyo")
    )
})

test_that("split_tags works", {
    txt1 <- c(d1 = "@quanteda @koheiw7 @QUANTEDA_INITIATIVE")
    expect_identical(
        as.list(tokens(txt1, what = "word")),
        list(d1 = c("@quanteda", "@koheiw7", "@QUANTEDA_INITIATIVE"))
    )
    expect_identical(
        as.list(tokens(txt1, what = "word", split_tags = TRUE)),
        list(d1 = c("@", "quanteda", "@", "koheiw7", "@", "QUANTEDA_INITIATIVE"))
    )
    
    txt2 <- c(d1 = "#quanteda #q-x #q_y #q100 #q")
    expect_identical(
        as.list(tokens(txt2, what = "word")),
        list(d1 = c("#quanteda", "#q", "-", "x", "#q_y", "#q100", "#q"))
    )
    expect_identical(
        as.list(tokens(txt2, what = "word", split_tags = TRUE)),
        list(d1 = c("#", "quanteda", "#", "q-x", "#", "q_y", "#", "q100", "#", "q"))
    )
})

test_that("edge case usernames are correctly recognized", {
    toks <- tokens("Valid username: @_", remove_punct = TRUE)
    expect_identical(
        as.character(toks),
        c("Valid", "username", "@_")
    )
})

test_that("split_elisions is working", {
    expect_equal(
        tokens("Qu'est-ce que c'est?", split_elisions = FALSE)[[1]],
        c("Qu'est-ce", "que", "c'est", "?")
    )
    expect_equal(
        tokens("Qu'est-ce que c'est?", split_elisions = TRUE)[[1]],
        c("Qu'", "est-ce", "que", "c'", "est", "?")
    )
})

quanteda_options(reset = TRUE)
