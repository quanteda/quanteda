context("testing tokens")

##
## tokens_hashed tests
##

test_that("nsyllable works as expected", {
    txt <- c(one = "super freakily yes",
             two = "merrily all go aerodynamic")
    toksh <- tokens(txt)
    toks <- tokenize(txt)
    expect_equivalent(nsyllable(toks), nsyllable(toksh), list(c(2, 3, 1), c(3, 1, 1, 5)))
})

test_that("nsyllable works as expected with padding = TRUE", {
    txt <- c(one = "super freakily yes",
             two = "merrily, all go aerodynamic")
    toks <- tokens_remove(tokens(txt), c("yes", "merrily"), padding = TRUE)
    expect_equivalent(nsyllable(toks), list(c(2, 3, NA), c(NA, NA, 1, 1, 5)))
})


test_that("tokens_wordstem works as expected for tokens_hashed", {

    txt <- c(one = "Eating eater eaters eats ate.",
             two = "Taxing taxes taxed my tax return.")
    toks <- tokenize(char_tolower(txt), remove_punct = TRUE)
    toksh <- tokens(char_tolower(txt), remove_punct = TRUE)
    classic <- tokens_wordstem(toks)
    hashed <- tokens_wordstem(toksh)
    expect_equivalent(classic, as.tokenizedTexts(hashed))
})

test_that("ngrams works as expected for tokens_hashed", {
    txt <- c(one = char_tolower("Insurgents killed in ongoing fighting."),
             two = "A B C D E")
    toks <- tokenize(txt, remove_punct = TRUE)
    toksh <- tokens(txt, remove_punct = TRUE)
    classic <- tokens_ngrams(toks, n = 2:3)
    hashed <- as.tokenizedTexts(tokens_ngrams(toksh, n = 2:3))
    # testthat::expect_equivalent(as.list(classic),
    #                             as.list(hashed))
    classic <- list(sort(unlist(classic$one)), sort(unlist(classic$two)))
    hashed <- list(sort(unlist(hashed$one)), sort(unlist(hashed$two)))
    expect_equivalent(lapply(classic, sort),
                      lapply(hashed, sort))
})

test_that("skipgrams works as expected for tokens_hashed", {
    txt <- c(one = "insurgents killed in ongoing fighting")
    toks <- tokenize(txt)
    toksh <- tokens(txt)
    classic <- skipgrams(toks, n = 3, skip = 0:2, concatenator = " ")
    hashed <- skipgrams(toksh, n = 3, skip = 0:2, concatenator = " ")
    expect_equivalent(classic, as.tokenizedTexts(hashed))
})


test_that("as.tokens tokenizedTexts works as expected", {
    txt <- c(doc1 = "The first sentence is longer than the second.",
             doc2 = "Told you so.")
    toks <- tokenize(txt)
    toksh <- tokens(txt) 
    expect_equivalent(toksh, 
                      as.tokens(toks))
})

test_that("as.tokens list version works as expected", {
    txt <- c(doc1 = "The first sentence is longer than the second.",
             doc2 = "Told you so.")
    toksh <- tokens(txt) 
    toks <- tokenize(txt)
    attributes(toks) <- NULL
    names(toks) <- names(txt)
    expect_equal(class(toks), "list")
    expect_equivalent(toksh, 
                      as.tokens(toks))
})

test_that("as.tokens list version works as expected", {
    txt <- c(doc1 = "The first sentence is longer than the second.",
             doc2 = "Told you so.")
    tokslist <- as.list(tokens(txt))
    toks <- tokens(txt)
    expect_equal(as.tokens(tokslist), 
                      toks)
})


test_that("tokens indexing works as expected", {
    toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))

    expect_equal(toks$d1, c("one", "two", "three"))
    expect_equal(toks[[1]], c("one", "two", "three"))
    
    expect_equal(as.list(toks["d2"]), list(d2 = c("four", "five", "six")))
    expect_equal(as.list(toks[2]), list(d2 = c("four", "five", "six")))
    
    # issue #370
    expect_equal(attr(toks[1], "types"), c("one", "two", "three"))
    expect_equal(attr(toks[2], "types"), c("four", "five", "six"))
})

test_that("tokens_recompile combine duplicates is working", {
    toksh <- tokens(c(one = "a b c d A B C D", two = "A B C d"))
    expect_equivalent(attr(toksh, "types"),
                    c("a", "b", "c", "d", "A", "B", "C", "D"))
    expect_equivalent(attr(tokens_tolower(toksh), "types"),
                    c("a", "b", "c", "d"))
    attr(toksh, "types") <- char_tolower(attr(toksh, "types"))
    expect_equivalent(attr(quanteda:::tokens_recompile(toksh), "types"),
                    c("a", "b", "c", "d"))
    
})

test_that("test `ngrams` with padding = FALSE: #428", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks2 <- tokens_remove(toks, c('b', 'e'), padding = FALSE)
    
    expect_equal(as.list(tokens_ngrams(toks2, n = 2)),
                 list(doc1 = c("a_c", "c_d", "d_f", "f_g")))
    expect_equal(as.list(tokens_ngrams(toks2, n = 3)),
                 list(doc1 = c("a_c_d", "c_d_f", "d_f_g")))
    expect_equal(as.list(tokens_ngrams(toks2, n = 2, skip = 2)),
                 list(doc1 = c("a_f", "c_g")))
})

test_that("test `ngrams` with padding = TRUE: #428", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    
    expect_equal(as.list(tokens_ngrams(toks3, n = 2)),
                 list(doc1 = c("c_d", "f_g")))
    expect_equal(as.list(tokens_ngrams(toks3, n = 3)),
                 list(doc1 = character(0)))
    expect_equal(as.list(tokens_ngrams(toks3, n = 2, skip = 2)),
                 list(doc1 = c("a_d", "c_f", "d_g")))
})

test_that("test dfm with padded tokens, padding = FALSE", {
    toks <- tokens(c(doc1 = 'a b c d e f g',
                     doc2 = 'a b c g',
                     doc3 = ''))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = FALSE)
    expect_equivalent(as.matrix(dfm(toks3)),
                      matrix(c(1, 1, 1, 1, 1, 
                               1, 1, 0, 0, 1,
                               0, 0, 0, 0, 0), nrow = 3, byrow = TRUE))
})

test_that("test dfm with padded tokens, padding = TRUE", {
    toks <- tokens(c(doc1 = 'a b c d e f g',
                     doc2 = 'a b c g',
                     doc3 = ''))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equivalent(as.matrix(dfm(toks3)),
                      matrix(c(2, 1, 1, 1, 1, 1, 
                               1, 1, 1, 0, 0, 1, 
                               0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE))
})

test_that("test verious functions with padded tokens, padding = FALSE", {
    toks <- tokens(c(doc1 = 'A b c d E f g',
                     doc2 = 'a b c g'))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = FALSE)
    expect_equivalent(nfeature(toks3), 6)
    expect_equivalent(nfeature(tokens_tolower(toks3)), 5)
    expect_equivalent(nfeature(tokens_toupper(toks3)), 5)
    expect_equivalent(as.character(toks3),
                      c("A", "c", "d", "f", "g", "a", "c", "g"))
})

test_that("test verious functions with padded tokens, padding = TRUE", {
    toks <- tokens(c(doc1 = 'A b c d E f g',
                     doc2 = 'a b c g'))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equivalent(nfeature(toks3), 7)
    expect_equivalent(nfeature(tokens_tolower(toks3)), 6)
    expect_equivalent(nfeature(tokens_toupper(toks3)), 6)
    expect_equivalent(as.character(toks3),
                      c("A", "", "c", "d", "", "f", "g", "a", "", "c", "g"))
})

test_that("docnames works for tokens", {
    expect_equal(names(data_char_ukimmig2010),
                 docnames(tokens(data_char_ukimmig2010)))
})

test_that("longer features longer than documents do not crash (#447)", {
    toks <- tokens(c(d1 = 'a b', d2 = 'a b c d e'))
    feat <- 'b c d e'
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

test_that("tokens works as expected for what = \"character\"", {
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_separators = TRUE)),
        c("o", "n", "e", ",", "t", "w", "o", "t", "h", "r", "e", "e", ".")
    )
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_separators = FALSE)),
        c("o", "n", "e", ",", " ", "t", "w", "o", " ", "t", "h", "r", "e", "e", ".")
    )
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_punct = TRUE)),
        c("o", "n", "e", "t", "w", "o", "t", "h", "r", "e", "e")
    )
})

test_that("tokens works with unusual hiragana #554", {
    skip_on_travis()
    skip_on_cran()
    skip_on_appveyor()
    skip_on_os("windows")
    txts <- c("づいﾞ", "゛んﾞ", "たーﾟ")
    expect_equivalent(as.list(tokens(txts)),
                      list(c('づ', '', 'いﾞ'), c('゛', '', 'んﾞ'), c('た', '',  'ーﾟ')))
})

test_that("types attribute is a character vector", {
    toks <- tokens("one two three")
    expect_true(is.character(attr(toks, 'types')))
    expect_equal(length(attributes(attr(toks, 'types'))), 0)
})


test_that("remove_url works as expected", {
    txt <- c("The URL was http://t.co/something.",
             "The URL was http://quanteda.io",
             "https://github.com/kbenoit/quanteda/issue/1 is another URL")
    toks <- tokens(txt, remove_url = TRUE)
    expect_equal(
        as.list(toks),
        list(text1 = c("The", "URL", "was"), 
             text2 = c("The", "URL", "was"), 
             text3 = c("is", "another", "URL"))
    )

    toks2 <- tokenize(txt, remove_url = TRUE)
    expect_equivalent(
        as.list(toks2),
        list(c("The", "URL", "was"), c("The", "URL", "was"), c("is", "another", "URL"))
    )
})

test_that("remove_punct and remove_twitter interact correctly, #607", {
    txt <- "they: #stretched, @ @@ in,, a # ## never-ending @line."
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = TRUE, remove_twitter = TRUE)),
        c("they", "stretched", "in", "a", "never-ending", "line")
    )
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = FALSE)),
        c("they", ":", "#stretched", ",", "@", "@@", "in", ",", ",", "a", "#", "##", "never-ending", "@line", ".")
    )    
    # this is #607
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = TRUE, remove_twitter = FALSE)),
        c("they", "#stretched", "in", "a", "never-ending", "@line")
    )
    # remove_twitter should be inactive if remove_punct is FALSE
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = TRUE)),
        as.character(tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = FALSE))
    )
    expect_warning(
        tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = TRUE),
        "remove_twitter reset to FALSE when remove_punct = FALSE"
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
})

test_that("c() works with tokens", {

    txt1 <- c(d1 = "This is sample document one.",
              d2 = "Here is the second sample document.")
    txt2 <- c(d3 = "And the third document.")
    txt3 <- c(d4 = "This is sample document 4.")
    txt4 <- c(d1 = "This is sample document five!")
    
    expect_equal(
        c(tokens(txt1), tokens(txt2)),
        tokens(txt1) + tokens(txt2)
    )
    
    expect_equal(
        c(tokens(txt1), tokens(txt2), tokens(txt3)),
        tokens(txt1) + tokens(txt2) + tokens(txt3)
    )
    
    expect_error(
        c(tokens(txt1), tokens(txt4)),
        'Document names are duplicated'
    )
})

test_that("docvars are erased for tokens added", {
    mycorpus <- corpus(c(d1 = "This is sample document one.",
                         d2 = "Here is the second sample document."), 
                        docvars = data.frame(dvar1 = c("A", "B"), dvar2 = c(1, 2)))
    expect_equivalent(
        docvars(tokens(mycorpus, include_docvars = TRUE)),
        data.frame(dvar1 = c("A", "B"), dvar2 = c(1, 2))
    )
    expect_equivalent(
        docvars(tokens(mycorpus) + tokens("And the third sample document.")),
        data.frame()
    )
})

test_that("what = character works with @ and #, issue #637", {
    
    expect_equal(as.list(tokens("This: is, a @test! #tag", what = "character", remove_punct = FALSE)),
                 list(text1 = c("T", "h", "i", "s", ":", "i", "s", ",", 
                                "a", "@", "t", "e", "s", "t", "!", "#", "t", "a", "g")))
                      
    expect_equal(as.list(tokens("This: is, a @test! #tag", what = "character", remove_punct = TRUE)),
                 list(text1 = c("T", "h", "i", "s", "i", "s", 
                                "a", "t", "e", "s", "t", "t", "a", "g")))

})

test_that("unlist retuns character vector, issue #716", {
    expect_equal(unlist(tokens(c(doc1 = 'aaa bbb cccc', doc2 = 'aaa bbb dddd'))),
                 c(doc11 = "aaa", doc12 = "bbb", doc13 = "cccc",
                   doc21 = "aaa", doc22 = "bbb", doc23 = "dddd"))
    expect_equal(unlist(tokens(c(doc1 = 'aaa bbb cccc', doc2 = 'aaa bbb dddd')), use.names = FALSE),
                 c("aaa", "bbb", "cccc", "aaa", "bbb", "dddd"))
})


test_that("deprecated tokens arguments still work", {

    expect_warning(
        tokens("This contains 99 numbers.", removeNumbers = TRUE),
        "removeNumbers is deprecated"
    )
    
    # for tokens
    expect_identical(
        as.character(tokens(c(d1 = "This: punctuation"), remove_punct = TRUE)),
        c("This", "punctuation")
    )
    expect_identical(
        as.character(tokens(c(d1 = "This: punctuation"), remove_punct = TRUE)),
        as.character(tokens(c(d1 = "This: punctuation"), removePunct = TRUE))
    )
    expect_warning(
        tokens(c(d1 = "This: punctuation"), notanargument = TRUE),
        "Argument notanargument not used"
    )
    
})

test_that("tokens arguments works with values from parent frame (#721)", {
    expect_identical(
        tokens("This contains 99 numbers.", remove_numbers = T),
        tokens("This contains 99 numbers.", remove_numbers = TRUE)
    )
    
    expect_identical(
        dfm("This contains 99 numbers.", remove_numbers = T),
        dfm("This contains 99 numbers.", remove_numbers = TRUE)
    )

    val <- FALSE
    expect_identical(
        tokens("This contains 99 numbers.", remove_numbers = val),
        tokens("This contains 99 numbers.", remove_numbers = F)
    )
    expect_identical(
        dfm("This contains 99 numbers.", remove_numbers = val),
        dfm("This contains 99 numbers.", remove_numbers = F)
    )
})

test_that("tokens works for strange spaces (#796)", {
    txt <- "space tab\t newline\n non-breakingspace\u00A0, em-space\u2003 variationselector16 \uFE0F."
    expect_equal(ntoken(txt, remove_punct = FALSE, remove_separators = TRUE), c(text1 = 8))
    expect_equal(
        as.character(tokens(txt, remove_punct = TRUE, remove_separators = TRUE)),
        c("space", "tab", "newline", "non-breakingspace", "em-space", "variationselector16")
    )
    expect_equal(ntoken(txt, remove_punct = FALSE, remove_separators = FALSE), c(text1 = 18))
    expect_equal(
        as.character(tokens(txt, remove_punct = FALSE, remove_separators = FALSE))[16:18],
        c("variationselector16", " ", ".")
    )
    expect_equal(
        ntoken(txt, remove_punct = TRUE, remove_separators = FALSE),
        c(text1 = 16)
    )
    expect_equal(
        as.character(tokens(txt, remove_punct = TRUE, remove_separators = FALSE))[15:16],
        c("variationselector16", " ")
    )
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

test_that("remove_hyphens is working correctly", {
    txt <- 'a b-c d . !'
    expect_equal(as.character(tokens(txt, remove_hyphens = FALSE, remove_punct = FALSE)[[1]]),
                 c("a", "b-c", "d", ".", "!"))
    expect_equal(as.character(tokens(txt, remove_hyphens = FALSE, remove_punct = TRUE)[[1]]),
                 c("a", "b-c", "d"))
    expect_equal(as.character(tokens(txt, remove_hyphens = TRUE, remove_punct = FALSE)[[1]]),
                 c("a", "b", "-", "c", "d", ".", "!"))
    expect_equal(as.character(tokens(txt, remove_hyphens = TRUE, remove_punct = TRUE)[[1]]),
                 c("a", "b", "c", "d"))
})

test_that("test that features remove by tokens.tokens is comparable to tokens.character", {
    
    chars <- c("a b c 12345 ! @ # $ % ^ & * ( ) _ + { } | : \' \" < > ? ! , . \t \n \u2028 \u00A0 \u2003 \uFE0F",
               "#tag @user", "abc be-fg hi 100kg 2017", "https://github.com/kbenoit/quanteda", "a b c d e")
    toks1 <- as.tokens(stringi::stri_split_fixed(chars[1], ' '))
    toks2 <- as.tokens(stringi::stri_split_fixed(chars[2], ' '))
    toks3 <- as.tokens(stringi::stri_split_fixed(chars[3], ' '))
    toks4 <- as.tokens(stringi::stri_split_fixed(chars[4], ' '))
    toks5 <- as.tokens(stringi::stri_split_fixed(chars[5], ' '))
    
    expect_equal(tokens(chars[1], remove_numbers = TRUE),
                 tokens(toks1, remove_numbers = TRUE))
    
    expect_equal(tokens(chars[1], remove_punct = TRUE),
                 tokens(toks1, remove_punct = TRUE))
    
    expect_equal(tokens(chars[1], remove_separator = TRUE),
                 tokens(toks1, remove_separator = TRUE))
    
    expect_equal(tokens(chars[1], remove_symbols = TRUE),
                 tokens(toks1, remove_symbols = TRUE))
    
    expect_equal(tokens(chars[2], remove_punct = TRUE, remove_twitter = TRUE),
                 tokens(toks2, remove_punct = TRUE, remove_twitter = TRUE))
    
    expect_equal(tokens(chars[4], remove_url = TRUE),
                 tokens(toks4, remove_url = TRUE))
    
    expect_equal(tokens(chars[5], ngrams = 1:2),
                 tokens(toks5, ngrams = 1:2))
    
    expect_equal(tokens(chars[5], ngrams = 2, skip = 1:2),
                 tokens(toks5, ngrams = 2, skip = 1:2))
    
    # This fails because hyphnated words are concatenated in toks
    #expect_equal(tokens(chars[3], remove_hyphens = TRUE),
    #             tokens(toks3, remove_hyphens = TRUE))
    
    # This fails because there is not separator in toks
    # expect_equal(tokens(chars[1], remove_symbols = TRUE, remove_separator = FALSE),
    #              tokens(toks1, remove_symbols = TRUE, remove_separator = FALSE))
    
})

test_that("remove_hyphens is working correctly", {
    corp <- data_corpus_inaugural[1:2]
    toks <- tokens(corp)
    
    expect_equal(dfm(corp), dfm(toks))
    expect_equal(dfm(corp, remove_punct = TRUE), dfm(toks, remove_punct = TRUE))
    expect_equal(setdiff(featnames(dfm(corp, ngrams = 2)), featnames(dfm(toks, ngrams = 2))),
                 character())

})

test_that("tokens works as expected with NA, and blanks", {
    expect_equal(
        as.list(tokens(c("one", "two", ""))),
        list(text1 = "one", text2 = "two", text3 = character())
    )   
    expect_equal(
        as.list(tokens(c("one", NA, ""))),
        list(text1 = "one", text2 = "NA", text3 = character())
    )   
    expect_equal(
        as.list(tokens(c(NA, "one", ""))),
        list(text1 = "NA", text2 = "one", text3 = character())
    )   
    expect_equal(
        as.list(tokens("")),
        list(text1 = character())
    )   
    expect_equal(
        as.list(tokens(c(d1 = "", d2 = NA))),
        list(d1 = character(), d2 = "NA")
    )   
    expect_equal(
        as.list(tokens(c(d1 = NA, d2 = ""))),
        list(d1 = "NA", d2 = character())
    )
    expect_equal(tokens_hash(list("")), tokens_hash(list("")))
    expect_equal(
        as.character(as.tokens(list(""))),
        ""
    )
})
    