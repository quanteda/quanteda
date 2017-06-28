context('test tokens_select.R')

test_that("test that tokens_select is working", {
    txt <- c(doc1 = "This IS UPPER And Lower case",
             doc2 = "THIS is ALL CAPS aNd sand")
    toks <- tokens(txt)
    
    feats_fixed <- c("and", "is")
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_fixed, selection = "remove", valuetype = "fixed", case_insensitive = TRUE)),
        list(c("This", "UPPER", "Lower", "case"), c("THIS", "ALL", "CAPS", "sand"))
    )
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_fixed, selection = "keep", valuetype = "fixed", case_insensitive = TRUE)),
        list(c("IS", "And"), c("is", "aNd"))
    )
    
    expect_equivalent(
        as.list(selectFeatures(toks, feats_fixed, selection = "remove", valuetype = "fixed", case_insensitive = FALSE)),
        list(c("This", "IS", "UPPER", "And", "Lower", "case"), c("THIS", "ALL", "CAPS", "aNd", "sand"))
    )
    
    expect_equivalent(
        as.list(selectFeatures(toks, feats_fixed, selection = "keep", valuetype = "fixed", case_insensitive = FALSE)),
        list(character(), c("is"))
    )
    
    feats_regex <- c("is$", "and")

    expect_equivalent(
        as.list(tokens_select(toks, feats_regex, selection = "remove", valuetype = "regex", case_insensitive = FALSE)),
        list(c("IS", "UPPER", "And", "Lower", "case"), c("THIS", "ALL", "CAPS", "aNd"))
    )
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_regex, selection = "keep", valuetype = "regex", case_insensitive = FALSE)),
        list(c("This"), c("is", "sand"))
    )
    
    feats_glob <- c("*is*", "?and")
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_glob, selection = "remove", valuetype = "glob", case_insensitive = TRUE)),
        list(c("UPPER", "And", "Lower", "case"), c("ALL", "CAPS", "aNd"))
    )
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_glob, selection = "keep", valuetype = "glob", case_insensitive = TRUE)),
        list(c("This", "IS"), c("THIS", "is", "sand"))
    )
    
    feats_multi <- list(c("this", "is"))
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_multi, selection = "remove", valuetype = "fixed", case_insensitive = TRUE)),
        list(c("UPPER", "And", "Lower", "case"), c("ALL", "CAPS", "aNd", "sand"))
    )
    
    expect_equivalent(
        as.list(tokens_select(toks, feats_multi, selection = "keep", valuetype = "fixed", case_insensitive = TRUE)),
        list(c("This", "IS"), c("THIS", "is"))
    )
    
})


test_that("tokens_select with padding = TRUE is working", {
    toks <- tokens(c(txt1 = "This is a sentence.", txt2 = "This is a second sentence."), 
                   remove_punct = TRUE)
    toks_list <- as.list(tokens_select(toks, c("is", "a", "this"), selection = "keep", padding = TRUE))
    expect_equal(toks_list$txt1[4], "")
    expect_equal(toks_list$txt2[4:5], c("", ""))                 
    
    toks_list <- as.list(tokens_select(toks, c("is", "a", "this"), selection = "remove", padding = TRUE))
    expect_equal(toks_list$txt1[1:3], c("", "", ""))
    expect_equal(toks_list$txt2[1:3], c("", "", ""))
})

test_that("tokens_select reduces the types appropriately", {
    ## see issue/PR #416
    toks <- tokens(c(doc1 = "This is a SAMPLE text", doc2 = "this sample text is better"))
    feats <- c("this", "sample", "is")
    expect_equal(attributes(tokens_select(toks, feats, selection = "keep"))$types,
                 c("This", "is", "SAMPLE", "this", "sample"))
    expect_equal(attributes(tokens_select(toks, feats, selection = "keep", case_insensitive = FALSE))$types,
                 c("is", "this", "sample"))
})

test_that("tokens_remove works on \"\" with tokens containing padding", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equal(as.character(tokens_remove(toks, c("a", "g"))),
                 c("", "c", "d", "", "f"))
    expect_equal(as.character(tokens_remove(toks, "")),
                 c("a", "c", "d", "f", "g"))
})

test_that("tokens_select works on \"\" with tokens containing padding", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equal(as.character(tokens_select(toks, c("a", "b", ""))),
                 c("a", "", ""))
})


test_that("fcm works on tokens containing padding", {
    toks <- tokens(c(doc1 = 'a b c d e f g',
                     doc2 = "f a c c f g b"))
    toks <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equal(featnames(fcm(toks)), c("", "a", "c", "d", "f", "g"))
})

test_that("tokens_remove works regardless when features are overlapped, issue #711", {
    toks <- tokens("one two three four")
    expect_equal(as.list(tokens_remove(toks, features = c("one", "two", "three"))),
                 list('four'))
    expect_equal(as.list(tokens_remove(toks, features = c("one", "two three"))),
                 list(c("two", "three", "four")))
    expect_equal(as.list(tokens_remove(toks, features = c("one two", "two three"))),
                 as.list(toks))
    expect_equal(as.list(tokens_remove(toks, features = c("one two", "two three four"))),
                 as.list(toks))
    # for phrases
    expect_equal(as.list(tokens_remove(toks, features = phrase(c("one two", "two three")))),
                 list("four"))
    expect_equal(as.list(tokens_remove(toks, features = phrase(c("one two", "two three four")))),
                 list(character()))
})



context("test feature selection according to new scheme")

## objects to work with in tests
txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
toks_uni <- tokens(txt)
dfm_uni <- dfm(toks_uni)
toks_bi <- tokens(txt, n = 2, concatenator = " ")
dfm_bi <- dfm(toks_bi)
char_uni <- c("a", "b", "g", "j")
char_bi <- c("a b", "g j")
list_uni <- list("a", "b", "g", "j")
list_bi <- list("a b", "g j")
dict_uni <- dictionary(one = c("a", "b"), two = c("g", "j"))
dict_bi <- dictionary(one = "a b", two = "g j")
coll_bi <- textstat_collocations(toks_uni, method = "lr", max_size = 2)
coll_tri <- textstat_collocations(toks_uni, method = "lr", min_size = 3, max_size = 3)[1, ]

test_that("tokens_select works as expected for unigrams selected on char, list of unigrams", {
    expect_equal(
        as.list(tokens_select(toks_uni, char_uni)),
        list(d1 = c("a", "b", "g"), d2 = c("a", "b", "g", "j"))
    )
    expect_equal(
        tokens_select(toks_uni, list_uni), 
        tokens_select(toks_uni, char_uni)
    )
    expect_equal(
        as.list(tokens_select(toks_uni, c("a b", "c", "g j"))),
        list(d1 = "c", d2 = character())
    )
})

test_that("tokens_select works as expected for unigrams selected on char, list of bigrams", {
    expect_equal(
        as.list(tokens_select(toks_uni, char_bi)),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_select(toks_uni, list_bi)), 
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        tokens_select(toks_uni, list_bi), 
        tokens_select(toks_uni, char_bi)
    )
    expect_equal(
        as.list(tokens_select(toks_uni, phrase(char_bi))),
        list(d1 = c("a", "b"), d2 = c("a", "b"))
    )
})

test_that("tokens_select works as expected for bigrams selected on char, list of unigrams", {
    expect_equal(
        as.list(tokens_select(toks_bi, char_uni)),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        tokens_select(toks_bi, list_uni), 
        tokens_select(toks_bi, char_uni)
    )
    expect_silent(
        tokens_select(toks_uni, list(c("a b", "c"), "g j"))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, list(c("a b", "c"), "g j"))),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_select(toks_uni, list(c("a b", "c"), "g j"), padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
})


test_that("tokens_select works as expected for bigrams selected on char, list of bigrams", {
    expect_equal(
        as.list(tokens_select(toks_bi, char_bi)),
        list(d1 = "a b", d2 = "a b")
    )
    expect_equal(
        as.list(tokens_select(toks_bi, list_bi)), 
        list(d1 = "a b", d2 = "a b")
    )
    expect_equal(
        tokens_select(toks_bi, list_bi), 
        tokens_select(toks_bi, char_bi)
    )
    expect_equal(
        as.list(tokens_select(toks_bi, list(c("a b", "b e"), "g j"))),
        list(d1 = character(0), d2 = c("a b", "b e"))
    )
})

test_that("tokens_select works correctly with collocations objects", {
    expect_equal(
        as.list(tokens_select(toks_uni, coll_bi)),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_remove(toks_uni, phrase(coll_bi))),
        list(d1 = c("c", "d"), d2 = c("i", "j"))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, coll_tri)),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_select(toks_bi, coll_bi)),
        list(d1 = c("a b", "e g", "g h"), d2 = c("a b", "e g", "g h"))
    )
    expect_silent(
        tokens_select(toks_bi, coll_bi)
    )
    expect_equal(
        as.list(tokens_select(toks_bi, coll_tri)),
        list(d1 = character(), d2 = character())
    )
    expect_silent(
        tokens_select(toks_bi, coll_tri)
    )
})


test_that("tokens_select fails as expected with dfm objects", {
    expect_error(tokens_select(toks_uni, dfm_uni))
    expect_error(tokens_select(toks_uni, dfm_bi))
    expect_error(tokens_select(toks_bi, dfm_uni))
    expect_error(tokens_select(toks_bi, dfm_bi))
})


test_that("tokens_select on unigrams works as expected when padding = TRUE", {
    expect_equal(
        as.list(tokens_select(toks_uni, "c d e", padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, list("c d e"), padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, list(c("c", "d", "e")), padding = TRUE)),
        list(d1 = c("", "", "c", "d", "e", "", ""), d2 = rep("", 7))
    )
    
    expect_equal(
        as.list(tokens_select(toks_uni, coll_bi, padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, list_bi, padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
})

test_that("tokens_select on bigrams works as expected when padding = TRUE", {
    expect_equal(
        as.list(tokens_select(toks_bi, "c d e", padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
    expect_equal(
        as.list(tokens_select(toks_bi, list("c d e"), padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
    expect_equal(
        as.list(tokens_select(toks_bi, list(c("c", "d", "e")), padding = TRUE)),
        list(d1 = rep("", 7), d2 = rep("", 7))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, phrase("c d e"), padding = TRUE)),
        list(d1 = c("", "", "c", "d", "e", "", ""), d2 = rep("", 7))
    )
    expect_silent(
        as.list(tokens_select(toks_bi, list(c("c", "d", "e")), padding = TRUE))
    )
    
    expect_equal(
        as.list(tokens_select(toks_bi, coll_bi, padding = TRUE)),
        list(d1 = c("a b", "", "", "", "e g", "g h"), 
             d2 = c("a b", "", "e g", "g h", "", ""))
    )
    expect_silent(
        as.list(tokens_select(toks_bi, coll_bi, padding = TRUE))
    )
})

