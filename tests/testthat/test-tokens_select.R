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
                   removePunct = TRUE)
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


