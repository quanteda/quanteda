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


