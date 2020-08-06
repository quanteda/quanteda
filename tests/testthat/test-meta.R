context("test meta functions")

test_that("meta/meta<- works user data", {
    txt <- c(d1 = "a b c", d2 = "x y z")
    corp <- corpus(txt, docvars = data.frame(dv = 1:2))
    toks <- tokens(corp)
    dfmat <- dfm(toks)
    dict <- dictionary(list("key1" = "aaa", "key2" = "bbb"))
    targetmeta3 <- list(usermeta1 = "test1", usermeta2 = "test2", usermeta3 = "test3")
    targetmeta2 <- list(usermeta1 = "test1", usermeta3 = "test3")
    
    meta(corp, "usermeta1") <- "test1"
    meta(corp)["usermeta2"] <- "test2"
    meta(corp)$usermeta3    <- "test3"
    expect_identical(meta(corp), targetmeta3)
    meta(corp)[2] <- NULL
    expect_identical(meta(corp), targetmeta2)
    expect_identical(names(meta(corp, type = "all")), 
                     c("system", "object", "user"))

    meta(toks, "usermeta1") <- "test1"
    meta(toks)["usermeta2"] <- "test2"
    meta(toks)$usermeta3    <- "test3"
    expect_identical(meta(toks), targetmeta3)
    meta(toks)[2] <- NULL
    expect_identical(meta(toks), targetmeta2)
    expect_identical(names(meta(toks, type = "all")), 
                     c("system", "object", "user"))
    
    meta(dfmat, "usermeta1") <- "test1"
    meta(dfmat)["usermeta2"] <- "test2"
    meta(dfmat)$usermeta3    <- "test3"
    expect_identical(meta(dfmat), targetmeta3)
    meta(dfmat)[2] <- NULL
    expect_identical(meta(dfmat), targetmeta2)
    expect_identical(names(meta(dfmat, type = "all")), 
                     c("system", "object", "user"))
    
    meta(dict, "usermeta1") <- "test1"
    meta(dict)["usermeta2"] <- "test2"
    meta(dict)$usermeta3    <- "test3"
    expect_identical(meta(dict), targetmeta3)
    meta(dict)[2] <- NULL
    expect_identical(meta(dict), targetmeta2)
    expect_identical(names(meta(dict, type = "all")), 
                     c("system", "object", "user"))
    
})

test_that("meta.default produces expected error", {
    expect_error(
        meta(NULL),
        "meta() only works on corpus, dfm, dictionary2, tokens objects",
        fixed = TRUE
    )
})

test_that("meta works correctly on pre-v2 objects", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")
    load("../data/pre_v2_objects/data_tokens_pre2.rda")
    load("../data/pre_v2_objects/data_dfm_pre2.rda")
    expect_identical(
        names(meta(data_corpus_pre2)),
        c("source", "notes", "created")
    )
    expect_null(meta(data_tokens_pre2))
    expect_null(meta(data_dfm_pre2))
})

test_that("meta<- works", {
    corp <- corpus(c("one", "two"))
    expect_error(
        meta(corp) <- "needs to be a list",
        "value must be a named list"
    )
    expect_error(
        meta(corp) <- list("needs to be named"),
        "every element of the meta list must be named"
    )
    
    meta(corp, "newmeta") <- "some meta info"
    expect_identical(
        meta(corp),
        list(newmeta = "some meta info")
    )
})

test_that("meta_system", {
    corp <- corpus(c("one", "two"))

    expect_error(
        quanteda:::meta_system(corp) <- "needs to be a list",
        "value must be a named list"
    )
    expect_error(
        quanteda:::meta_system(corp) <- list("needs to be named"),
        "every element of the meta list must be named"
    )
    
    namlist <- c("package-version", "r-version", "system", 
                 "directory", "created") %in% names(quanteda:::meta_system(corp))
    expect_true(all(namlist))
    
    expect_identical(
        meta(corp, type = "system"),
        quanteda:::meta_system(corp)
    )
})

test_that("meta_system<-", {
    corp <- corpus(c("one", "two"))
    corp2 <- quanteda:::"meta_system<-.corpus"(corp, "source", "test-meta.R")
    expect_identical(quanteda:::meta_system(corp2, "source"), "test-meta.R")
    
    toks <- tokens(corp)
    toks2 <- quanteda:::"meta_system<-.tokens"(toks, "source", "test-meta.R")
    expect_identical(quanteda:::meta_system(toks2, "source"), "test-meta.R")
    
    dfmat <- dfm(toks)
    dfmat2 <- quanteda:::"meta_system<-.dfm"(dfmat, "source", "dfm test")
    expect_identical(quanteda:::meta_system(dfmat2, "source"), "dfm test")
})

test_that("adding summary info works", {
    corp <- corpus(data_char_ukimmig2010)
    corp <- quanteda:::add_summary_metadata(corp)
    expect_identical(
        summary(corp),
        quanteda:::get_summary_metadata(corp)
    )

    # for over 100 documents
    set.seed(10)
    corp <- corpus(sample(LETTERS, size = 110, replace = TRUE)) %>%
        quanteda:::add_summary_metadata()
    expect_identical(
        summary(corp, n = ndoc(corp)),
        quanteda:::get_summary_metadata(corp)
    )

    # expect_warning(
    #     get_summary_metadata(corp[1:10]),
    #     "^documents have changed; computing summary$"
    # )
    expect_identical(
        suppressWarnings(quanteda:::get_summary_metadata(corp[1:10])),
        summary(corp[1:10])
    )

    # test when tokens options are passed
    corp1 <- corpus(c(d1 = "One. Two!", d2 = "One 2"))
    corp2 <- quanteda:::add_summary_metadata(corp1,
                                             remove_punct = TRUE,
                                             remove_numbers = TRUE)
    expect_identical(
        summary(corp1, remove_punct = TRUE, remove_numbers = TRUE),
        quanteda:::get_summary_metadata(corp2)
    )
    expect_identical(
        summary(corp2[1], remove_punct = TRUE, remove_numbers = TRUE),
        quanteda:::get_summary_metadata(corp2[1], remove_punct = TRUE, remove_numbers = TRUE)
    )

    # test errors when non-tokens ... are passed
    expect_warning(
        quanteda:::add_summary_metadata(corp1, not_arg = TRUE),
        "^not_arg argument is not used"
    )
})

test_that("adding extended summary information works", {
    corp <- corpus(c("One, two ®, 3. ", "😚 Yeah!!"))
    extsumm <- quanteda:::summarize_texts_extended(corp)
    expect_true(all(
        c("total_tokens", "all_tokens", "total_punctuation", "total_symbols",
          "total_numbers", "total_words", "total_stopwords", "top_dfm") %in%
        names(extsumm)
    ))
    expect_is(extsumm$top_dfm, "dfm")

    corp <- quanteda:::add_summary_metadata(corp, extended = TRUE)
    expect_equivalent(
        meta(corp, "summary_extended", type = "system"),
        extsumm
    )
})


test_that("object meta information is handled properly", {
    
    # make object meta for corpus
    meta_corp1 <- quanteda:::make_meta("corpus")
    expect_identical(
        names(meta_corp1),
        c("system", "object", "user")
    )
    
    # add fields for tokens
    meta_inherit <- meta_corp1
    meta_inherit$object$concatenator <- "+"
    meta_inherit$object$ngram <- 10L
    
    # make object meta for tokens
    meta_toks1 <- quanteda:::make_meta("tokens", 
                                       inherit = meta_inherit,
                                       unit = "sentences")
    expect_identical(
        names(meta_toks1),
        c("system", "object", "user")
    )
    expect_identical(meta_toks1$object$concatenator, "+")
    expect_identical(meta_toks1$object$ngram, 10L)
    expect_identical(meta_toks1$object$unit, "sentences")
    
    # add unused field
    meta_inherit$object$xxx <- 999
    expect_warning(
        quanteda:::make_meta("tokens", "corpus", inherit = meta_inherit),
        "xxx is ignored.", fixed = TRUE
    )
    meta_inherit$object$xxx <- NULL # correct
    expect_warning(
        quanteda:::make_meta("tokens", xxx = 999),
        "xxx is ignored.", fixed = TRUE
    )

    # assign invalid values to used field
    meta_inherit$object$skip <- FALSE
    expect_error(
        quanteda:::make_meta("tokens", inherit = meta_inherit)
    )
    meta_inherit$object$skip <- 0L # correct
    expect_error(
        quanteda:::make_meta("tokens", skip = FALSE)
    )
    # make object meta for dfm
    meta_dfm1 <- quanteda:::make_meta("dfm", 
                                      inherit = meta_inherit,
                                      unit = "paragraphs")
    expect_identical(
        names(meta_dfm1),
        c("system", "object", "user")
    )
    expect_identical(meta_dfm1$object$concatenator, "+")
    expect_identical(meta_dfm1$object$ngram, 10L)
    expect_identical(meta_dfm1$object$unit, "paragraphs")
})
