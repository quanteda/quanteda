txt <- c(doc1 = "a B c D e",
         doc2 = "a BBB c D e",
         doc3 = "Aaaa BBB cc")
toks <- tokens(txt)
dfmt_test <- dfm(toks, tolower = FALSE)

test_that("test dfm_select, fixed", {
    expect_equal(
        featnames(dfm_select(dfmt_test, c("a", "b", "c"), selection = "keep", valuetype = "fixed")),
        c("a", "B", "c")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("a", "b", "c"), selection = "remove", valuetype = "fixed")),
        setdiff(featnames(dfmt_test), c("a", "B", "c"))
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("a", "b", "c"), selection = "keep", valuetype = "fixed", case_insensitive = FALSE)),
        c("a", "c")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("a", "b", "c"), selection = "remove", valuetype = "fixed", case_insensitive = FALSE)),
        setdiff(featnames(dfmt_test), c("a", "c"))
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("aaaa", "bbb", "cc"), selection = "keep", valuetype = "fixed", min_nchar = 3)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("bbb"), selection = "remove", valuetype = "fixed", min_nchar = 3)),
        c("Aaaa")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("aaaa", "bbb", "cc"), selection = "keep", valuetype = "fixed", min_nchar = 3, max_nchar = 3)),
        c("BBB")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, c("bbb"), selection = "remove", valuetype = "fixed", min_nchar = 3, max_nchar = 3)),
        character()
    )
    expect_message(
        dfm_keep(dfmt_test, c("a", "b", "c"), verbose = TRUE),
        "dfm_keep() changed", fixed = TRUE
    )
    expect_message(
        dfm_remove(dfmt_test, c("a", "b", "c"), verbose = TRUE),
        "dfm_remove() changed", fixed = TRUE
    )
})

test_that("test dfm_select, glob", {
    feats <- c("a*", "B*", "c")
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "keep", valuetype = "glob")),
        c("a", "B", "c", "BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "remove", valuetype = "glob")),
        c("D", "e", "cc")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "keep", valuetype = "glob", case_insensitive = FALSE)),
        c("a", "B", "c", "BBB")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "remove", valuetype = "glob", case_insensitive = FALSE)),
        c("D", "e", "Aaaa", "cc")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "keep", valuetype = "glob", min_nchar = 3)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "remove", valuetype = "glob", min_nchar = 3)),
        character()
    )
})

test_that("test dfm_select, regex", {
    feats <- c("[A-Z].*", "c.+")
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "keep", valuetype = "regex")),
        c("a", "B", "c", "D", "e", "BBB", "Aaaa", "cc")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "remove", valuetype = "regex")),
        character()
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "keep", valuetype = "regex", case_insensitive = FALSE)),
        c("B", "D", "BBB", "Aaaa", "cc")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "remove", valuetype = "regex", case_insensitive = FALSE)),
        c("a", "c", "e")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "keep", valuetype = "regex", min_nchar = 3)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(dfmt_test, feats, selection = "remove", valuetype = "regex", min_nchar = 3)),
        character()
    )
})

test_that("glob works if results in no features", {
    expect_equal(featnames(dfm_select(dfmt_test, "notthere")), character())
})

test_that("selection that is out of bounds", {
    expect_equal(dfm_select(dfmt_test), dfmt_test)

    expect_equal(
        featnames(dfm_select(dfmt_test, selection = "keep", min_nchar = 5)),
        character()
    )

    expect_equal(
        featnames(dfm_select(dfmt_test, selection = "remove", min_nchar = 5)),
        character()
    )
})

test_that("test dfm_select with ngrams #589", {
    ngramdfm <- dfm(tokens(c("of_the", "in_the", "to_the", "of_our", "and_the", " it_is", "by_the", "for_the")))
    expect_equal(featnames(dfm_select(ngramdfm, pattern = c("of_the", "in_the"), valuetype = "fixed")),
                 c("of_the", "in_the"))
    expect_equal(featnames(dfm_select(ngramdfm, pattern = "*_the", valuetype = "glob")),
                 c("of_the", "in_the", "to_the", "and_the", "by_the", "for_the"))
})

test_that("test dfm_select with ngrams concatenated with whitespace", {
    ngramdfm <- dfm(tokens(c("of_the", "in_the", "to_the", "of_our", "and_the", " it_is", "by_the", "for_the")))
    colnames(ngramdfm) <- stringi::stri_replace_all_fixed(colnames(ngramdfm), "_", " ")
    expect_equal(
        featnames(dfm_select(ngramdfm, pattern = c("of the", "in the"), valuetype = "fixed")),
        c("of the", "in the")
    )
    expect_equal(
        featnames(dfm_select(ngramdfm, pattern = "* the", valuetype = "glob")),
        c("of the", "in the", "to the", "and the", "by the", "for the")
    )
})

test_that("dfm_select on a dfm returns equal feature sets", {
    txts <- c(d1 = "This is text one", d2 = "The second text", d3 = "This is text three")
    dfmt1 <- dfm(tokens(txts[1:2]))
    dfmt2 <- dfm(tokens(txts[2:3]))
    expect_error({
        dfmt3 <- dfm_select(dfmt1, dfmt2)
    }, "The `pattern` argument of `dfm_select()` cannot be a dfm",
    fixed = TRUE)
})

test_that("dfm_select removes padding", {
    txt <- c(d1 = "This is text one", 
             d2 = "The second text", 
             d3 = "This is text three")
    toks <- tokens(txt)
    toks <- tokens_remove(toks, stopwords(), padding = TRUE)
    expect_true("" %in% featnames(dfm(toks)))
    expect_false("" %in% featnames(dfm(toks, remove_padding = TRUE)))
    expect_false("" %in% featnames(dfm_remove(dfm(toks), "")))

})

# test_that("dfm_select raises warning when padding = TRUE but not valuetype = fixed", {
#
#   expect_warning(dfm_select(testdfm, c("z", "d", "e"), padding = TRUE),
#                  "padding is used only when valuetype is 'fixed'")
#
# })

test_that("dfm_select returns empty dfm when not maching features", {
    expect_equal(dim(dfm_select(dfmt_test, pattern = c("x", "y", "z"))),
                 c(3, 0))
})

test_that("dfm_remove works even when it does not remove anything, issue 711", {
    txt <- c(d1 = "This is text one", d2 = "The second text", d3 = "This is text three")
    dfmt <- dfm(tokens(txt))

    expect_silent(dfm_remove(dfmt, c("xxx", "yyy", "x y")))
    expect_equal(featnames(dfm_remove(dfmt, c("xxx", "yyy", "x y"))),
                 featnames(dfmt))
})

test_that("dfm_select errors when dictionary has multi-word features, issue 775", {
    dfmt <- dfm(tokens(data_corpus_inaugural[50:58]))
    testdict1 <- dictionary(list(eco = c("compan*", "factory worker*"),
                                 pol = c("political part*", "election*")),
                            separator = " ")
    testdict2 <- dictionary(list(eco = c("compan*", "factory_worker"),
                                 pol = c("political_part*", "election*")),
                            separator = "_")
    expect_equal(
        featnames(dfm_select(dfmt, pattern = testdict1, valuetype = "glob")),
        c("election", "elections", "company", "companies")
    )
    expect_equal(
        featnames(dfm_select(dfmt, pattern = phrase(testdict1), valuetype = "glob")),
        c("political", "election", "part", "parties", "elections", "partisan", "company", "participation", "party", "partisanship", "partial", "companies")
    )
    expect_equal(
        featnames(dfm_select(dfmt, pattern = testdict2, valuetype = "glob")),
        c("election", "elections", "company", "companies")
    )
    expect_equal(
        featnames(dfm_select(dfmt, pattern = phrase(testdict2), valuetype = "glob")),
        c("political", "election", "part", "parties", "elections", "partisan", "company", "participation", "party", "partisanship", "partial", "companies")
    )
})


# test_that("dfm_select works when selecting on collocations", {
#     txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
#     toks_uni <- tokens(txt)
#     dfm_uni <- dfm(toks_uni)
#     toks_bi <- tokens(txt) |> tokens_ngrams(n = 2, concatenator = " ")
#     dfm_bi <- dfm(toks_bi)
#     coll_bi <- textstat_collocations(toks_uni, size = 2, min_count = 2)
#     coll_tri <- textstat_collocations(toks_uni, size = 3, min_count = 2)
#
#     expect_equal(
#         dim(dfm_select(dfm_uni, coll_bi)),
#         c(2, 0)
#     )
#     expect_equal(
#         dim(dfm_select(dfm_uni, coll_tri)),
#         c(2, 0)
#     )
#
#     expect_equal(sum(dfm_select(dfm_bi, coll_bi)), 6)
#     expect_equal(featnames(dfm_select(dfm_bi, coll_bi)), c("a b", "e g", "g h"))
#
#     # wrong
#     expect_equal(dim(dfm_select(dfm_bi, coll_tri)), c(2, 0))
#     expect_equal(featnames(dfm_select(dfm_bi, coll_tri)), character())
# })

test_that("shortcut functions works", {
    dfmt <- dfm(tokens(data_corpus_inaugural[1:5]))
    expect_equal(dfm_select(dfmt, stopwords("english"), selection = "keep"),
                 dfm_keep(dfmt, stopwords("english")))
    expect_equal(dfm_select(dfmt, stopwords("english"), selection = "remove"),
                 dfm_remove(dfmt, stopwords("english")))
})

test_that("dfm_remove/keep fail if selection argument is used", {
    dfmt <- dfm(tokens(c("a b c d d", "a a b c d")))
    expect_error(
        dfm_remove(dfmt, c("b", "c"), selection = "remove"),
        "dfm_remove cannot include selection argument"
    )
    expect_error(
        dfm_keep(dfmt, c("b", "c"), selection = "keep"),
        "dfm_keep cannot include selection argument"
    )
})

test_that("dfm_remove works when selection is a dfm (#1320)", {
    d1 <- dfm(tokens("a b b c c c d d d d"))
    d2 <- dfm(tokens("d d d a a"))
    expect_error({
        d3 <- dfm_remove(d1, pattern = d2)
    }, "The `pattern` argument of `dfm_select()` cannot be a dfm",
    fixed = TRUE)
    expect_error({
        d4 <- dfm_select(d1, pattern = d2, selection = "remove")
    }, "The `pattern` argument of `dfm_select()` cannot be a dfm",
    fixed = TRUE)
})

test_that("really long words are not removed in tokens() (#1713)", {
    dfmt <- dfm(tokens("one two DonaudampfschiffahrtselektrizittenhauptbetriebswerkbauunterbeamtengesellschaftXXX"))
    expect_equivalent(nfeat(dfmt), 3)
})

test_that("padding in dfm_select works in the same way as tokens_select (#2152)", {
    
    corp <- corpus(c("a b c d d", "a a b c d"),
                   docvars = data.frame(var1 = c(1, 2), var2 = c(TRUE, FALSE)))
    toks1 <- tokens(corp) 
    dfmt1 <- dfm(toks1)
    
    expect_equal(
        as.vector(dfm_remove(dfmt1, c("a", "b"), padding = TRUE)[,1]),
        c(2, 3)
    )
    expect_equal(
        dim(dfm_remove(dfmt1, c("a", "b"), padding = TRUE)),
        c(2, 3)
    )
    expect_equal(
        dim(dfm_remove(dfmt1, c("z"), padding = TRUE)),
        c(2, 4)
    )
    
    expect_equal(dfm(tokens_select(toks1, "d", padding = TRUE)),
                 dfm_select(dfmt1, "d", padding = TRUE))
    expect_equal(dfm(tokens_select(toks1, "z", padding = TRUE)),
                 dfm_select(dfmt1, "z", padding = TRUE))
    expect_equal(dfm(tokens_remove(toks1, "d", padding = TRUE)),
                 dfm_remove(dfmt1, "d", padding = TRUE))
    expect_equal(dfm(tokens_remove(toks1, "z", padding = TRUE)),
                 dfm_remove(dfmt1, "z", padding = TRUE))
    
    # objects that already have padding
    toks2 <- tokens_remove(toks1, "c", padding = TRUE)
    dfmt2 <- dfm(toks2, remove_padding = FALSE)
    
    expect_equal(
        as.vector(dfm_remove(dfmt2, c("a", "b"), padding = TRUE)[,1]),
        c(3, 4)
    )
    expect_equal(
        dim(dfm_remove(dfmt2, c("a", "b"), padding = TRUE)),
        c(2, 2)
    )
    expect_equal(
        dim(dfm_remove(dfmt2, c("z"), padding = TRUE)),
        c(2, 4)
    )
    
    expect_equal(dfm(tokens_select(toks2, "d", padding = TRUE)),
                 dfm_select(dfmt2, "d", padding = TRUE))
    expect_equal(dfm(tokens_select(toks2, "z", padding = TRUE)),
                 dfm_select(dfmt2, "z", padding = TRUE))
    expect_equal(dfm(tokens_remove(toks2, "d", padding = TRUE)),
                 dfm_remove(dfmt2, "d", padding = TRUE))
    expect_equal(dfm(tokens_remove(toks2, "z", padding = TRUE)),
                 dfm_remove(dfmt2, "z", padding = TRUE))
    
})




