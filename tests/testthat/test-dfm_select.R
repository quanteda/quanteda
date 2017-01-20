context("test dfm_select")

txt <- c(doc1 = "a B c D e",
         doc2 = "a BBB c D e",
         doc3 = "Aaaa BBB cc")
testdfm <- dfm(txt, tolower = FALSE)

test_that("test dfm_select, fixed", {
    expect_equal(
        featnames(dfm_select(testdfm, c("a", "b", "c"), selection = "keep", valuetype = "fixed", verbose = FALSE)),
        c("a", "B", "c")
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("a", "b", "c"), selection = "remove", valuetype = "fixed", verbose = FALSE)),
        setdiff(featnames(testdfm), c("a", "B", "c"))
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("a", "b", "c"), selection = "keep", valuetype = "fixed", case_insensitive = FALSE, verbose = FALSE)),
        c("a", "c")
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("a", "b", "c"), selection = "remove", valuetype = "fixed", case_insensitive = FALSE, verbose = FALSE)),
        setdiff(featnames(testdfm), c("a", "c"))
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("aaaa", "bbb", "cc"), selection = "keep", valuetype = "fixed", min_nchar = 3, verbose = FALSE)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("aaaa", "bbb", "cc"), selection = "remove", valuetype = "fixed", min_nchar = 3, verbose = FALSE)),
        setdiff(featnames(testdfm), c("BBB", "Aaaa"))
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("aaaa", "bbb", "cc"), selection = "keep", valuetype = "fixed", min_nchar = 3, max_nchar = 3, verbose = FALSE)),
        c("BBB")
    )
    expect_equal(
        featnames(dfm_select(testdfm, c("aaaa", "bbb", "cc"), selection = "remove", valuetype = "fixed", min_nchar = 3, max_nchar = 3, verbose = FALSE)),
        setdiff(featnames(testdfm), c("BBB"))
    )
})

test_that("test dfm_select, glob", {
    feats <- c("a*", "B*", "c")
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "keep", valuetype = "glob", verbose = FALSE)),
        c("a", "B", "c", "BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "remove", valuetype = "glob", verbose = FALSE)),
        setdiff(featnames(testdfm), c("a", "B", "c", "BBB", "Aaaa"))
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "keep", valuetype = "glob", case_insensitive = FALSE, verbose = FALSE)),
        c("a", "B", "c", "BBB")
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "remove", valuetype = "glob", case_insensitive = FALSE, verbose = FALSE)),
        setdiff(featnames(testdfm), c("a", "B", "c", "BBB"))
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "keep", valuetype = "glob", min_nchar = 3, verbose = FALSE)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "remove", valuetype = "glob", min_nchar = 3, verbose = FALSE)),
        setdiff(featnames(testdfm), c("BBB", "Aaaa"))
    )
})

test_that("test dfm_select, regex", {
    feats <- c("[A-Z].*", "c.+")
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "keep", valuetype = "regex", verbose = FALSE)),
        c("a", "B", "c", "D", "e", "BBB", "Aaaa", "cc")
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "remove", valuetype = "regex", verbose = FALSE)),
        character(0)
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "keep", valuetype = "regex", case_insensitive = FALSE, verbose = FALSE)),
        c("B", "D", "BBB", "Aaaa", "cc")
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "remove", valuetype = "regex", case_insensitive = FALSE, verbose = FALSE)),
        setdiff(featnames(testdfm), c("B", "D", "BBB", "Aaaa", "cc"))
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "keep", valuetype = "regex", min_nchar = 3, verbose = FALSE)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(dfm_select(testdfm, feats, selection = "remove", valuetype = "regex", min_nchar = 3, verbose = FALSE)),
        setdiff(featnames(testdfm), c("BBB", "Aaaa"))
    )
})

test_that("glob works if results in no features", {
    expect_equal(featnames(dfm_select(testdfm, "notthere")), NULL)
})

test_that("featnames.NULL, docnames.NULL works as expected", {
    expect_equal(featnames(NULL), NULL)
    expect_equal(docnames(NULL), NULL)
})

test_that("selection that is out of bounds", {
    expect_equal(dfm_select(testdfm), testdfm)
    
    expect_equal(
        featnames(dfm_select(testdfm, selection = "keep", min_nchar = 5)),
        NULL
    )             

    expect_equal(
        featnames(dfm_select(testdfm, selection = "remove", min_nchar = 5)),
        featnames(testdfm)
    )

    # some tests for docnames and featnames
    expect_equal(docnames(NULL), NULL)
    expect_equal(featnames(NULL), NULL)
})

test_that("longer selection than longer than features that exist (related to #447)", {
    dfmtest <- dfm(tokens(c(d1 = 'a b', d2 = 'a b c d e')))
    feat <- c('b', 'c', 'd', 'e', 'f', 'g')
    # bugs in C++ needs repeated tests
    expect_message(dfm_select(dfmtest, feat, verbose = TRUE),
                   "kept 4 features, from 6 supplied.*")
    expect_equivalent(
        as.matrix(dfm_select(dfmtest, feat)),
        matrix(c(1, 1, 0, 1, 0, 1, 0, 1), nrow = 2)
    )
})

test_that("test dfm_select with features from a dfm,  fixed", {
    expect_equal(
        featnames(dfm_select(testdfm, dfm(c("a", "b", "c")), selection = "keep", valuetype = "fixed", verbose = FALSE)),
        c("a", "B", "c")
    )
    expect_equal(
        featnames(dfm_select(testdfm, dfm(c("a", "b", "c")), selection = "remove", valuetype = "fixed", verbose = FALSE)),
        setdiff(featnames(testdfm), c("a", "B", "c"))
    )
})
    

    
