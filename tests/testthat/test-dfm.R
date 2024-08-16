
test_that("test c.corpus", {
    
    toks <- tokens(c("What does the fox say?", "What does the fox say?", ""),
                   remove_punct = TRUE)
    expect_equal(
        matrix(dfm(toks)),
        matrix(rep(c(1, 1, 0), 5), nrow = 15, ncol = 1)
    )
})

## rbind.dfm

# TODO: Add function for testing the equality of dfms

test_that("test rbind.dfm with different columns", {
    dfmt1 <- dfm(tokens(c(text1 = "What does the fox?"), remove_punct = TRUE))
    dfmt2 <- dfm(tokens(c(text2 = "fox say"), remove_punct = TRUE))
    dfmt3 <- rbind(dfmt1, dfmt2)
    dfmt4 <- as.dfm(matrix(c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0), nrow = 2,
                    dimnames = list(c("text1", "text2"),
                                    c("does", "fox", "say", "the", "what"))))

    expect_true(
        setequal(featnames(dfmt3), featnames(dfmt4))
    )

    expect_that(
        rbind(dfmt1, dfmt2),
        is_a("dfm")
    )

})

test_that("test rbind.dfm with different columns, three args and repeated words", {
    dfmt1 <- dfm(tokens("What does the?", remove_punct = TRUE))
    dfmt2 <- dfm(tokens("fox say fox", remove_punct = TRUE))
    dfmt3 <- dfm(tokens("The quick brown fox", remove_punct = TRUE))
    dfmt4 <- rbind(dfmt1, dfmt2, dfmt3)

    dfmt5 <- as.dfm(matrix(
        c(0, 0, 1, 1, 0, 0, 0, 2, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0),
        nrow = 3,
        dimnames = list(
            c("text1", "text1", "text1"),
            c("brown", "does", "fox", "quick", "say", "the", "what")
        )
    ))

    expect_true(
        setequal(featnames(dfmt4), featnames(dfmt5))
    )

    expect_that(
        rbind(dfmt1, dfmt2, dfmt3),
        is_a("dfm")
    )

})

test_that("test rbind.dfm with a single argument returns the same dfm", {
    fox <- "What does the fox say?"
    expect_true(
        all(
            rbind(dfm(tokens(fox))) == dfm(tokens(fox))
        )
    )
    expect_that(
        rbind(dfm(tokens(fox, remove_punct = TRUE))),
        is_a("dfm")
    )
})

test_that("test rbind.dfm with the same features, but in a different order", {
    fox <- "What does the fox say?"
    xof <- "say fox the does What??"
    foxdfm <- rep(1, 20)
    dim(foxdfm) <- c(4, 5)
    colnames(foxdfm) <- c("does", "fox", "say", "the", "what")
    rownames(foxdfm) <- rep(c("text1", "text2"), 2)

    dfm1 <- dfm(tokens(c(fox, xof), remove_punct = TRUE))

    expect_true(
        all(rbind(dfm1, dfm1) == foxdfm)
    )
})

test_that("dfm keeps all types with > 10,000 documents (#438) (a)", {
    generate_testdfm <- function(n) {
        dfm(tokens(paste("X", seq_len(n), sep = "")))
    }
    expect_equal(nfeat(generate_testdfm(10000)), 10000)
    expect_equal(nfeat(generate_testdfm(20000)), 20000)
})

test_that("dfm keeps all types with > 10,000 documents (#438) (b)", {
    set.seed(10)
    generate_testdfm <- function(n) {
        dfm(tokens(paste(sample(letters, n, replace = TRUE), 1:n)))
    }
    expect_equal(nfeat(generate_testdfm(10000)), 10026)
    expect_equal(nfeat(generate_testdfm(10001)), 10027)
})

test_that("dfm.dfm works as expected", {
    corp <- data_corpus_inaugural
    toks <- tokens(corp)
    dfmt <- dfm(toks, tolower = FALSE)

    expect_identical(dfm(toks, tolower = FALSE), dfm(dfmt, tolower = FALSE))
    expect_identical(dfm(toks, tolower = TRUE), dfm(dfmt, tolower = TRUE))

    expect_identical(dfmt, dfm(dfmt, tolower = FALSE))
    expect_identical(dfm_tolower(dfmt), dfm(dfmt, tolower = TRUE))

    # REMOVED in v3
    # expect_true({
    #     sum(suppressWarnings(dfm(tokens(corp), select = c("The", "a", "an")))) >
    #     sum(suppressWarnings(dfm(tokens(corp), select = c("The", "a", "an"), case_insensitive = FALSE)))
    # })

    # expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = FALSE, tolower = FALSE),
    #                  dfm_remove(dfmt, c("The", "a", "an"), case_insensitive = FALSE))
    # expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = TRUE, tolower = FALSE),
    #                  dfm_remove(dfmt, c("The", "a", "an"), case_insensitive = TRUE))

    # expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = FALSE),
    #                  dfm(tokens_remove(toks, c("The", "a", "an"), case_insensitive = FALSE)))
    # expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = TRUE),
    #                  dfm(tokens_remove(toks, c("The", "a", "an"), case_insensitive = TRUE)))


})

test_that("cbind.dfm works as expected", {
    dfm1 <- dfm(tokens("This is one sample text sample"))
    dfm2 <- dfm(tokens("More words here"))
    dfm12 <- cbind(dfm1, dfm2)

    expect_equal(nfeat(dfm12), 8)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("cbind.dfm works with non-dfm objects", {
    dfm1 <- dfm(tokens(c("a b c", "c d e")))

    vec <- c(10, 20)
    expect_equal(
        as.matrix(cbind(dfm1, vec)),
        matrix(c(1, 1, 1, 0, 0, 10, 0, 0, 1, 1, 1, 20), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat1")))
    )
    expect_equal(
        as.matrix(cbind(vec, dfm1)),
        matrix(c(10, 1, 1, 1, 0, 0, 20, 0, 0, 1, 1, 1), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c("feat1", letters[1:5])))
    )

    mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("f1", "f2")))
    expect_equal(
        as.matrix(cbind(dfm1, mat)),
        matrix(c(1,1,1,0,0,1,3, 0,0,1,1,1,2,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "f1", "f2")))
    )
    expect_equal(
        as.matrix(cbind(mat, dfm1)),
        matrix(c(1,3,1,1,1,0,0, 2,4,0,0,1,1,1), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c("f1", "f2", letters[1:5])))
    )

    expect_equal(
        as.matrix(cbind(dfm1, vec, mat)),
        matrix(c(1,1,1,0,0,10,1,3, 0,0,1,1,1,20,2,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"),
                               features = c(letters[1:5], "feat1", "f1", "f2")))
    )

    expect_equal(
        suppressWarnings(as.matrix(cbind(vec, dfm1, vec))),
        matrix(c(10,1,1,1,0,0,10, 20,0,0,1,1,1,20), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"),
                               features = c("feat1", letters[1:5], "feat1")))
    )

    expect_warning(
        cbind(vec, dfm1, vec),
        "cbinding dfms with overlapping features"
    )
    expect_warning(
        cbind(dfm1, dfm1),
        "cbinding dfms with overlapping features"
    )

    expect_equal(
        as.matrix(cbind(dfm1, 100)),
        matrix(c(1, 1, 1, 0, 0, 100, 0, 0, 1, 1, 1, 100), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat1")))
    )

})

test_that("more cbind tests for dfms", {
    txts <- c("a b c d", "b c d e")
    mydfm <- dfm(tokens(txts))

    expect_equal(
        as.matrix(cbind(mydfm, as.dfm(cbind("ALL" = ntoken(mydfm))))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
                  dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "ALL")))
    )

    expect_equal(
        as.matrix(cbind(mydfm, cbind("ALL" = ntoken(mydfm)))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "ALL")))
    )

    expect_equal(
        as.matrix(cbind(mydfm, "ALL" = ntoken(mydfm))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "ALL")))
    )
    expect_equal(
        as.matrix(cbind(mydfm, ntoken(mydfm))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat1")))
    )
})

test_that("cbind.dfm keeps attributes of the dfm", {
    mx1 <- as.dfm(matrix(c(0, 0, 0, 0, 1, 2), nrow = 2,
                         dimnames = list(c("doc1", "doc2"), c("aa", "bb", "cc"))))
    mx2 <- as.dfm(matrix(c(2, 3, 0, 0, 0, 0), nrow = 2,
                         dimnames = list(c("doc1", "doc2"), c("dd", "ee", "ff"))))
    meta(mx1, "settings") <- list(somesetting = "somevalue")
    mx3 <- cbind(mx1, mx2)
    expect_equal(meta(mx3), list(settings = list(somesetting = "somevalue")))
})

test_that("rbind.dfm works as expected", {
    dfm1 <- dfm(tokens("This is one sample text sample"))
    dfm2 <- dfm(tokens("More words here"))
    dfm12 <- rbind(dfm1, dfm2)

    expect_equal(nfeat(dfm12), 8)
    expect_equal(ndoc(dfm12), 2)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("dfm works with relational operators", {
    testdfm <- dfm(tokens(c("This is an example.", "This is a second example.")))
    expect_is(testdfm == 0, "lgCMatrix")
    expect_is(testdfm >= 0, "lgCMatrix")
    expect_is(testdfm <= 0, "lgCMatrix")
    expect_is(testdfm < 0, "lgCMatrix")
    expect_is(testdfm < 1, "lgCMatrix")
    expect_is(testdfm > 0, "lgCMatrix")
    expect_is(testdfm > 1, "lgCMatrix")
    expect_is(testdfm > -1, "lgCMatrix")
    expect_is(testdfm < -1, "lgCMatrix")
})

test_that("dfm addition (+) keeps attributes #1279", {
    dfmt <- head(data_dfm_lbgexample, 4)

    # @settings slot
    meta(dfmt, "testsetting") <- list(test = 1)
    expect_equal(
        meta(dfmt + 1)["testsetting"],
        list(testsetting = list(test = 1))
    )
    expect_equal(
        meta(1 + dfmt)["testsetting"],
        list(testsetting = list(test = 1))
    )

    # @weightTf slot
    dfmt@meta$object$weight_tf <- list(scheme = "prop", base = exp(1), K = 2)
    expect_equal(
        (dfmt + 1)@meta$object$weight_tf,
        list(scheme = "prop", base = exp(1), K = 2)
    )
    expect_equal(
        (1 + dfmt)@meta$object$weight_tf,
        list(scheme = "prop", base = exp(1), K = 2)
    )

    # @weightDf slot
    weight <- list(scheme = "idf", base = NULL, c = NULL,
                   smoothing = NULL, threshold = NULL)
    dfmt@meta$object$weight_df <- weight
    expect_equal(
        (dfmt + 1)@meta$object$weight_df,
        weight
    )
    expect_equal(
        (1 + dfmt)@meta$object$weight_df,
        weight
    )

    # @smooth slot
    dfmt@meta$object$smooth <- 5.5
    expect_equal(
        (dfmt + 1)@meta$object$smooth,
        5.5
    )
    expect_equal(
        (1 + dfmt)@meta$object$smooth,
        5.5
    )

    # @ngrams slot
    dfmt@meta$object$ngram <- 5L
    expect_equal(
        (dfmt + 1)@meta$object$ngram,
        5L
    )
    expect_equal(
        (1 + dfmt)@meta$object$ngram,
        5L
    )

    # @skip slot
    dfmt@meta$object$skip <- 3L
    expect_equal(
        (dfmt + 1)@meta$object$skip,
        3L
    )
    expect_equal(
        (1 + dfmt)@meta$object$skip,
        3L
    )

    # @concatenator slot
    dfmt@meta$object$concatenator <- "+-+"
    expect_equal(
        (dfmt + 1)@meta$object$concatenator,
        "+-+"
    )
    expect_equal(
        (1 + dfmt)@meta$object$concatenator,
        "+-+"
    )

    # @version slot
    dfmt@meta$system$`package-version` <- as.package_version("10.5.1")
    expect_equal(
        (dfmt + 1)@meta$system$`package-version`,
        as.package_version("10.5.1")
    )
    expect_equal(
        (1 + dfmt)@meta$system$`package-version`,
        as.package_version("10.5.1")
    )

    # @docvars slot
    dfmt@docvars <- data.frame(test = letters[1:ndoc(dfmt)])
    expect_equal(
        (dfmt + 1)@docvars,
        data.frame(test = letters[1:ndoc(dfmt)])
    )
    expect_equal(
        (1 + dfmt)@docvars,
        data.frame(test = letters[1:ndoc(dfmt)])
    )
})


test_that("dfm print works with options as expected", {
    dfmt <- dfm(tokens(data_corpus_inaugural[1:14],
                remove_punct = FALSE, remove_numbers = FALSE, split_hyphens = TRUE))
    expect_output(
        print(dfmt, max_ndoc = 6, max_nfeat = 10, show_summary = TRUE),
        paste0("^Document-feature matrix of: 14 documents, 4,452 features \\(81\\.97% sparse\\) and 4 docvars",
               ".*",
               "\\[ reached max_ndoc \\.\\.\\. 8 more documents, reached max_nfeat \\.\\.\\. 4,442 more features \\]$")
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = 6, max_nfeat = 10, show_summary = TRUE),
        paste0("^Document-feature matrix of: 5 documents, 5 features \\(4\\.00% sparse\\) and 4 docvars\\.",
               ".*",
               "1789-Washington\\s+3\\s+2\\s+5\\s+71\\s+116")
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = -1, max_nfeat = -1, show_summary = TRUE),
        paste0("^Document-feature matrix of: 5 documents, 5 features \\(4\\.00% sparse\\) and 4 docvars\\.",
               ".*",
               "1805-Jefferson\\s+8\\s+1\\s+10\\s+101\\s+143")
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = 0, max_nfeat = -1, show_summary = TRUE),
        "^Document-feature matrix of: 5 documents, 5 features \\(4\\.00% sparse\\) and 4 docvars\\.$"
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = -1, max_nfeat = 0, show_summary = TRUE),
        paste0("^Document-feature matrix of: 5 documents, 5 features \\(4\\.00% sparse\\) and 4 docvars\\.",
               "\\n",
               "\\[ reached max_nfeat \\.\\.\\. 5 more features ]$")
    )
    expect_output(
        print(dfmt, max_ndoc = 6, max_nfeat = 10, show_summary = FALSE),
        paste0("^\\s+features",
               ".*",
               "\\[ reached max_ndoc \\.\\.\\. 8 more documents, reached max_nfeat \\.\\.\\. 4,442 more features \\]$")
    )
    expect_error(print(dfmt, max_ndoc = -2),
                 "The value of max_ndoc must be between -1 and Inf")
    expect_error(print(dfmt, max_nfeat = -2),
                 "The value of max_nfeat must be between -1 and Inf")
})


test_that("dfm verbose option prints correctly", {
    
    txt <- c(d1 = "a b c d e", d2 = "a a b c c c")
    corp <- corpus(txt)
    toks <- tokens(txt)
    xtoks <- as.tokens_xptr(toks)
    expect_message(dfm(toks, verbose = TRUE), 
                   "Creating a dfm from a tokens object")
    expect_message(dfm(xtoks, verbose = TRUE), 
                   "Creating a dfm from a tokens_xptr object")
})

test_that("dfm works with purrr::map (#928)", {
    skip_if_not_installed("purrr")
    toks1 <- tokens("a b")
    toks2 <- tokens("a a a b b")
    suppressWarnings(expect_identical(
        vapply(purrr::map(list(toks1, toks2), dfm), is.dfm, logical(1)),
        c(TRUE, TRUE)
    ))
    expect_identical(
        vapply(purrr::map(list(dfm(toks1), dfm(toks2)), dfm), is.dfm, logical(1)),
        c(TRUE, TRUE)
    )
})

test_that("dfm works when features are created (#946", {
    dfm1 <- as.dfm(matrix(1:6, nrow = 2,
                          dimnames = list(c("doc1", "doc2"), c("a", "b", "c"))))
    dfm2 <- as.dfm(matrix(1:6, nrow = 2,
                          dimnames = list(c("doc1", "doc2"), c("b", "c", "feat_2"))))

    expect_equal(
        as.matrix(dfm_match(dfm1, featnames(dfm2))),
        matrix(c(3, 4, 5, 6, 0, 0), nrow = 2,
               dimnames = list(docs = c("doc1", "doc2"), features = c("b", "c", "feat_2")))
    )

    expect_equal(
        as.matrix(cbind(dfm(tokens("a b")), dfm(tokens("feat_1")))),
        matrix(c(1, 1, 1), nrow = 1, dimnames = list(docs = "text1", features = c("a", "b", "feat_1")))
    )
})

test_that("dfm warns of argument not used", {
    
    txt <- c(d1 = "a b c d e", d2 = "a a b c c c")
    toks <- tokens(txt)
    xtoks <- as.tokens_xptr(toks)
    dfmat <- dfm(toks)

    expect_warning(dfm(toks, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")
    expect_warning(dfm(xtoks, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")
    expect_warning(dfm(dfmat, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")
})


test_that("test topfeatures", {
    expect_identical(
        topfeatures(dfm(tokens("a a a a b b b c c d")), scheme = "count"),
        c(a = 4, b = 3, c = 2, d = 1)
    )
    expect_error(
        topfeatures(dfm(tokens("a a a a b b b c c d")), "count"),
        "n must be a number"
    )
    dfmat <- corpus(c("a b b c", "b d", "b c"), 
                    docvars = data.frame(numdv = c(1, 2, 1))) |>
        tokens() |>
        dfm()
    expect_identical(
        topfeatures(dfmat, groups = numdv),
        list("1" = c(b = 3, c = 2, a = 1, d = 0),
             "2" = c(b = 1, d = 1, a = 0, c = 0))
    )
    expect_identical(
        topfeatures(dfmat, scheme = "docfreq"),
        c(b = 3L, c = 2L, a = 1L, d = 1L)
    )
    expect_identical(
        topfeatures(dfm_weight(dfmat, scheme = "prop"), groups = numdv),
        list("1" = c(b = 1.00, c = 0.75, a = 0.25, d = 0.00),
             "2" = c(b = 0.5, d = 0.5, a = 0, c = 0))
    )
    lifecycle::expect_defunct(
        topfeatures(fcm(dfmat))
    )
})

test_that("test sparsity", {
    expect_equal(
        sparsity(dfm(tokens(c("a a a a  c c d", "b b b")))),
        0.5
    )
})

test_that("test null dfm is handled properly", {
    mx <- quanteda:::make_null_dfm()

    # constructor
    expect_equal(dfm(mx), mx)

    # selection and grouping
    expect_equal(dfm_select(mx), mx)
    expect_equal(dfm_select(mx, "a"), mx)
    expect_equal(dfm_trim(mx), mx)
    expect_equal(dfm_sample(mx), mx)
    expect_equal(dfm_subset(mx), mx)
    expect_equal(dfm_compress(mx, "both"), mx)
    expect_equal(dfm_compress(mx, "features"), mx)
    expect_equal(dfm_compress(mx, "documents"), mx)
    expect_equal(dfm_sort(mx, margin = "both"), mx)
    expect_equal(dfm_sort(mx, margin = "features"), mx)
    expect_equal(dfm_sort(mx, margin = "documents"), mx)
    expect_equal(dfm_lookup(mx, dictionary(list(A = "a"))), mx)
    expect_equal(dfm_group(mx), mx)
    expect_equal(dfm_replace(mx, "A", "a"), mx)
    expect_equal(head(mx), mx)
    expect_equal(tail(mx), mx)

    # weighting
    expect_equal(topfeatures(mx), numeric())
    expect_equal(dfm_weight(mx, "count"), mx)
    expect_equal(dfm_weight(mx, "prop"), mx)
    expect_equal(dfm_weight(mx, "propmax"), mx)
    expect_equal(dfm_weight(mx, "logcount"), mx)
    expect_equal(dfm_weight(mx), mx)
    expect_equal(dfm_weight(mx, "augmented"), mx)
    expect_equal(dfm_weight(mx, "boolean"), mx)
    expect_equal(dfm_weight(mx, "logave"), mx)
    expect_equal(dfm_tfidf(mx), mx)
    expect_equal(docfreq(mx), numeric())
    expect_equal(dfm_smooth(mx), mx)

    # transformation
    expect_equal(dfm_tolower(mx), mx)
    expect_equal(dfm_toupper(mx), mx)
    expect_equal(dfm_wordstem(mx), mx)

    # binding
    expect_equal(rbind(mx, mx), mx)
    expect_equal(cbind(mx, mx), mx)

    expect_output(print(mx),
                  "Document-feature matrix of: 0 documents, 0 features (0.00% sparse) and 0 docvars.", fixed = TRUE)
})

test_that("test empty dfm is handled properly (#1419)", {
    mx <- dfm_trim(data_dfm_lbgexample, 1000)
    docvars(mx) <- data.frame(var = c(1, 5, 3, 6, 6, 4))

    # constructor
    expect_equal(dfm(mx), mx)

    # selection and grouping
    expect_equal(dfm_select(mx), mx)
    expect_equal(dfm_select(mx, "a"), mx)
    expect_equal(dfm_trim(mx), mx)
    expect_equal(ndoc(dfm_sample(mx)), ndoc(mx))
    expect_equal(dfm_subset(mx, var > 5), mx[4:5, ])
    expect_equal(dfm_compress(mx, "both"), mx)
    expect_equal(dfm_compress(mx, "features"), mx)
    expect_equal(dfm_compress(mx, "documents"), mx)
    expect_equal(dfm_sort(mx, margin = "both"), mx)
    expect_equal(dfm_sort(mx, margin = "features"), mx)
    expect_equal(dfm_sort(mx, margin = "documents"), mx)
    expect_equal(dfm_lookup(mx, dictionary(list(A = "a"))), mx)
    expect_equal(dfm_group(mx), mx)
    expect_equal(dfm_replace(mx, "A", "a"), mx)
    expect_equal(head(mx), mx)
    expect_equal(tail(mx), mx)

    # weighting
    expect_equal(topfeatures(mx), numeric())
    expect_equal(dfm_weight(mx, "count"), mx)
    expect_equal(dfm_weight(mx, "prop"), mx)
    expect_equal(dfm_weight(mx, "propmax"), mx)
    expect_equal(dfm_weight(mx, "logcount"), mx)
    expect_equal(dfm_weight(mx), mx)
    expect_equal(dfm_weight(mx, "augmented"), mx)
    expect_equal(dfm_weight(mx, "boolean"), mx)
    expect_equal(dfm_weight(mx, "logave"), mx)
    expect_equal(dfm_tfidf(mx), mx)
    expect_equal(docfreq(mx), numeric())
    expect_equal(dfm_smooth(mx), mx)

    # transformation
    expect_equal(dfm_tolower(mx), mx)
    expect_equal(dfm_toupper(mx), mx)
    expect_equal(dfm_wordstem(mx), mx)

    # binding
    expect_equal(ndoc(rbind(mx, mx)), ndoc(mx) * 2)
    expect_equal(ndoc(cbind(mx, mx)), ndoc(mx))

    expect_output(print(mx),
                  "Document-feature matrix of: 6 documents, 0 features (0.00% sparse) and 1 docvar.", fixed = TRUE)
})

test_that("dfm raise nicer error message, #1267", {
    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    mx <- dfm(tokens(txt))
    expect_error(mx["d4"], "Subscript out of bounds")
    expect_error(mx["d4", ], "Subscript out of bounds")
    expect_error(mx[4], "Subscript out of bounds")
    expect_error(mx[4, ], "Subscript out of bounds")
    expect_error(mx["d4", , TRUE], "Subscript out of bounds")
    expect_error(mx[4, , TRUE], "Subscript out of bounds")
    expect_error(mx[1:4, , TRUE], "Subscript out of bounds")
    expect_error(mx[1:4, , TRUE], "Subscript out of bounds")

    expect_error(mx["five"], "Subscript out of bounds")
    expect_error(mx[, "five"], "Subscript out of bounds")
    expect_error(mx[5], "Subscript out of bounds")
    expect_error(mx[, 5], "Subscript out of bounds")
    expect_error(mx[, 1:5], "Subscript out of bounds")
    expect_error(mx["d4", "five"], "Subscript out of bounds")
    expect_error(mx[, "five", TRUE], "Subscript out of bounds")
    expect_error(mx[, 5, TRUE], "Subscript out of bounds")
    expect_error(mx[, 1:5, TRUE], "Subscript out of bounds")
    expect_error(mx["d4", "five", TRUE], "Subscript out of bounds")

    expect_error(mx[4, 5], "Subscript out of bounds")
    expect_error(mx[4:5], "Subscript out of bounds")
    expect_error(mx[1:4, 1:5], "Subscript out of bounds")
    expect_error(mx[4, 5, TRUE], "Subscript out of bounds")
    expect_error(mx[1:4, 1:5, TRUE], "Subscript out of bounds")

})

test_that("dfm keeps non-existent types, #1278", {
    toks <- tokens("a b c")
    dict <- dictionary(list(A = "a", B = "b", Z = "z"))

    toks_key <- tokens_lookup(toks, dict)
    expect_equal(types(toks_key), c("A", "B", "Z"))

    expect_equal(featnames(dfm(toks_key, tolower = TRUE)),
                 c("a", "b", "z"))

    expect_equal(featnames(dfm(toks_key, tolower = FALSE)),
                 c("A", "B", "Z"))

})

test_that("arithmetic/linear operation works with dfm", {
    mt <- dfm(tokens(c(d1 = "a a b", d2 = "a b b c", d3 = "c c d")))
    expect_true(is.dfm(mt + 2))
    expect_true(is.dfm(mt - 2))
    expect_true(is.dfm(mt * 2))
    expect_true(is.dfm(mt / 2))
    expect_true(is.dfm(mt ^ 2))
    expect_true(is.dfm(2 + mt))
    expect_true(is.dfm(2 - mt))
    expect_true(is.dfm(2 * mt))
    expect_true(is.dfm(2 / mt))
    expect_true(is.dfm(2 ^ mt))
    expect_true(is.dfm(t(mt)))
    expect_equal(rowSums(mt), colSums(t(mt)))
})

test_that("rbind and cbind wokrs with empty dfm", {
    mt <- dfm(tokens(c(d1 = "a a b", d2 = "a b b c", d3 = "c c d")))

    expect_identical(docnames(rbind(mt, quanteda:::make_null_dfm())),
                     docnames(mt))
    expect_identical(docnames(mt),
                     docnames(rbind(mt, quanteda:::make_null_dfm())))

    expect_identical(docnames(cbind(mt, quanteda:::make_null_dfm())),
                     docnames(mt))
    expect_identical(docnames(mt),
                     docnames(cbind(mt, quanteda:::make_null_dfm())))
})

test_that("format_sparsity works correctly", {
    expect_error(
        quanteda:::format_sparsity(-1),
        "The value of x must be between 0 and 1"
    )
    expect_identical(
        quanteda:::format_sparsity(sparsity(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.5)))),
        "50.00%"
    )
    expect_identical(
        quanteda:::format_sparsity(sparsity(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.1)))),
        "90.00%"
    )
    expect_identical(
        quanteda:::format_sparsity(sparsity(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.99)))),
        "1.00%"
    )
    expect_identical(quanteda:::format_sparsity(1.0), "100.00%")
    expect_identical(quanteda:::format_sparsity(0.9999), "99.99%")
    expect_identical(quanteda:::format_sparsity(0.99991), ">99.99%")
    expect_identical(quanteda:::format_sparsity(0.0001), "0.01%")
    expect_identical(quanteda:::format_sparsity(0.00001), "<0.01%")
    expect_identical(quanteda:::format_sparsity(0.00011), "0.01%")
    expect_identical(quanteda:::format_sparsity(0.0), "0.00%")
    expect_identical(quanteda:::format_sparsity(NA), "0.00%")
})

test_that("unused argument warning only happens only once (#1509)", {
    expect_warning(
        dfm(tokens("some text"), NOTARG = TRUE),
        "^NOTARG argument is not used\\.$"
    )
    expect_warning(
        dfm(tokens("some text"), NOTARG = TRUE),
        "^NOTARG argument is not used\\.$"
    )
    expect_warning(
        dfm(tokens("some text"), NOTARG = TRUE, NOTARG2 = FALSE),
        "^NOTARG, NOTARG2 arguments are not used\\.$"
    )
})

test_that("dimnames are always character vectors", {
    mt <- data_dfm_lbgexample
    expect_identical(dimnames(mt[, character()]),
                     list(docs = rownames(mt), features = character()))
    expect_identical(dimnames(mt[, FALSE]),
                     list(docs = rownames(mt), features = character()))
    expect_identical(dimnames(mt[character(), ]),
                     list(docs = character(), features = colnames(mt)))
    expect_identical(dimnames(mt[FALSE, ]),
                     list(docs = character(), features = colnames(mt)))
})

test_that("set_dfm_dimnames etc functions work", {
    x <- dfm(tokens(c("a a b b c", "b b b c")))

    quanteda:::set_dfm_featnames(x) <- paste0("feature", 1:3)
    expect_identical(featnames(x), c("feature1", "feature2", "feature3"))

    quanteda:::set_dfm_docnames(x) <- paste0("DOC", 1:2)
    expect_identical(docnames(x), c("DOC1", "DOC2"))

    quanteda:::set_dfm_dimnames(x) <- list(c("docA", "docB"), LETTERS[1:3])
    expect_identical(docnames(x), c("docA", "docB"))
    expect_identical(featnames(x), c("A", "B", "C"))
})

test_that("dfm feature and document names have encoding", {
    mt <- dfm(tokens(c("文書１" = "あ い い う", "文書２" = "え え え お")))
    expect_true(all(Encoding(colnames(mt)) == "UTF-8"))
    #expect_true(all(Encoding(rownames(mt)) == "UTF-8")) fix in new corpus

    mt1 <- dfm_sort(mt)
    expect_true(all(Encoding(colnames(mt1)) == "UTF-8"))
    #expect_true(all(Encoding(rownames(mt1)) == "UTF-8")) fix in new corpus

    mt2 <- dfm_group(mt, c("文書３", "文書３"))
    expect_true(all(Encoding(colnames(mt2)) == "UTF-8"))
    #expect_true(all(Encoding(rownames(mt2)) == "UTF-8")) fix in new corpus

    mt3 <- dfm_remove(mt, c("あ"))
    expect_true(all(Encoding(colnames(mt3)) == "UTF-8"))
    #expect_true(all(Encoding(rownames(mt3)) == "UTF-8")) fix in new corpus

    mt4 <- dfm_trim(mt, min_termfreq = 2)
    expect_true(all(Encoding(colnames(mt4)) == "UTF-8"))
    #expect_true(all(Encoding(rownames(mt4)) == "UTF-8")) fix in new corpus
})

test_that("dfm verbose = TRUE works as expected", {
    skip("the verbose message has been changed")
    expect_message(
        tmp <- suppressWarnings(dfm(data_corpus_inaugural[1:3], verbose = TRUE)),
        "Creating a dfm from a corpus input"
    )
    expect_message(
        tmp <- dfm(tokens(data_corpus_inaugural[1:3]), verbose = TRUE),
        "Finished constructing a 3 x 1,\\d{3} sparse dfm"
    )
    dict <- dictionary(list(pos = "good", neg = "bad", neg_pos = "not good", neg_neg = "not bad"))
    expect_message(
        tmp <- suppressWarnings(dfm(tokens(data_corpus_inaugural[1:3]), dictionary = dict, verbose = TRUE)),
        "applying a dictionary consisting of 4 keys"
    )
    expect_message(
        tmp <- suppressWarnings(dfm(dfm(tokens(data_corpus_inaugural[1:3])), dictionary = dict, verbose = TRUE)),
        "applying a dictionary consisting of 4 keys"
    )
    expect_message(
        tmp <- suppressWarnings(dfm(tokens(data_corpus_inaugural[1:3]), 
                                    groups = data_corpus_inaugural$President[1:3], 
                                    verbose = TRUE)),
        "grouping texts"
    )
    expect_message(
        tmp <- suppressWarnings(dfm(tokens(data_corpus_inaugural[1:2]), stem = TRUE, verbose = TRUE)),
        "stemming types \\(English\\)"
    )
    expect_message(
        tmp <- suppressWarnings(dfm(dfm(tokens(data_corpus_inaugural[1:2])), stem = TRUE, verbose = TRUE)),
        "stemming features \\(English\\)"
    )
    expect_message(
        tmp <- suppressWarnings(dfm(dfm(tokens(data_corpus_inaugural[1:3])), 
                                    groups = data_corpus_inaugural$President[1:3], 
                                    verbose = TRUE)),
        "grouping texts"
    )
    expect_error(
        dfm(tokens("one two three"), remove = "one", select = "three"),
        "only one of select and remove may be supplied at once"
    )

    toks <- tokens(c("one two", "two three four"))
    attributes(toks)$types[4] <- NA
    dfm(toks)
})

test_that("dfm_sort works as expected", {
    dfmat <- dfm(tokens(c(d1 = "z z x y a b", d3 = "x y y y c", d2 = "a z")))
    expect_identical(
        featnames(dfm_sort(dfmat, margin = "features", decreasing = TRUE)),
        c("y", "z", "x", "a", "b", "c")
    )
    expect_identical(
        featnames(dfm_sort(dfmat, margin = "features", decreasing = FALSE)),
        c("b", "c", "x", "a", "z", "y")
    )
    expect_identical(
        docnames(dfm_sort(dfmat, margin = "documents", decreasing = TRUE)),
        c("d1", "d3", "d2")
    )
    expect_identical(
        docnames(dfm_sort(dfmat, margin = "documents", decreasing = FALSE)),
        rev(c("d1", "d3", "d2"))
    )
})

test_that("test dfm transpose for #1903", {
    dfmat <- dfm(tokens(c(d1 = "one two three", d2 = "two two three")))
    dfmat_t <- t(dfmat)
    expect_equal(
        names(dimnames(dfmat_t)),
        c("features", "docs")
    )
    expect_equal(
        docnames(dfmat_t),
        c("one", "two", "three")
    )
    expect_equal(
        dfmat_t@docvars$docname_,
        c("one", "two", "three")
    )
    expect_equal(
        names(dfmat_t@meta),
        c("system", "object", "user")
    )
})

test_that("remove_padding argument works", {
    txt <- c("a a b b c", "a a b c c d d")
    toks <- tokens(txt) |> tokens_remove("b", padding = TRUE)
    dfmat <- dfm(toks)
    expect_identical(
        featnames(dfm(toks, remove_padding = FALSE)),
        c("", "a", "c", "d")
    )
    expect_identical(
        featnames(dfm(dfmat, remove_padding = TRUE)),
        c("a", "c", "d")
    )
    expect_identical(
        featnames(dfm(dfmat, remove_padding = FALSE)),
        c("", "a", "c", "d")
    )
})


test_that("features of DFM are always in the same order (#2100)", {
    
    toks1 <- quanteda:::build_tokens(list(c(1, 0, 2, 3, 4)), types = c("a", "b", "c", "d"),
                                     padding = TRUE,
                                     docvars = quanteda:::make_docvars(1L))
    toks2 <- quanteda:::build_tokens(list(c(1, 0, 3, 2, 4)), types = c("a", "c", "b", "d"),
                                     padding = TRUE,
                                     docvars = quanteda:::make_docvars(1L))
    toks3 <- quanteda:::build_tokens(list(c(1, 2, 3, 4)), types = c("a", "b", "c", "d"),
                                     padding = FALSE,
                                     docvars = quanteda:::make_docvars(1L))
    dfmat1 <- dfm(toks1)
    dfmat2 <- dfm(toks2)
    dfmat3 <- dfm(toks3)
    
    expect_identical(c("", "a", "b", "c", "d"), featnames(dfmat1))
    expect_identical(c("", "a", "b", "c", "d"), featnames(dfmat2))
    expect_identical(c("a", "b", "c", "d"), featnames(dfmat3))
    
})

test_that("dfm works with no-breaking space (#2407)", {
    
    toks <- tokens("A a b \n\Ufeff", 
                   padding = TRUE, xptr = FALSE)
    expect_equal(
        as.matrix(dfm(toks, tolower = TRUE)),
        matrix(c(2, 1), nrow = 1, 
               dimnames = list(docs = "text1", features = c("a", "b"))
        )
    )
    
    expect_equal(
        as.matrix(dfm(toks, tolower = FALSE)),
        matrix(c(1, 1, 1), nrow = 1, 
               dimnames = list(docs = "text1", features = c("A", "a", "b"))
        )
    )
    
    toks2 <- tokens("AAA \Ufeff aaa \Ufeff\Ufeff \Ufe00 \U2066 \U0001f600")
    
    expect_equal(
        as.matrix(dfm(toks2, tolower = TRUE)),
        matrix(c(2, 1), nrow = 1, 
               dimnames = list(docs = "text1", features = c("aaa", "\U0001f600"))
        )
    )
    
    expect_equal(
        as.matrix(dfm(toks2, tolower = FALSE)),
        matrix(c(1, 1, 1), nrow = 1, 
               dimnames = list(docs = "text1", features = c("AAA", "aaa", "\U0001f600"))
        )
    )
})
