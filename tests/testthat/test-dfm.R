context("test dfm")

test_that("test c.corpus", {
    expect_equal(
        matrix(dfm(corpus(c("What does the fox say?", "What does the fox say?", "")),
                   remove_punct = TRUE)),
        matrix(rep(c(1, 1, 0), 5), nrow = 15, ncol = 1)
    )
})

## rbind.dfm
## TODO: Test classes returned

test_that("test rbind.dfm with the same columns", {

    fox <- "What does the fox say?"
    foxdfm <- rep(1, 20)
    dim(foxdfm) <- c(4, 5)
    colnames(foxdfm) <- c("does", "fox", "say", "the", "what")
    rownames(foxdfm) <-  rep(c("text1", "text2"), 2)

    dfm1 <- dfm(c(fox, fox), remove_punct = TRUE)

    expect_true(
        all(rbind(dfm1, dfm1) == foxdfm)
    )
    expect_that(
        rbind(dfm1, dfm1),
        is_a("dfm")
    )

})

# TODO: Add function for testing the equality of dfms

test_that("test rbind.dfm with different columns", {
    dfmt1 <- dfm(c(text1 = "What does the fox?"), remove_punct = TRUE)
    dfmt2 <- dfm(c(text2 = "fox say"), remove_punct = TRUE)
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

    dfmt1 <- dfm("What does the?", remove_punct = TRUE)
    dfmt2 <- dfm("fox say fox", remove_punct = TRUE)
    dfmt3 <- dfm("The quick brown fox", remove_punct = TRUE)
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
            rbind(dfm(fox)) == dfm(fox)
        )
    )
    expect_that(
        rbind(dfm(fox, remove_punct = TRUE)),
        is_a("dfm")
    )
})

test_that("test rbind.dfm with the same features, but in a different order", {

    fox <- "What does the fox say?"
    xof <- "say fox the does What??"
    foxdfm <- rep(1, 20)
    dim(foxdfm) <- c(4, 5)
    colnames(foxdfm) <- c("does", "fox", "say", "the", "what")
    rownames(foxdfm) <-  rep(c("text1", "text2"), 2)

    dfm1 <- dfm(c(fox, xof), remove_punct = TRUE)

    expect_true(
        all(rbind(dfm1, dfm1) == foxdfm)
    )
})

test_that("dfm keeps all types with > 10,000 documents (#438) (a)", {
    generate_testdfm <- function(n) {
        dfm(paste("X", seq_len(n), sep = ""))
    }
    expect_equal(nfeat(generate_testdfm(10000)), 10000)
    expect_equal(nfeat(generate_testdfm(20000)), 20000)
})

test_that("dfm keeps all types with > 10,000 documents (#438) (b)", {
    set.seed(10)
    generate_testdfm <- function(n) {
        dfm(paste(sample(letters, n, replace = TRUE), 1:n))
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

    expect_true({
        sum(dfm(corp, select = c("The", "a", "an"))) >
        sum(dfm(corp, select = c("The", "a", "an"), case_insensitive = FALSE))
    })

    expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = FALSE, tolower = FALSE),
                     dfm_remove(dfmt, c("The", "a", "an"), case_insensitive = FALSE))
    expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = TRUE, tolower = FALSE),
                     dfm_remove(dfmt, c("The", "a", "an"), case_insensitive = TRUE))

    expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = FALSE),
                     dfm(tokens_remove(toks, c("The", "a", "an"), case_insensitive = FALSE)))
    expect_identical(dfm(dfmt, remove = c("The", "a", "an"), case_insensitive = TRUE),
                     dfm(tokens_remove(toks, c("The", "a", "an"), case_insensitive = TRUE)))

    dfmt_group <- dfm(dfmt,
                      groups =  ifelse(docvars(data_corpus_inaugural, "Party") %in%
                                           c("Democratic", "Republican"), "Mainstream", "Minor"),
                      tolower = FALSE)
    expect_identical(colSums(dfmt_group), colSums(dfmt_group))
    expect_identical(docnames(dfmt_group), c("Mainstream", "Minor"))

    dict <- dictionary(list(articles = c("The", "a", "an"),
                            preps = c("of", "for", "In")), tolower = FALSE)

    expect_true({
        sum(dfm(corp, dictionary = dict)) >
        sum(dfm(corp, dictionary = dict, case_insensitive = FALSE))
    })

    expect_equivalent(
        dfm(corp, dictionary = dict),
        dfm(dfmt, dictionary = dict)
    )

    expect_equivalent(
        dfm(dfmt, dictionary = dict),
        dfm(tokens_lookup(toks, dict))
    )

    expect_equivalent(
        dfm(corp, dictionary = dict, case_insensitive = FALSE),
        dfm(dfmt, dictionary = dict, case_insensitive = FALSE)
    )

    expect_equivalent(
        dfm(dfmt, dictionary = dict, case_insensitive = FALSE),
        dfm(tokens_lookup(toks, dict, case_insensitive = FALSE))
    )

    expect_identical(
        dfm(corp, stem = TRUE),
        dfm(dfmt, stem = TRUE)
    )
    expect_identical(
        dfm(corp, stem = TRUE),
        dfm(dfmt, stem = TRUE)
    )
})

test_that("cbind.dfm works as expected", {
    dfm1 <- dfm("This is one sample text sample")
    dfm2 <- dfm("More words here")
    dfm12 <- cbind(dfm1, dfm2)

    expect_equal(nfeat(dfm12), 8)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("cbind.dfm works with non-dfm objects", {
    dfm1 <- dfm(c("a b c", "c d e"))

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
    mydfm <- dfm(txts)

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
    dfm1 <- dfm("This is one sample text sample")
    dfm2 <- dfm("More words here")
    dfm12 <- rbind(dfm1, dfm2)

    expect_equal(nfeat(dfm12), 8)
    expect_equal(ndoc(dfm12), 2)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("dfm(x, dictionary = mwvdict) works with multi-word values", {
    mwvdict <- dictionary(list(sequence1 = "a b", sequence2 = "x y", notseq = c("d", "e")))
    txt <- c(d1 = "a b c d e f g x y z",
             d2 = "a c d x z",
             d3 = "x y",
             d4 = "f g")

    # as dictionary
    dfm1 <- dfm(txt, dictionary = mwvdict, verbose = TRUE)
    expect_identical(
        as.matrix(dfm1),
        matrix(c(1, 0, 0, 0, 1, 0, 1, 0, 2, 1, 0, 0),
               nrow = 4,
               dimnames = list(docs = paste0("d", 1:4),
                               features = c("sequence1", "sequence2", "notseq")))
    )

    # as thesaurus
    dfm2 <- dfm(txt, thesaurus = mwvdict, verbose = TRUE)
    expect_identical(
        as.matrix(dfm2),
        matrix(c(1, 0, 0, 0, 1, 0, 1, 0, 2, 1, 0, 0,
                 0, 1, 0, 0,  1, 1, 0, 0,  1, 0, 0, 1,  1, 0, 0, 1,  0, 1, 0, 0, 1, 1, 0, 0),
               nrow = 4,
               dimnames = list(docs = paste0("d", 1:4),
                               features = c("SEQUENCE1", "SEQUENCE2", "NOTSEQ",
                                            "a", "c", "f", "g", "x", "z")))
    )
})

test_that("dfm works with relational operators", {
    testdfm <- dfm(c("This is an example.", "This is a second example."))
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
    dfmt <- head(data_dfm_lbgexample, 4, nf = 3)

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

test_that("dfm's document counts in verbose message is correct", {
    txt <- c(d1 = "a b c d e f g x y z",
             d2 = "a c d x z",
             d3 = "x y",
             d4 = "f g")
    expect_message(dfm(txt, remove = c("a", "f"), verbose = TRUE),
                   "removed 2 features")
    expect_message(dfm(txt, select = c("a", "f"), verbose = TRUE),
                   "kept 2 features")
})

test_that("dfm head, tail work as expected", {
    dfmt <- head(data_dfm_lbgexample, 4, nf = 3)
    expect_equal(featnames(dfmt), LETTERS[1:3])
    expect_equal(docnames(dfmt), paste0("R", 1:4))

    dfmt <- head(data_dfm_lbgexample, -4, nf = -30)
    expect_equal(featnames(dfmt), LETTERS[1:7])
    expect_equal(docnames(dfmt), paste0("R", 1:2))

    dfmt <- tail(data_dfm_lbgexample, 4, nf = 3)
    expect_equal(featnames(dfmt), c("ZI", "ZJ", "ZK"))
    expect_equal(docnames(dfmt), c("R3", "R4", "R5", "V1"))

    dfmt <- tail(data_dfm_lbgexample, -4, nf = -34)
    expect_equal(featnames(dfmt), c("ZI", "ZJ", "ZK"))
    expect_equal(docnames(dfmt), c("R5", "V1"))
})

test_that("dfm print works with options as expected", {
    dfmt <- dfm(data_corpus_inaugural[1:14],
                remove_punct = FALSE, remove_numbers = FALSE, split_hyphens = TRUE)
    expect_output(
        print(dfmt, max_ndoc = 6, max_nfeat = 10, show_summary = TRUE),
        paste0("^Document-feature matrix of: 14 documents, 4,452 features \\(82\\.0% sparse\\) and 4 docvars",
               ".*",
               "\\[ reached max_ndoc \\.\\.\\. 8 more documents, reached max_nfeat \\.\\.\\. 4,442 more features \\]$")
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = 6, max_nfeat = 10, show_summary = TRUE),
        paste0("^Document-feature matrix of: 5 documents, 5 features \\(4\\.0% sparse\\) and 4 docvars\\.",
               ".*",
               "1789-Washington\\s+3\\s+2\\s+5\\s+71\\s+116")
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = -1, max_nfeat = -1, show_summary = TRUE),
        paste0("^Document-feature matrix of: 5 documents, 5 features \\(4\\.0% sparse\\) and 4 docvars\\.",
               ".*",
               "1805-Jefferson\\s+8\\s+1\\s+10\\s+101\\s+143")
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = 0, max_nfeat = -1, show_summary = TRUE),
        "^Document-feature matrix of: 5 documents, 5 features \\(4\\.0% sparse\\) and 4 docvars\\.$"
    )
    expect_output(
        print(dfmt[1:5, 1:5], max_ndoc = -1, max_nfeat = 0, show_summary = TRUE),
        paste0("^Document-feature matrix of: 5 documents, 5 features \\(4\\.0% sparse\\) and 4 docvars\\.",
               "\\n",
               "\\[ reached max_nfeat \\.\\.\\. 5 more features ]$")
    )
    expect_output(
        print(dfmt, max_ndoc = 6, max_nfeat = 10, show_summary = FALSE),
        paste0("^\\s+features",
               ".*",
               "\\[ reached max_ndoc \\.\\.\\. 8 more documents, reached max_nfeat \\.\\.\\. 4,442 more features \\]$")
    )
})

test_that("cannot supply remove and select in one call (#793)", {
    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    corp <- corpus(txt, docvars = data.frame(grp = c(1, 1, 2)))
    toks <- tokens(corp)
    expect_error(
        dfm(txt, select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
    expect_error(
        dfm(corp, select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
    expect_error(
        dfm(toks, select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
    expect_error(
        dfm(dfm(toks), select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
})

test_that("dfm with selection options produces correct output", {
    txt <- c(d1 = "a b", d2 = "a b c d e")
    toks <- tokens(txt)
    dfmt <- dfm(toks)
    feat <- c("b", "c", "d", "e", "f", "g")
    expect_message(
        dfm(txt, remove = feat, verbose = TRUE),
        "removed 4 features"
    )
    expect_message(
        dfm(toks, remove = feat, verbose = TRUE),
        "removed 4 features"
    )
    expect_message(
        dfm(dfmt, remove = feat, verbose = TRUE),
        "removed 4 features"
    )
})

test_that("dfm works with stem options", {
    txt_english <- "running ran runs"
    txt_french <- "courant courir cours"

    quanteda_options(language_stemmer = "english")
    expect_equal(
        as.character(tokens_wordstem(tokens(txt_english))),
        c("run", "ran", "run")
    )
    expect_equal(
        featnames(dfm(txt_english)),
        c("running", "ran", "runs")
    )
    expect_equal(
        featnames(dfm(txt_english, stem = TRUE)),
        c("run", "ran")
    )

    quanteda_options(language_stemmer = "french")
    expect_equal(
        as.character(tokens_wordstem(tokens(txt_french))),
        rep("cour", 3)
    )
    expect_equal(
        featnames(dfm(txt_french)),
        c("courant", "courir", "cours")
    )
    expect_equal(
        featnames(dfm(txt_french, stem = TRUE)),
        "cour"
    )
    quanteda_options(reset = TRUE)
})

test_that("dfm verbose option prints correctly", {
    txt <- c(d1 = "a b c d e", d2 = "a a b c c c")
    corp <- corpus(txt)
    toks <- tokens(txt)
    mydfm <- dfm(toks)
    expect_message(dfm(txt, verbose = TRUE), "Creating a dfm from a character input")
    expect_message(dfm(corp, verbose = TRUE), "Creating a dfm from a corpus input")
    expect_message(dfm(toks, verbose = TRUE), "Creating a dfm from a tokens input")
    expect_message(dfm(mydfm, verbose = TRUE), "Creating a dfm from a dfm input")
})

test_that("dfm works with purrr::map (#928)", {
    skip_if_not_installed("purrr")
    a <- "a b"
    b <- "a a a b b"
    expect_identical(
        vapply(purrr::map(list(a, b), dfm), is.dfm, logical(1)),
        c(TRUE, TRUE)
    )
    expect_identical(
        vapply(purrr::map(list(corpus(a), corpus(b)), dfm), is.dfm, logical(1)),
        c(TRUE, TRUE)
    )
    expect_identical(
        vapply(purrr::map(list(tokens(a), tokens(b)), dfm), is.dfm, logical(1)),
        c(TRUE, TRUE)
    )
    expect_identical(
        vapply(purrr::map(list(dfm(a), dfm(b)), dfm), is.dfm, logical(1)),
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
        as.matrix(cbind(dfm("a b"), dfm("feat_1"))),
        matrix(c(1, 1, 1), nrow = 1, dimnames = list(docs = "text1", features = c("a", "b", "feat_1")))
    )
})

test_that("dfm warns of argument not used", {

    txt <- c(d1 = "a b c d e", d2 = "a a b c c c")
    corp <- corpus(txt)
    toks <- tokens(txt)
    mx <- dfm(toks)

    expect_warning(dfm(txt, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")
    expect_warning(dfm(corp, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")
    expect_warning(dfm(toks, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")
    expect_warning(dfm(mx, xxxxx = "something", yyyyy = "else"),
                   "^xxxxx, yyyyy arguments are not used")

})

test_that("dfm pass arguments to tokens, issue #1121", {

    txt <- data_char_sampletext
    corp <- corpus(txt)

    expect_equal(dfm(txt, what = "character"),
                 dfm(tokens(corp, what = "character")))

    expect_equivalent(dfm(txt, what = "character"),
                      dfm(tokens(txt, what = "character")))

    expect_equal(dfm(txt, remove_punct = TRUE),
                 dfm(tokens(corp, remove_punct = TRUE)))

    expect_equivalent(dfm(txt, remove_punct = TRUE),
                      dfm(tokens(txt, remove_punct = TRUE)))

})

test_that("dfm error when a dfm is given to for feature selection when x is not a dfm, #1067", {
    txt <- c(d1 = "a b c d e", d2 = "a a b c c c")
    corp <- corpus(txt)
    toks <- tokens(txt)
    mx <- dfm(toks)
    mx2 <- dfm(c("a b", "c"))

    expect_error(dfm(txt, select = mx2),
                "selection on a dfm is only available when x is a dfm")
    expect_error(dfm(corp, select = mx2),
                "selection on a dfm is only available when x is a dfm")
    expect_error(dfm(toks, select = mx2),
                "selection on a dfm is only available when x is a dfm")
    expect_warning(dfm(mx, select = mx2),
                "pattern = dfm is deprecated")
    expect_equal(
        suppressWarnings(as.matrix(dfm(mx, select = mx2))),
        matrix(c(1, 2, 1, 1, 1, 3), nrow = 2, dimnames = list(docs = c("d1", "d2"), features = letters[1:3]))
    )
})

test_that("test topfeatures", {
    expect_identical(
        topfeatures(dfm("a a a a b b b c c d"), scheme = "count"),
        c(a = 4, b = 3, c = 2, d = 1)
    )
    expect_error(
        topfeatures(dfm("a a a a b b b c c d"), "count"),
        "n must be a number"
    )
    dfmat <- corpus(c("a b b c", "b d", "b c"), docvars = data.frame(numdv = c(1, 2, 1))) %>%
        dfm()
    expect_identical(
        topfeatures(dfmat, groups = "numdv"),
        list("1" = c(b = 3, c = 2, a = 1, d = 0),
             "2" = c(b = 1, d = 1, a = 0, c = 0))
    )
    expect_identical(
        topfeatures(dfmat, scheme = "docfreq"),
        c(b = 3L, c = 2L, a = 1L, d = 1L)
    )
})

test_that("test sparsity", {
    expect_equal(
        sparsity(dfm(c("a a a a  c c d", "b b b"))),
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
    expect_equal(dfm_sort(mx, "both"), mx)
    expect_equal(dfm_sort(mx, "features"), mx)
    expect_equal(dfm_sort(mx, "documents"), mx)
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

    expect_output(print(mx), "Document-feature matrix of: 0 documents, 0 features.")
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
    expect_equal(dfm_sort(mx, "both"), mx)
    expect_equal(dfm_sort(mx, "features"), mx)
    expect_equal(dfm_sort(mx, "documents"), mx)
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

    expect_output(print(mx), "Document-feature matrix of: 6 documents, 0 features.")
})

test_that("dfm raise nicer error message, #1267", {

    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    mx <- dfm(txt)
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

    mt <- dfm(c(d1 = "a a b", d2 = "a b b c", d3 = "c c d"))
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

    mt <- dfm(c(d1 = "a a b", d2 = "a b b c", d3 = "c c d"))

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
        "illegal sparsity value; must be 0 <= x <= 1.0"
    )
    expect_identical(
        quanteda:::format_sparsity(sparsity(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.5)))),
        " (50.0% sparse)"
    )
    expect_identical(
        quanteda:::format_sparsity(sparsity(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.1)))),
        " (90.0% sparse)"
    )
    expect_identical(
        quanteda:::format_sparsity(sparsity(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.99)))),
        " (1.0% sparse)"
    )
    expect_identical(quanteda:::format_sparsity(.9999), " (99.99% sparse)")
    expect_identical(quanteda:::format_sparsity(.99991), " (>99.99% sparse)")
    expect_identical(quanteda:::format_sparsity(.0001), " (0.01% sparse)")
    expect_identical(quanteda:::format_sparsity(.00001), " (<0.01% sparse)")
    expect_identical(quanteda:::format_sparsity(.00011), " (0.011% sparse)")
    expect_identical(quanteda:::format_sparsity(.00011, digits = 3), " (0.011% sparse)")
})

test_that("unused argument warning only happens only once (#1509)", {
    expect_warning(
        dfm("some text", NOTARG = TRUE),
        "^NOTARG argument is not used\\.$"
    )
    expect_warning(
        dfm(corpus("some text"), NOTARG = TRUE),
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

test_that("dfm.tokens() with groups works as expected", {
    x <- tokens(data_corpus_inaugural)
    groupeddfm <- dfm(tokens(x),
                      groups = c("FF", "FF", rep("non-FF", ndoc(x) - 2)))
    expect_equal(ndoc(groupeddfm), 2)
    expect_equal(docnames(groupeddfm), c("FF", "non-FF"))
    expect_equal(featnames(groupeddfm), featnames(dfm(x)))
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
    x <- dfm(c("a a b b c", "b b b c"))

    quanteda:::set_dfm_featnames(x) <- paste0("feature", 1:3)
    expect_identical(featnames(x), c("feature1", "feature2", "feature3"))

    quanteda:::set_dfm_docnames(x) <- paste0("DOC", 1:2)
    expect_identical(docnames(x), c("DOC1", "DOC2"))

    quanteda:::set_dfm_dimnames(x) <- list(c("docA", "docB"), LETTERS[1:3])
    expect_identical(docnames(x), c("docA", "docB"))
    expect_identical(featnames(x), c("A", "B", "C"))
})

test_that("dfm feature and document names have encoding", {
    mt <- dfm(c("文書１" = "あ い い う", "文書２" = "え え え お"))
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
    expect_message(
        tmp <- dfm(data_corpus_inaugural[1:3], verbose = TRUE),
        "Creating a dfm from a corpus input"
    )
    expect_message(
        tmp <- dfm(data_corpus_inaugural[1:3], verbose = TRUE),
        "Finished constructing a 3 x 1,\\d{3} sparse dfm"
    )
    expect_message(
        tmp <- dfm(data_corpus_inaugural[1:3], dictionary = data_dictionary_LSD2015, verbose = TRUE),
        "applying a dictionary consisting of 4 keys"
    )
    expect_message(
        tmp <- dfm(dfm(data_corpus_inaugural[1:3]), dictionary = data_dictionary_LSD2015, verbose = TRUE),
        "applying a dictionary consisting of 4 keys"
    )
    expect_message(
        tmp <- dfm(data_corpus_inaugural[1:3], groups = "President", verbose = TRUE),
        "grouping texts"
    )
    expect_message(
        tmp <- dfm(data_corpus_inaugural[1:2], stem = TRUE, verbose = TRUE),
        "stemming types \\(English\\)"
    )
    expect_message(
        tmp <- dfm(dfm(data_corpus_inaugural[1:2]), stem = TRUE, verbose = TRUE),
        "stemming features \\(English\\)"
    )
    expect_message(
        tmp <- dfm(dfm(data_corpus_inaugural[1:3]), groups = "President", verbose = TRUE),
        "grouping texts"
    )
    expect_error(
        dfm("one two three", remove = "one", select = "three"),
        "only one of select and remove may be supplied at once"
    )

    toks <- tokens(c("one two", "two three four"))
    attributes(toks)$types[4] <- NA
    dfm(toks)
})

test_that("dfm_sort works as expected", {
    dfmat <- dfm(c(d1 = "z z x y a b", d3 = "x y y y c", d2 = "a z"))
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
    dfmat <- dfm(c(d1 = "one two three", d2 = "two two three"))
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
