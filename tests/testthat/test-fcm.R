context("testing fcm")

test_that("compare the output feature co-occurrence matrix to that of the text2vec package", {
    skip_if_not_installed("text2vec")
    library("text2vec")

    txt <- "A D A C E A D F E B A C E D"
    tokens <- txt %>% tolower %>% word_tokenizer
    it <- itoken(tokens)
    v <- create_vocabulary(it)
    vectorizer <- vocab_vectorizer(v)
    tcm <- create_tcm(itoken(tokens), vectorizer, skip_grams_window = 3L)

    # convert to a symmetric matrix to facilitate the sorting
    tcm <- as.matrix(tcm)
    ttcm <- tcm
    # diag(ttcm) <- 0
    tcm <- tcm + t(ttcm)

    # sort the matrix according to rowname-colname and convert back to a upper triangle matrix
    tcm <- tcm[order(rownames(tcm)), order(colnames(tcm))]
    tcm[lower.tri(tcm, diag = FALSE)] <- 0

    toks <- tokens(char_tolower(txt), remove_punct = TRUE)
    fcm <- fcm(toks, context = "window", count = "weighted", weights = 1 / seq_len(3),
               window = 3)
    fcm <- fcm_sort(fcm)

    expect_equivalent(as.matrix(fcm), tcm, tol = .00001)
})

test_that("fcm works with character and tokens in the same way", {
    txt <- "A D A C E A D F E B A C E D"
    fcmt_char <- fcm(txt, context = "window", count = "weighted",
                    weights = c(3, 2, 1), window = 3)
    toks <- tokens(txt)
    fcmt_toks <- fcm(toks, context = "window", count = "weighted",
                    weights = c(3, 2, 1), window = 3)
    expect_equivalent(round(as.matrix(fcmt_char), 2),
                      round(as.matrix(fcmt_toks), 2))
})

# Testing weighting function

test_that("not weighted", {
    txt <- "A D A C E A D F E B A C E D"
    fcmt <- fcm(txt, context = "window", window = 3)

    mat <- matrix(c(4, 1, 4, 4, 5, 2,
                     0, 0, 1, 1, 2, 1,
                     0, 0, 0, 3, 3, 0,
                     0, 0, 0, 0, 4, 1,
                     0, 0, 0, 0, 0, 2,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    fcmt <- fcm_sort(fcmt)
    expect_equivalent(as.matrix(fcmt), mat)
})

test_that("weighted by default", {
    txt <- "A D A C E A D F E B A C E D"
    fcmt <- fcm(txt, context = "window", count = "weighted", window = 3)

    mat <- matrix(c(1.67, 1, 2.83, 3.33, 2.83, 0.83,
                     0, 0, 0.5, 0.33, 1.33, 0.50,
                     0, 0, 0, 1.33, 2.33, 0,
                     0, 0, 0, 0, 2.33, 1.00,
                     0, 0, 0, 0, 0, 1.33,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    fcmt <- fcm_sort(fcmt)
    expect_equivalent(mat, round(as.matrix(fcmt), 2))
})

test_that("customized weighting function", {
    txt <- "A D A C E A D F E B A C E D"
    fcmt <- fcm(txt, context = "window", count = "weighted", weights = c(3, 2, 1), window = 3)

    mat <- matrix(c(6, 3, 9, 10, 10, 3,
                     0, 0, 2, 1, 4, 2,
                     0, 0, 0, 5, 7, 0,
                     0, 0, 0, 0, 8, 3,
                     0, 0, 0, 0, 0, 4,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    fcmt <- fcm_sort(fcmt)
    expect_equivalent(mat, round(as.matrix(fcmt), 2))
})

test_that("ordered setting: window", {
    txt <- "A D A C E A D F E B A C E D"
    fcmat <- fcm(txt, context = "window", window = 3, ordered = TRUE, tri = FALSE)
    fcmat <- fcm_sort(fcmat)
    mat <- matrix(c(2, 0, 3, 3, 3, 1,
                     1, 0, 1, 0, 1, 0,
                     1, 0, 0, 2, 2, 0,
                     1, 1, 1, 0, 2, 1,
                     2, 1, 1, 2, 0, 1,
                     1, 1, 0, 0, 1, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcmat, 2) == round(mat, 2)))
    expect_true(fcmat@ordered)

    fcmat_nord <- fcm(txt, context = "window", window = 3, ordered = FALSE, tri = FALSE)
    fcmat_nord <- fcm_sort(fcmat_nord)
    mat <- matrix(c(4, 1, 4, 4, 5, 2,
                     1, 0, 1, 1, 2, 1,
                     4, 1, 0, 3, 3, 0,
                     4, 1, 3, 0, 4, 1,
                     5, 2, 3, 4, 0, 2,
                     2, 1, 0, 1, 2, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcmat_nord, 2) == round(mat, 2)))
    expect_false(fcmat_nord@ordered)
})

test_that("ordered setting: boolean", {
    txt <- c("b a b c", "a a c b e", "a c e f g")
    fcm <- fcm(txt, context = "window", count = "boolean", window = 2,
               ordered = TRUE, tri = TRUE)
    fcm <- fcm_sort(fcm)
    mat <- matrix(c(1, 2, 3, 1, 0, 0,
                     1, 1, 1, 1, 0, 0,
                     0, 1, 0, 2, 1, 0,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(mat, as.matrix(fcm))

    fcm <- fcm(txt, context = "window", count = "boolean", window = 2,
               ordered = FALSE, tri = TRUE)
    fcm <- fcm_sort(fcm)
    mat <- matrix(c(2, 2, 3, 1, 0, 0,
                     0, 2, 2, 1, 0, 0,
                     0, 0, 0, 2, 1, 0,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(mat, as.matrix(fcm))
})

test_that("window = 2", {
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    fcm <- fcm(txt, context = "window", count = "boolean", window = 2)
    mat <- matrix(c(4, 1, 2, 2, 0, 0,
                   0, 2, 1, 0, 0, 0,
                   0, 0, 0, 2, 1, 0,
                   0, 0, 0, 0, 1, 1,
                   0, 0, 0, 0, 0, 1,
                   0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    fcm <- fcm_sort(fcm)
    expect_equivalent(mat, as.matrix(fcm))
})

test_that("window = 3", {
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    fcm <- fcm(txt, context = "window", count = "boolean", window = 3)
    fcm <- fcm_sort(fcm)
    mat <- matrix(c(4, 1, 3, 2, 1, 0,
                    0, 2, 1, 0, 0, 0,
                    0, 0, 0, 2, 1, 1,
                    0, 0, 0, 0, 1, 1,
                    0, 0, 0, 0, 0, 1,
                    0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(mat, as.matrix(fcm))
})

test_that("fcm.dfm works same as fcm.tokens", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(char_tolower(txt), remove_punct = TRUE)
    dfmat <- dfm(toks)
    expect_equal(fcm(toks, context = "document"),
                 fcm(dfmat, context = "document"))

    fcmat <- fcm(dfm_weight(dfmat, scheme = "boolean"))
    expect_equal(as.vector(fcmat[1, ]), c(0, 1, 1, 2, 2, 1, 1, 2, 1, 1))
})

test_that("fcm.dfm only works for context = \"document\"", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(char_tolower(txt), remove_punct = TRUE)
    expect_error(fcm(dfm(toks), context = "window"),
                 "fcm.dfm only works on context = \"document\"")
})

test_that("fcm.dfm does works for context = \"document\" with weighted counts", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(char_tolower(txt), remove_punct = TRUE)
    expect_error(fcm(dfm(toks), context = "document", count = "weighted"),
                 "Cannot have weighted counts with context = \"document\"")
})

test_that("fcm works as expected for tokens_hashed", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(char_tolower(txt), remove_punct = TRUE)
    toksh <- tokens(char_tolower(txt), remove_punct = TRUE)
    classic <- fcm(toks, context = "window", window = 3)
    hashed <- fcm(toksh, context = "window", window = 3)
    expect_equivalent(classic, hashed)
})

test_that("fcm print works as expected", {
    dfmt <- dfm(data_corpus_inaugural[1:2],
                remove_punct = FALSE, remove_numbers = FALSE, split_hyphens = TRUE)
    fcmt <- fcm(dfmt)
    expect_output(print(fcmt, max_nfeat = 6, show_summary = TRUE),
                  paste0("^Feature co-occurrence matrix of: 634 by 634 features\\.",
                         ".*",
                         "\\[ reached max_feat \\.\\.\\. 628 more features, reached max_nfeat \\.\\.\\. 628 more features \\]$")
    )
    expect_output(print(fcmt[1:5, 1:5], max_nfeat = 6, show_summary = TRUE),
                  paste0("^Feature co-occurrence matrix of: 5 by 5 features\\.",
                         ".*",
                         "fellow\\s+3\\s+6\\s+16\\s+224\\s+361")
    )
    expect_output(print(fcmt[1:10, 1:2], max_nfeat = 6, show_summary = TRUE),
                  paste0("^Feature co-occurrence matrix of: 10 by 2 features\\.",
                         ".*",
                         "\\[ reached max_feat \\.\\.\\. 4 more features \\]$")
    )
    expect_output(print(fcmt[1:5, 1:5], max_nfeat = -1, show_summary = TRUE),
                  paste0("^Feature co-occurrence matrix of: 5 by 5 features\\.",
                         ".*",
                         "the\\s+0\\s+0\\s+0\\s+0\\s+6748$")
    )
    expect_output(print(fcmt[1:10, 1:2], max_nfeat = -1, show_summary = TRUE),
                  paste0("^Feature co-occurrence matrix of: 10 by 2 features\\.",
                         ".*",
                         ":\\s+0\\s+0$")
    )
    expect_output(print(fcmt, max_nfeat = 6, show_summary = FALSE),
                  paste0("^\\s+features",
                         ".*",
                         "\\[ reached max_feat \\.\\.\\. 628 more features, reached max_nfeat \\.\\.\\. 628 more features \\]$")
    )
})

test_that("fcm works the same for different object types", {
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    expect_equivalent(fcm(txt), fcm(corpus(txt)))
    expect_equivalent(fcm(tokens(txt)), fcm(corpus(txt)))
    expect_identical(fcm(txt), fcm(tokens(txt)))
})

test_that("fcm expects error for wrong weight or window", {
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    toks <- tokens(txt)
    expect_error(fcm(toks, context = "window", window = 0),
                "window size must be at least 1")
    expect_error(fcm(toks, context = "window", window = 2, weight = 0.1,
                     count = "weighted", weights = c(1, 2, 3)),
                 "weights length must be equal to the window size")
    expect_error(fcm(toks, context = "window", window = 2, weight = c(0.1, 0.2, 0.3),
                     count = "weighted", weights = c(1, 2, 3)),
                 "weights length must be equal to the window size")
})


test_that("fcm works tokens with paddings, #788", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(txt, remove_punct = TRUE)
    toks <- tokens_remove(toks, pattern = stopwords(), padding = TRUE)
    testfcm <- fcm(toks, context = "window", window = 3)
    expect_equal(sort(colnames(testfcm)), sort(attr(toks, "types")))
})

test_that("as.network.fcm works", {
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    mat <- fcm(txt)
    net <- as.network(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(network::is.network(net))
    expect_identical(network::network.vertex.names(net), featnames(mat))
    expect_identical(network::get.vertex.attribute(net, "frequency"), unname(mat@margin))
    expect_silent(as.network(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("as.network.fcm works with window", {
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    mat <- fcm(txt, contex = "window", window = 2)
    net <- as.network(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(network::is.network(net))
    expect_identical(network::network.vertex.names(net), featnames(mat))
    expect_identical(network::get.vertex.attribute(net, "frequency"), unname(mat@margin))
    expect_silent(as.network(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("as.igraph.fcm works", {
    skip_if_not_installed("igraph")
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    mat <- fcm(txt)
    net <- as.igraph(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(igraph::is.igraph(net))
    expect_identical(igraph::vertex_attr(net, "name"), featnames(mat))
    expect_identical(igraph::vertex_attr(net, "frequency"), unname(mat@margin))
    expect_silent(as.igraph(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("as.igraph.fcm works with window", {
    skip_if_not_installed("igraph")
    skip_on_os("linux")
    txt <- c("a a a b b c", "a a c e", "a c e f g")
    mat <- fcm(txt, contex = "window", window = 2)
    net <- as.igraph(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(igraph::is.igraph(net))
    expect_identical(igraph::vertex_attr(net, "name"), featnames(mat))
    expect_identical(igraph::vertex_attr(net, "frequency"), unname(mat@margin))
    expect_silent(as.igraph(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("test empty object is handled properly", {

    mat <- quanteda:::make_null_dfm()
    expect_equal(dim(fcm(mat)), c(0, 0))
    expect_true(is.fcm(fcm(mat)))

    toks <- tokens(c("", ""))
    expect_equal(dim(fcm(toks)), c(0, 0))
    expect_true(is.fcm(fcm(toks)))
})

test_that("arithmetic/linear operation works with dfm", {

    mt <- fcm(dfm(c(d1 = "a a b", d2 = "a b b c", d3 = "c c d")))
    expect_true(is.fcm(mt + 2))
    expect_true(is.fcm(mt - 2))
    expect_true(is.fcm(mt * 2))
    expect_true(is.fcm(mt / 2))
    expect_true(is.fcm(mt ^ 2))
    expect_true(is.fcm(2 + mt))
    expect_true(is.fcm(2 - mt))
    expect_true(is.fcm(2 * mt))
    expect_true(is.fcm(2 / mt))
    expect_true(is.fcm(2 ^ mt))
    expect_true(is.fcm(t(mt)))
    expect_equal(rowSums(mt), colSums(t(mt)))

})

test_that("ordered is working correctly (#1413)", {
    expect_equivalent(
        as.matrix(fcm(c("a b c", "a b c"), "window", window = 1, ordered = TRUE)),
        matrix(c(0, 2, 0, 0, 0, 2, 0, 0, 0),
               nrow = 3, ncol = 3, byrow = TRUE))

    expect_equivalent(
        as.matrix(fcm(c("a b c", "a b c"), "window", window = 2, ordered = TRUE)),
        matrix(c(0, 2, 2, 0, 0, 2, 0, 0, 0),
               nrow = 3, ncol = 3, byrow = TRUE))

    expect_equivalent(
        as.matrix(fcm(c("a b c", "c b a"), "window", window = 1, ordered = TRUE)),
        matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0),
               nrow = 3, ncol = 3, byrow = TRUE))

    expect_equivalent(
        as.matrix(fcm(c("a b c", "c b a"), "window", window = 2, ordered = TRUE)),
        matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0),
               nrow = 3, ncol = 3, byrow = TRUE))

    expect_equal(fcm(c("a b c", "a b c"), "window", window = 1, ordered = TRUE, tri = TRUE),
                 fcm(c("a b c", "a b c"), "window", window = 1, ordered = TRUE, tri = FALSE))

})


test_that("dimnames are always character vectors", {
    mt <- fcm(c("a b c", "a b c"), "window", window = 1, ordered = TRUE)
    expect_identical(dimnames(mt[, character()]),
                     list(features = rownames(mt), features = character()))
    expect_identical(dimnames(mt[, FALSE]),
                     list(features = rownames(mt), features = character()))
    expect_identical(dimnames(mt[character(), ]),
                     list(features = character(), features = colnames(mt)))
    expect_identical(dimnames(mt[FALSE, ]),
                     list(features = character(), features = colnames(mt)))
})

test_that("fcm_setnames works", {
    x <- fcm(c("a b c", "a b c"), "window", window = 1)

    quanteda:::set_fcm_featnames(x) <- paste0("feature", 1:3)
    expect_identical(featnames(x), c("feature1", "feature2", "feature3"))

    quanteda:::set_fcm_dimnames(x) <- list(paste0("feature", 1:3), paste0("ALTFEAT", 1:3))
})


test_that("fcm feature names have encoding", {
    mt <- fcm(c("文書１" = "あ い い う", "文書２" = "え え え お"))
    expect_true(all(Encoding(colnames(mt)) == "UTF-8"))
    expect_true(all(Encoding(rownames(mt)) == "UTF-8"))

    mt1 <- fcm_sort(mt)
    expect_true(all(Encoding(colnames(mt1)) == "UTF-8"))
    expect_true(all(Encoding(rownames(mt1)) == "UTF-8"))

    mt2 <- fcm_remove(mt, c("あ"))
    expect_true(all(Encoding(colnames(mt2)) == "UTF-8"))
    expect_true(all(Encoding(rownames(mt2)) == "UTF-8"))
})

test_that("fcm raise nicer error message, #1267", {

    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    mx <- fcm(dfm(txt))
    expect_silent(mx[])
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
