context("test fcm methods")

toks_test <- tokens(c("b A A d", "C C a b B e"))
fcmt_test <- fcm(toks_test, context = "document")

test_that("fcm_compress works as expected, not working for 'window' context", {
    fcmt <- fcm(toks_test, 
                context = "window", window = 3)
    expect_error(fcm_compress(fcmt), 
                 "fcm must be created with a document context")
})

test_that("fcm_tolower and fcm_compress work as expected", {
    fcmt_lc <- fcm_tolower(fcmt_test)
    expect_equivalent(rownames(fcmt_lc), 
                      c("b", "a", "d", "c", "e"))
    mt <- matrix(c(1, 3, 1, 2, 2, 
                     0, 1, 2, 0, 1, 
                     0, 0, 0, 0, 0, 
                     0, 0, 0, 1, 2, 
                     0, 0, 0, 0, 0),
                   nrow = 5, ncol = 5, byrow = TRUE)
    expect_true(all(as.vector(Matrix::triu(fcmt_lc)) == as.vector(mt)))
})

test_that("fcm_toupper and fcm_compress work as expected",{
    fcmt_uc <- fcm_toupper(fcmt_test)
    expect_equivalent(rownames(fcmt_uc), 
                      c("B", "A", "D", "C", "E"))
    mt <- matrix(c(1, 3, 1, 2, 2, 
                     0, 1, 2, 0, 1, 
                     0, 0, 0, 0, 0, 
                     0, 0, 0, 1, 2, 
                     0, 0, 0, 0, 0),
                   nrow = 5, ncol = 5, byrow = TRUE)
    expect_true(all(as.vector(Matrix::triu(fcmt_uc)) == as.vector(mt)))
})


txt <- c(doc1 = "a B c D e",
         doc2 = "a BBB c D e",
         doc3 = "Aaaa BBB cc")
fcmt_test2 <- fcm(txt, context = "document", count = "frequency", tri = TRUE)

test_that("test fcm_select, fixed", {
    expect_equal(
        featnames(fcm_select(fcmt_test2, c("a", "b", "c"), selection = "keep", valuetype = "fixed", verbose = FALSE)),
        c("a", "B", "c")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, c("a", "b", "c"), selection = "remove", valuetype = "fixed", verbose = FALSE)),
        setdiff(featnames(fcmt_test2), c("a", "B", "c"))
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, c("a", "b", "c"), selection = "keep", valuetype = "fixed", case_insensitive = FALSE, verbose = FALSE)),
        c("a", "c")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, c("a", "b", "c"), selection = "remove", valuetype = "fixed", case_insensitive = FALSE, verbose = FALSE)),
        setdiff(featnames(fcmt_test2), c("a", "c"))
    )
#     expect_equal(
#         featnames(fcm_select(fcmt_test2, c("aaaa", "bbb", "cc"), selection = "keep", valuetype = "fixed", min_nchar = 3, verbose = FALSE)),
#         c("BBB", "Aaaa")
#     )
#     expect_equal(
#         featnames(fcm_select(fcmt_test2, c("aaaa", "bbb", "cc"), selection = "remove", valuetype = "fixed", min_nchar = 3, verbose = FALSE)),
#         setdiff(featnames(fcmt_test2), c("BBB", "Aaaa"))
#     )
#     expect_equal(
#         featnames(fcm_select(fcmt_test2, c("aaaa", "bbb", "cc"), selection = "keep", valuetype = "fixed", min_nchar = 3, max_nchar = 3, verbose = FALSE)),
#         c("BBB")
#     )
#     expect_equal(
#         featnames(fcm_select(fcmt_test2, c("aaaa", "bbb", "cc"), selection = "remove", valuetype = "fixed", min_nchar = 3, max_nchar = 3, verbose = FALSE)),
#         setdiff(featnames(fcmt_test2), c("BBB"))
#     )
})

test_that("test fcm_select, glob", {
    pat <- c("a*", "B*", "c")
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "keep", valuetype = "glob", verbose = FALSE)),
        c("a", "B", "c", "BBB", "Aaaa")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "remove", valuetype = "glob", verbose = FALSE)),
        setdiff(featnames(fcmt_test2), c("a", "B", "c", "BBB", "Aaaa"))
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "keep", valuetype = "glob", case_insensitive = FALSE, verbose = FALSE)),
        c("a", "B", "c", "BBB")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "remove", valuetype = "glob", case_insensitive = FALSE, verbose = FALSE)),
        setdiff(featnames(fcmt_test2), c("a", "B", "c", "BBB"))
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, selection = "keep", valuetype = "glob", min_nchar = 3, verbose = FALSE)),
        c("BBB", "Aaaa")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, selection = "remove", valuetype = "glob", max_nchar = 2, verbose = FALSE)),
        setdiff(featnames(fcmt_test2), c("BBB", "Aaaa"))
    )
})

test_that("test fcm_select, regex", {
    pat <- c("[A-Z].*", "c.+")
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "keep", valuetype = "regex", verbose = FALSE)),
        c("a", "B", "c", "D", "e", "BBB", "Aaaa", "cc")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "remove", valuetype = "regex", verbose = FALSE)),
        character(0)
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "keep", valuetype = "regex", case_insensitive = FALSE, verbose = FALSE)),
        c("B", "D", "BBB", "Aaaa", "cc")
    )
    expect_equal(
        featnames(fcm_select(fcmt_test2, pat, selection = "remove", valuetype = "regex", case_insensitive = FALSE, verbose = FALSE)),
        setdiff(featnames(fcmt_test2), c("B", "D", "BBB", "Aaaa", "cc"))
    )
})

test_that("glob works if results in no features", {
    expect_true(is.fcm(fcm_select(fcmt_test2, "notthere")))
})

test_that("longer selection than longer than features that exist (related to #447)", {
    fcmt_test2 <- fcm(tokens(c(d1 = 'a b', d2 = 'a b c d e')))
    feat <- c('b', 'c', 'd', 'e', 'f', 'g')
    # bugs in C++ needs repeated tests
    expect_message(fcm_select(fcmt_test2, feat, verbose = TRUE),
                   "kept 4 features")
    expect_equivalent(
        as.matrix(fcm_select(fcmt_test2, feat)),
        matrix(c(0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0), nrow = 4, byrow = TRUE)
    )
})

# test_that("test fcm_select with features from a dfm,  fixed", {
#     txt <- c("a", "b", "c")
#     mx <- dfm(txt)
#     expect_equal(
#         featnames(fcm_select(fcmt_test2, mx, selection = "keep", valuetype = "fixed", verbose = FALSE)),
#         featnames(mx)
#     )
#     expect_equal(
#         featnames(fcm_select(fcmt_test2, mx, selection = "remove", valuetype = "fixed", verbose = FALSE)),
#         setdiff(featnames(fcmt_test2), featnames(mx))
#     )
# })

test_that("test fcm_compress retains class", {
    fcmt <- fcm(tokens(c("b A A d", "C C a b B e")), context = "document")
    colnames(fcmt) <- rownames(fcmt) <- tolower(colnames(fcmt))
    fcmt2 <- fcm_compress(fcmt)
    expect_equivalent(class(fcmt2), "fcm")
})

test_that("shortcut functions works", {
    fcmt_test2 <- fcm(data_corpus_inaugural[1:5])
    expect_equal(fcm_select(fcmt_test2, stopwords('english'), selection = 'keep'),
                 fcm_keep(fcmt_test2, stopwords('english')))
    expect_equal(fcm_select(fcmt_test2, stopwords('english'), selection = 'remove'),
                 fcm_remove(fcmt_test2, stopwords('english')))
})

test_that("as.fcm is working", {
    
    feat1 <- c("B", "A", "D", "C", "E")
    feat2 <- c("Z", "X", "N", "M", "K")
    
    mt1 <- matrix(c(1, 3, 1, 2, 2, 
                   0, 1, 2, 0, 1, 
                   0, 0, 0, 0, 0, 
                   0, 0, 0, 1, 2, 
                   0, 0, 0, 0, 0),
                 dimnames = list(feat1, feat1),
                 nrow = 5, ncol = 5, byrow = TRUE)
    
    expect_true(is.fcm(as.fcm(mt1)))
    expect_true(is.fcm(as.fcm(as(mt1, "dtCMatrix"))))
    expect_true(is.fcm(as.fcm(as(mt1, "dgCMatrix"))))
    expect_true(is.fcm(as.fcm(as(mt1, "dgTMatrix"))))
    expect_true(is.fcm(as.fcm(as(mt1, "dgeMatrix"))))
    
    mt2 <- matrix(c(1, 3, 1, 2, 2, 
                    0, 1, 2, 0, 1, 
                    0, 0, 0, 0, 0, 
                    0, 0, 0, 1, 2, 
                    0, 0, 0, 0, 0),
                  dimnames = list(feat1, feat2),
                  nrow = 5, ncol = 5, byrow = TRUE)
    
    expect_error(as.fcm(mt2), 
                "matrix must have the same rownames and colnames")
    expect_error(as.fcm(as(mt2, "dgeMatrix")),
                "matrix must have the same rownames and colnames")

})
