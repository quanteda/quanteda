test_that("docnames always return names even if there aren't", {
    corp <- corpus(c("aaa", "bbb", "ccc"))
    expect_equal(length(docnames(corp)), ndoc(corp))

    toks <- as.tokens(list("aaa", "bbb", "ccc"))
    expect_equal(length(docnames(toks)), ndoc(toks))
})

test_that("docnames<- works with corpus, tokens and dfm (#987)", {
    corp <- corpus(c("aaa", "bbb", "ccc"))
    toks <- tokens(corp)
    mx <- dfm(toks)

    name_new <- c("doc1", "doc2", "doc3")
    docnames(corp) <- name_new
    docnames(toks) <- name_new
    docnames(mx) <- name_new

    expect_equal(docnames(corp), name_new)
    expect_equal(docnames(toks), name_new)
    expect_equal(docnames(mx), name_new)
    expect_equal(attr(corp, "docvars")[["docname_"]], name_new)
    expect_equal(attr(toks, "docvars")[["docname_"]], name_new)
    expect_equal(attr(mx, "docvars")[["docname_"]], name_new)
})

test_that("docnames are character", {
    txt <- c("a b c", "d e f", "h i j")
    corp <- corpus(txt)
    docnames(corp) <- c(1, 5, 9)
    expect_identical(attr(corp, "names"), c("1", "5", "9"))
    expect_identical(attr(corp, "docvars")[["docname_"]], c("1", "5", "9"))
    toks <- tokens(corp)
    docnames(toks) <- c(2, 3, 7)
    expect_identical(attr(toks, "names"), c("2", "3", "7"))
    expect_identical(attr(toks, "docvars")[["docname_"]], c("2", "3", "7"))
    dfmat <- dfm(toks)
    docnames(dfmat) <- c(4, 8, 0)
    expect_identical(dfmat@Dimnames$docs, c("4", "8", "0"))
    expect_identical(attr(dfmat, "docvars")[["docname_"]], c("4", "8", "0"))
})

test_that("special names<- operator works as planned", {

    corp <- corpus(LETTERS[1:3], docnames = letters[1:3])
    names(corp)[1] <- "X"
    expect_identical(
        names(corp),
        attr(corp, "docvars")[["docname_"]]
    )

    toks <- tokens(corpus(LETTERS[1:3], docnames = letters[1:3]))
    names(toks)[1] <- "X"
    expect_identical(
        names(toks),
        attr(toks, "docvars")[["docname_"]]
    )

    dfmat <- dfm(tokens(corpus(LETTERS[1:3], docnames = letters[1:3])))
    rownames(dfmat)[1] <- "X"
    expect_identical(
        rownames(dfmat),
        attr(toks, "docvars")[["docname_"]]
    )
})


test_that("docnames are alwyas unique", {
    corp <- data_corpus_inaugural
    toks <- tokens(corp)
    dfmat <- dfm(toks)

    corp1 <- corp
    docnames(corp1) <- docvars(corp1, "Party")
    expect_false(any(duplicated((docnames(corp1)))))
    expect_false(any(duplicated((attr(corp1, "names")))))

    corp2 <- corp[c(5, 5)]
    expect_false(any(duplicated((docnames(corp2)))))
    expect_identical(docnames(corp2), attr(corp2, "names"))

    corp3 <- corp[c("1805-Jefferson", "1805-Jefferson")]
    expect_false(any(duplicated((docnames(corp3)))))
    expect_identical(docnames(corp3), attr(corp3, "names"))

    toks1 <- toks
    docnames(toks1) <- docvars(toks1, "Party")
    expect_false(any(duplicated((docnames(toks1)))))
    expect_identical(docnames(toks1), attr(toks1, "names"))

    toks2 <- toks[c(5, 5)]
    expect_false(any(duplicated((docnames(toks2)))))
    expect_identical(docnames(toks2), attr(toks2, "names"))

    toks3 <- toks[c("1805-Jefferson", "1805-Jefferson")]
    expect_false(any(duplicated((docnames(toks3)))))
    expect_identical(docnames(toks3), attr(toks3, "names"))

    dfmat1 <- dfmat
    docnames(dfmat1) <- docvars(dfmat1, "Party")
    expect_false(any(duplicated((docnames(dfmat1)))))
    expect_identical(docnames(dfmat1), dfmat1@Dimnames[["docs"]])

    dfmat2 <- dfmat[c(5, 5), ]
    expect_false(any(duplicated((docnames(dfmat2)))))
    expect_identical(docnames(dfmat2), dfmat2@Dimnames[["docs"]])

    dfmat3 <- dfmat[c("1805-Jefferson", "1805-Jefferson"), ]
    expect_false(any(duplicated((docnames(dfmat3)))))
    expect_identical(docnames(dfmat3), dfmat3@Dimnames[["docs"]])
})


test_that("docnames are the same after subsetting (#2127)", {
    
    corp <- corpus(c(doc1 = "This is a sentence.  Another sentence.  Yet another.", 
                     doc2 = "Premiere phrase.  Deuxieme phrase."))
    corp <- corpus_reshape(corp)
    toks <- tokens(corp)
    dfmat <- dfm(toks)

    # do not change docnames
    expect_identical(docnames(corp[c("doc1.2", "doc2.1", "doc2.2")]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docnames(corp[c(2, 4, 5)]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docnames(corp[c(FALSE, TRUE, FALSE, TRUE, TRUE)]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docid(corp[c(2, 4, 5)]), factor(c("doc1", "doc2", "doc2")))
    expect_identical(segid(corp[c(2, 4, 5)]), c(2L, 1L, 2L))
    
    expect_identical(docnames(toks[c("doc1.2", "doc2.1", "doc2.2")]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docnames(toks[c(2, 4, 5)]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docnames(toks[c(FALSE, TRUE, FALSE, TRUE, TRUE)]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docid(toks[c(2, 4, 5)]), factor(c("doc1", "doc2", "doc2")))
    expect_identical(segid(toks[c(2, 4, 5)]), c(2L, 1L, 2L))

    expect_identical(docnames(dfmat[c("doc1.2", "doc2.1", "doc2.2"),]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docnames(dfmat[c(2, 4, 5),]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docnames(dfmat[c(FALSE, TRUE, FALSE, TRUE, TRUE),]), c("doc1.2", "doc2.1", "doc2.2"))
    expect_identical(docid(dfmat[c(2, 4, 5),]), factor(c("doc1", "doc2", "doc2")))
    expect_identical(segid(dfmat[c(2, 4, 5),]), c(2L, 1L, 2L))
    
    # preserve order of segid 
    expect_identical(docnames(corp[c("doc1.1", "doc1.3", "doc1.1")]), c("doc1.1", "doc1.3", "doc1.2"))
    expect_identical(docnames(corp[c(1, 3, 1)]), c("doc1.1", "doc1.3", "doc1.2"))
    expect_identical(docnames(corp[c("doc2.1", "doc1.2", "doc2.1")]), c("doc2.1", "doc1.1", "doc2.2"))
    expect_identical(docnames(corp[c(4, 2, 4)]), c("doc2.1", "doc1.1", "doc2.2"))
    expect_identical(docid(corp[c(4, 2, 4)]), factor(c("doc2", "doc1", "doc2"), levels = c("doc1", "doc2")))
    expect_identical(segid(corp[c(4, 2, 4)]), c(1L, 1L, 2L))
    
    expect_identical(docnames(toks[c("doc1.1", "doc1.3", "doc1.1")]), c("doc1.1", "doc1.3", "doc1.2"))
    expect_identical(docnames(toks[c(1, 3, 1)]), c("doc1.1", "doc1.3", "doc1.2"))
    expect_identical(docnames(toks[c("doc2.1", "doc1.2", "doc2.1")]), c("doc2.1", "doc1.1", "doc2.2"))
    expect_identical(docnames(toks[c(4, 2, 4)]), c("doc2.1", "doc1.1", "doc2.2"))
    expect_identical(docid(toks[c(4, 2, 4)]), factor(c("doc2", "doc1", "doc2"), levels = c("doc1", "doc2")))
    expect_identical(segid(toks[c(4, 2, 4)]), c(1L, 1L, 2L))
    
    expect_identical(docnames(dfmat[c("doc1.1", "doc1.3", "doc1.1"),]), c("doc1.1", "doc1.3", "doc1.2"))
    expect_identical(docnames(dfmat[c(1, 3, 1),]), c("doc1.1", "doc1.3", "doc1.2"))
    expect_identical(docnames(dfmat[c("doc2.1", "doc1.2", "doc2.1"),]), c("doc2.1", "doc1.1", "doc2.2"))
    expect_identical(docnames(dfmat[c(4, 2, 4),]), c("doc2.1", "doc1.1", "doc2.2"))
    expect_identical(docid(dfmat[c(4, 2, 4),]), factor(c("doc2", "doc1", "doc2"), levels = c("doc1", "doc2")))
    expect_identical(segid(dfmat[c(4, 2, 4),]), c(1L, 1L, 2L))
})

    