context("test docnames")

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
    
    dfmat <- dfm(corpus(LETTERS[1:3], docnames = letters[1:3]))
    rownames(dfmat)[1] <- "X"
    expect_identical(
        rownames(dfmat),
        attr(toks, "docvars")[["docname_"]]
    )
})


test_that("docnames are alwyas unique", {
    
    corp <- data_corpus_irishbudget2010
    toks <- tokens(corp)
    dfmt <- dfm(toks)
    
    corp1 <- corp
    docnames(corp1) <- docvars(corp1, "party")
    expect_false(any(duplicated((docnames(corp1)))))
    expect_false(any(duplicated((attr(corp1, "names")))))
    
    corp2 <- corp[c(5, 5)]
    expect_false(any(duplicated((docnames(corp2)))))
    expect_identical(docnames(corp2), attr(corp2, "names"))
    
    corp3 <- corp[c("Cowen, Brian (FF)", "Cowen, Brian (FF)")]
    expect_false(any(duplicated((docnames(corp3)))))
    expect_identical(docnames(corp3), attr(corp3, "names"))
    
    toks1 <- toks
    docnames(toks1) <- docvars(toks1, "party")
    expect_false(any(duplicated((docnames(toks1)))))
    expect_identical(docnames(toks1), attr(toks1, "names"))
    
    toks2 <- toks[c(5, 5)]
    expect_false(any(duplicated((docnames(toks2)))))
    expect_identical(docnames(toks2), attr(toks2, "names"))
    
    toks3 <- toks[c("Cowen, Brian (FF)", "Cowen, Brian (FF)")]
    expect_false(any(duplicated((docnames(toks3)))))
    expect_identical(docnames(toks3), attr(toks3, "names"))
    
    dfmt1 <- dfmt
    docnames(dfmt1) <- docvars(dfmt1, "party")
    expect_false(any(duplicated((docnames(dfmt1)))))
    expect_identical(docnames(dfmt1), dfmt1@Dimnames[["docs"]])
    
    dfmt2 <- dfmt[c(5, 5),]
    expect_false(any(duplicated((docnames(dfmt2)))))
    expect_identical(docnames(dfmt2), dfmt2@Dimnames[["docs"]])
    
    dfmt3 <- dfmt[c("Cowen, Brian (FF)", "Cowen, Brian (FF)"),]
    expect_false(any(duplicated((docnames(dfmt3)))))
    expect_identical(docnames(dfmt3), dfmt3@Dimnames[["docs"]])
})





