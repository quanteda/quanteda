context("test docvars")


test_that("make_docvars() works", {
    
    docvar1 <- quanteda:::make_docvars(character())
    docvar2 <- quanteda:::make_docvars(c("A", "B", "C"))
    docvar3 <- quanteda:::make_docvars(1:3)
    expect_equal(dim(docvar1), c(0, 4))
    expect_equal(dim(docvar2), c(3, 4))
    expect_equal(dim(docvar3), c(3, 4))
    expect_equal(colnames(docvar1), c("_docid", "_docname", "_docnum", "_segnum"))
    expect_equal(colnames(docvar2), c("_docid", "_docname", "_docnum", "_segnum"))
    expect_equal(colnames(docvar3), c("_docid", "_docname", "_docnum", "_segnum"))
    
    docvar4 <- quanteda:::make_docvars(c("A", "A", "B", "B", "C"))
    expect_equal(docvar4[["_docid"]], c("A.1", "A.2", "B.1", "B.2", "C.1"))
    expect_equal(docvar4[["_docnum"]], c(1, 1, 2, 2, 3))
    docvar5 <- quanteda:::make_docvars(c("A", "B", "B", "A", "C"))
    expect_equal(docvar5[["_docid"]], c("A.1", "B.1", "B.2", "A.2", "C.1"))
    expect_equal(docvar5[["_docnum"]], c(1, 2, 2, 1, 3))
    docvar6 <- quanteda:::make_docvars(c("A", "A", "B", "B", "C"), unique = FALSE)
    expect_equal(docvar6[["_docid"]], c("A", "A", "B", "B", "C"))
    expect_equal(docvar6[["_docnum"]], c(1, 2, 3, 4, 5))
    expect_equal(docvar6[["_segnum"]], c(1, 1, 1, 1, 1))
})

test_that("upgrade_docvars() workds", {
    
    docvar1 <- data.frame()
    docvar2 <- data.frame("var1" = c(100, 200, 300),
                          "var2" = c(TRUE, TRUE, FALSE))
    docvar3 <- data.frame("var1" = c(100, 200, 300),
                          "var2" = c(TRUE, TRUE, FALSE),
                          row.names = c("A", "B", "C"))
    docvar4 <- data.frame("_docid" = c("A", "B", "C"), 
                          "_docname" = c("A", "B", "C"), 
                          "_docnum" = 1L:3L, 
                          "_segnum" = rep(1L, 3), 
                          "var1" = c(100, 200, 300),
                          "var2" = c(TRUE, TRUE, FALSE),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
    expect_identical(
        quanteda:::upgrade_docvars(docvar1, c("A", "B", "C")),
        docvar4[,1:4]
    )
    expect_identical(
        quanteda:::upgrade_docvars(docvar3),
                     docvar4
    )
    expect_identical(
        quanteda:::upgrade_docvars(docvar2, c("A", "B", "C")),
        docvar4
    )
})

test_that("get_docvars() workds", {
    
    data <- data.frame("_docid" = c("A", "B", "C"), 
                         "_docname" = c("A", "B", "C"), 
                         "_docnum" = 1L:3L, 
                         "_segnum" = rep(1L, 3), 
                         "var1" = c(100, 200, 300),
                         "var2" = c(TRUE, TRUE, FALSE),
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
    
    expect_identical(
        quanteda:::get_docvars.data.frame(data, system = TRUE),
        data.frame("_docid" = c("A", "B", "C"), 
                   "_docname" = c("A", "B", "C"), 
                   "_docnum" = 1L:3L, 
                   "_segnum" = rep(1L, 3), 
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    )
    expect_error(quanteda:::get_docvars.data.frame(data, "_docname", system = FALSE))
    expect_identical(
        quanteda:::get_docvars.data.frame(data, "_docname", system = TRUE),
        data.frame("_docname" = c("A", "B", "C"), 
                   check.names = FALSE, stringsAsFactors = FALSE)
    )
    expect_identical(
        quanteda:::get_docvars.data.frame(data, "_docname", system = TRUE, drop = TRUE),
        c("A", "B", "C")
    )
    expect_identical(
        quanteda:::get_docvars.data.frame(data),
        data.frame("var1" = c(100, 200, 300),
                   "var2" = c(TRUE, TRUE, FALSE),
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    )
    expect_identical(
        quanteda:::get_docvars.data.frame(data, "var1"),
        data.frame("var1" = c(100, 200, 300), stringsAsFactors = FALSE)
    )
    expect_identical(
        quanteda:::get_docvars.data.frame(data, "var1", drop = TRUE),
        c(100, 200, 300)
    )
})

test_that("set_docvars() workds", {
    
    data <- data.frame("_docid" = c("A", "B", "C"), 
                       "_docname" = c("A", "B", "C"), 
                       "_docnum" = 1L:3L, 
                       "_segnum" = rep(1L, 3), 
                       "var1" = c(100, 200, 300),
                       "var2" = c(TRUE, TRUE, FALSE),
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
    
    quanteda:::set_docvars(data, "var2") <- c(10, 20, 30)
    expect_identical(data[["var2"]], c(10, 20, 30))
    quanteda:::set_docvars(data, "var3") <- c(1000, 2000, 3000)
    expect_identical(data[["var3"]], c(1000, 2000, 3000))
    quanteda:::set_docvars(data) <- data.frame("var1" = c(100, 200, 300),
                                               "var2" = c(TRUE, TRUE, TRUE))
    expect_identical(data[["var1"]], c(100, 200, 300))
    expect_identical(data[["var2"]], c(TRUE, TRUE, TRUE))
    expect_identical(names(data), c("_docid", "_docname", "_docnum", "_segnum", "var1", "var2"))
    quanteda:::set_docvars(data) <- NULL
    expect_identical(names(data), c("_docid", "_docname", "_docnum", "_segnum"))
})

test_that("docvars of corpus is a data.frame", {
    expect_equal(
        class(docvars(data_corpus_inaugural)),
        "data.frame"
    )
})

test_that("docvars with non-existent field names generate correct error messages", {
    expect_error(
        docvars(data_corpus_inaugural, c("President", "nonexistent")),
        "field\\(s\\) nonexistent not found"
    )

    toks <- tokens(data_corpus_inaugural, include_docvars = TRUE)
    expect_error(
        docvars(toks, c("President", "nonexistent")),
        "field\\(s\\) nonexistent not found"
    )
})


test_that("docvars is working with tokens", {
    corp <- data_corpus_inaugural
    toks <- tokens(corp, include_docvars = TRUE)
    expect_equal(docvars(toks), docvars(corp))
    expect_equal(docvars(toks, 'President'), docvars(corp, 'President'))
    
    # Subset
    toks2 <- toks[docvars(toks, 'Year') > 2000]
    expect_equal(ndoc(toks2), nrow(docvars(toks2)))
    
    # # Add field to meta-data
    # expect_equal(
    #     docvars(quanteda:::"docvars<-"(toks2, 'Type', 'Speech'), "Type"), 
    #     rep('Speech', 5)
    # )
    # 
    # # Remove meta-data
    # expect_equal(
    #     docvars(quanteda:::"docvars<-"(toks, field = NULL, NULL)), 
    #     NULL
    # )
    # 
    # # Add fresh meta-data
    # expect_equal(
    #     docvars(quanteda:::"docvars<-"(toks, field = "ID", 1:58), "ID"), 
    #     1:58
    # )
}) 

test_that("docvars is working with dfm", {
    corp <- data_corpus_irishbudget2010
    toks <- tokens(corp, include_docvars = TRUE)
    thedfm <- dfm(toks)
    
    expect_equal(docvars(toks), docvars(thedfm))
    expect_equal(docvars(toks, 'party'), docvars(corp, 'party'))
    
    thedfm2 <- dfm(corp)
    expect_equal(docvars(corp), docvars(thedfm2))
    expect_equal(docvars(corp, 'party'), docvars(thedfm2, 'party'))

    corp2 <- corpus_subset(corp, party == "LAB")
    thedfm3 <- dfm(corp2)    
    expect_equal(docvars(corp2), docvars(thedfm3))
}) 

test_that("creating tokens and dfms with empty docvars", {
    expect_true(
        length(docvars(tokens(data_corpus_irishbudget2010, include_docvars = FALSE))) == 0
    )
    expect_true(
        length(docvars(dfm(data_corpus_irishbudget2010, include_docvars = FALSE))) == 0
    )
    
})

test_that("tokens works works with one docvar", {
    docv1 <- data.frame(dvar1 = c("A", "B"))
    mycorpus1 <- corpus(c(d1 = "This is sample document one.",
                          d2 = "Here is the second sample document."), 
                        docvars = docv1)
    toks1 <- tokens(mycorpus1, include_docvars = TRUE)
    expect_equivalent(docvars(toks1), docv1)
})


test_that("tokens works works with two docvars", {
    docv2 <- data.frame(dvar1 = c("A", "B"),
                        dvar2 = c(1, 2))
    mycorpus2 <- corpus(c(d1 = "This is sample document one.",
                          d2 = "Here is the second sample document."), 
                        docvars = docv2)
    toks2 <- tokens(mycorpus2, include_docvars = TRUE)
    expect_equivalent(docvars(toks2), docv2)
})

test_that("dfm works works with one docvar", {
    docv1 <- data.frame(dvar1 = c("A", "B"))
    mycorpus1 <- corpus(c(d1 = "This is sample document one.",
                          d2 = "Here is the second sample document."), 
                        docvars = docv1)
    dfm1 <- dfm(mycorpus1, include_docvars = TRUE)
    expect_equivalent(docvars(dfm1), docv1)
})


test_that("dfm works works with two docvars", {
    docv2 <- data.frame(dvar1 = c("A", "B"),
                        dvar2 = c(1, 2))
    mycorpus2 <- corpus(c(d1 = "This is sample document one.",
                          d2 = "Here is the second sample document."), 
                        docvars = docv2)
    dfm2 <- dfm(mycorpus2, include_docvars = TRUE)
    expect_equivalent(docvars(dfm2), docv2)
})

test_that("object always have docvars in the same rows as documents", {
    
    txts <- data_char_ukimmig2010
    corp1 <- corpus(txts)
    expect_true(nrow(docvars(corp1)) == ndoc(corp1))
    expect_true(all(rownames(docvars(corp1)) == docnames(corp1)))
    
    corp2 <- corpus_segment(corp1, "\\p{P}", valuetype = "regex")
    expect_true(nrow(docvars(corp2)) == ndoc(corp2))
    expect_true(all(rownames(docvars(corp2)) == docnames(corp2)))
    
    corp3 <- corpus_reshape(corp1, to = "sentences")
    expect_true(nrow(docvars(corp3)) == ndoc(corp3))
    expect_true(all(rownames(docvars(corp3)) == docnames(corp3)))
    
    corp4 <- corpus_sample(corp1, size = 5)
    expect_true(nrow(docvars(corp4)) == ndoc(corp4))
    expect_true(all(rownames(docvars(corp4)) == docnames(corp4)))
    
    toks1 <- tokens(txts)
    expect_true(nrow(docvars(toks1)) == ndoc(toks1))
    expect_true(all(rownames(docvars(toks1)) == docnames(toks1)))
    
    toks2 <- tokens(corpus(txts))
    expect_true(nrow(docvars(toks2)) == ndoc(toks2))
    expect_true(all(rownames(docvars(toks2)) == docnames(toks2)))
    
    toks3 <- quanteda:::tokens_group(toks1, rep(c(1, 2, 3), 3))
    expect_true(nrow(docvars(toks3)) == ndoc(toks3))
    expect_true(all(rownames(docvars(toks3)) == docnames(toks3)))
    
    toks4 <- tokens_select(toks1, stopwords())
    expect_true(nrow(docvars(toks4)) == ndoc(toks4))
    expect_true(all(rownames(docvars(toks4)) == docnames(toks4)))
    
    dfm1 <- dfm(txts)
    expect_true(nrow(docvars(dfm1)) == ndoc(dfm1))
    expect_true(all(rownames(docvars(toks3)) == docnames(toks3)))
    
    dfm2 <- dfm(tokens(txts))
    expect_true(nrow(docvars(dfm2)) == ndoc(dfm2))
    expect_true(all(rownames(docvars(dfm2)) == docnames(dfm2)))
    
    dfm3 <- dfm(corpus(txts))
    expect_true(nrow(docvars(dfm3)) == ndoc(dfm3))
    expect_true(all(rownames(docvars(dfm3)) == docnames(dfm3)))
    
    dfm4 <- dfm_group(dfm1, rep(c(1, 2, 3), 3))
    expect_true(nrow(docvars(dfm4)) == ndoc(dfm4))
    expect_true(all(rownames(docvars(dfm4)) == docnames(dfm4)))
    
    dfm5 <- dfm(dfm1, group = rep(c(1, 2, 3), 3))
    expect_true(nrow(docvars(dfm5)) == ndoc(dfm5))
    expect_true(all(rownames(docvars(dfm5)) == docnames(dfm5)))
    
    dfm6 <- dfm_subset(dfm1, rep(c(TRUE, TRUE, FALSE), 3))
    expect_true(nrow(docvars(dfm6)) == ndoc(dfm6))
    expect_true(all(rownames(docvars(dfm6)) == docnames(dfm6)))
    
    dfm7 <- rbind(dfm1, dfm1)
    expect_true(nrow(docvars(dfm7)) == ndoc(dfm7))
    
    dfm8 <- suppressWarnings(cbind(dfm1, dfm1))
    expect_true(nrow(docvars(dfm8)) == ndoc(dfm8))
    
})

test_that("error when nrow and ndoc mismatch", {
    
    toks <- tokens(c("a b c", "b c d", "c d e"))
    expect_error(docvars(toks) <- data.frame(var = c(1, 5)))
    expect_silent(docvars(toks) <- data.frame(var = c(1, 5, 6)))
    expect_error(docvars(toks) <- data.frame(var = c(1, 5, 6, 3)))
    
    mt <- dfm(toks)
    expect_error(docvars(mt) <- data.frame(var = c(1, 5)))
    expect_silent(docvars(mt) <- data.frame(var = c(1, 5, 6)))
    expect_error(docvars(mt) <- data.frame(var = c(1, 5, 6, 3)))
    
})

test_that("assignment of NULL only drop columns", {
    
    toks <- tokens(data_corpus_irishbudget2010)
    docvars(toks) <- NULL
    expect_identical(dim(docvars(toks)), c(14L, 0L))
    
    mt <- dfm(data_corpus_irishbudget2010)
    docvars(mt) <- NULL
    expect_identical(dim(docvars(mt)), c(14L, 0L))
    
})

test_that("can assign docvars when value is a dfm (#1417)", {
    mycorp <- corpus(data_char_ukimmig2010)

    thedfm <- dfm(mycorp)[, "the"]
    docvars(mycorp) <- thedfm
    expect_identical(
        docvars(mycorp),
        data.frame(the = as.vector(thedfm), row.names = docnames(mycorp))
    )
    
    anddfm <- dfm(mycorp)[, "and"]
    docvars(anddfm) <- anddfm
    expect_identical(
        docvars(anddfm),
        data.frame(and = as.vector(anddfm), row.names = 1:ndoc(mycorp)) # docnames(mycorp))
    )

    toks <- tokens(mycorp)
    docvars(toks) <- anddfm
    expect_identical(
        docvars(toks),
        data.frame(and = as.vector(anddfm), row.names = 1:ndoc(mycorp)) # docnames(mycorp))
    )
})
