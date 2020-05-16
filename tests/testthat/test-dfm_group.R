context("test dfm_group")

test_that("test dfm_group", {
    dfmt <- dfm(c("a b c c", "b c d", "a"))
    expect_equivalent(
        as.matrix(dfm_group(dfmt, c("doc1", "doc1", "doc2"))),
        matrix(c(1, 1, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c("doc1", "doc2"), c("a", "b", "c", "d")))
    )
    
    expect_equivalent(
        as.matrix(dfm_group(dfmt, c(1, 1, 2))),
        matrix(c(1, 1, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c("doc1", "doc2"), c("a", "b", "c", "d")))
    )
})

test_that("dfm_group works with empty documents", {
    dfmt <- dfm(c("a b c c", "b c d", ""))
    expect_equivalent(
        as.matrix(dfm_group(dfmt, c("doc1", "doc1", "doc2"))),
        matrix(c(1, 0, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c("doc1", "doc2"), c("a", "b", "c", "d")))
    )
    
    expect_equivalent(
        as.matrix(dfm_group(dfmt, c(1, 1, 2))),
        matrix(c(1, 0, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c("doc1", "doc2"), c("a", "b", "c", "d")))
    )
})

test_that("dfm_group works with docvars", {
    mycorpus <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
                       docvars = data.frame(grp = c(1, 1, 2, 2)))
    mydfm <- dfm(mycorpus)
    expect_equal(
        colSums(dfm_group(mydfm, groups = "grp")),
        colSums(mydfm)
    )
})

test_that("dfm.character groups works (#794)", {
    grp <- c(1, 1, 2)
    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    corp <- corpus(txt)
    toks <- tokens(corp)
    expect_equal(
        dfm(txt, groups = grp),
        dfm(toks, groups = grp)
    )
    expect_equal(
        dfm(txt, groups = grp),
        dfm(corp, groups = grp)
    )
})

test_that("test dfm_group with factor levels, fill = TRUE and FALSE, #854", {
    corp <- corpus(c("a b c c", "b c d", "a"),
                   docvars = data.frame(grp = factor(c("A", "A", "B"), levels = LETTERS[1:4])))
    dfmt <- dfm(corp)
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = "grp", fill = FALSE)),
        matrix(c(1,2,3,1, 1,0,0,0), byrow = TRUE, nrow = 2, 
               dimnames = list(docs = c("A", "B"), features = letters[1:4]))
    )
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = "grp", fill = TRUE)),
        matrix(c(1,2,3,1, 1,0,0,0, 0,0,0,0, 0,0,0,0), byrow = TRUE, nrow = 4 , 
               dimnames = list(docs = c("A", "B", "C", "D"), features = letters[1:4]))
    )
    
    dfmt <- dfm(c("a b c c", "b c d", "a"))
    external_factor <- factor(c("text1", "text1", "text2"), 
                              levels = paste0("text", 0:3))
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = external_factor, fill = FALSE)),
        matrix(c(1,2,3,1, 1,0,0,0), byrow = TRUE, nrow = 2, 
               dimnames = list(docs = c("text1", "text2"), features = letters[1:4]))
    )
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = external_factor, fill = TRUE)),
        matrix(c(0,0,0,0, 1,2,3,1, 1,0,0,0, 0,0,0,0), byrow = TRUE, nrow = 4, 
               dimnames = list(docs = paste0("text", 0:3), features = letters[1:4]))
    )
    # new documents in factor order
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = factor(c(1, 1, 2), levels = 4:1), fill = TRUE)),
        matrix(c(rep(0, 8), 1,0,0,0, 1,2,3,1), byrow = TRUE, nrow = 4,
               dimnames = list(docs = 4:1, features = letters[1:4]))
    )
    # should this also be ordered? (here the expectation is that it is)
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = factor(c(3, 3, 1), levels = 4:1), fill = FALSE)),
        matrix(c(1,2,3,1, 1,0,0,0), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c(3, 1), features = letters[1:4]))
    )
})

test_that("test dfm_group with non-factor grouping variable, with fill", {
    grp <- c("D", "D", "A", "C")
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = grp, stringsAsFactors = FALSE))
    dfmt <- dfm(corp)
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = "grp", fill = FALSE)),
        matrix(c(1,0,0,0, 0,1,0,2, 1,2,3,1), byrow = TRUE, nrow = 3, 
               dimnames = list(docs = c("A", "C", "D"), features = letters[1:4]))
    )
    expect_equal(
        dfm_group(dfmt, groups = "grp", fill = FALSE),
        dfm_group(dfmt, groups = "grp", fill = TRUE)
    )
    expect_equal(
        dfm_group(dfmt, groups = grp, fill = FALSE),
        dfm_group(dfmt, groups = grp, fill = TRUE)
    )
    
    expect_equal(
        as.matrix(dfm_group(dfmt, groups = grp, fill = FALSE)),
        matrix(c(1,0,0,0, 0,1,0,2, 1,2,3,1), byrow = TRUE, nrow = 3, 
               dimnames = list(docs = c("A", "C", "D"), features = letters[1:4]))
    )
    expect_equal(
        dfm_group(dfmt, groups = grp, fill = FALSE),
        dfm_group(dfmt, groups = "grp", fill = FALSE)
    )
})
    
test_that("test dfm_group with wrongly dimensioned groups variables", {
    grp <- c("D", "D", "A", "C")
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = grp, stringsAsFactors = FALSE))
    dfmt <- dfm(corp)
    expect_error(
        dfm_group(dfmt, groups = c(1, 1, 2, 3, 3), fill = FALSE),
        "groups must name docvars or provide data matching the documents in x"
    )
    expect_error(
        dfm_group(dfmt, groups = c(1, 1, 2, 3, 3), fill = TRUE),
        "groups must name docvars or provide data matching the documents in x"
    )
    expect_error(
        dfm_group(dfmt, groups = c(1, 1, 2, 3, 4), fill = TRUE),
        "groups must name docvars or provide data matching the documents in x"
    )
})

test_that("test dfm_group keeps group-level variables", {
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = c("D", "D", "A", "C"), 
                                        var1 = c(1, 1, 2, 2),
                                        var2 = c(1, 2, 2, 3),
                                        var3 = c("x", "x", "y", NA),
                                        var4 = c("x", "y", "y", "x"),
                                        var5 = as.Date(c("2018-01-01", "2018-01-01", "2015-03-01", "2012-12-15")),
                                        var6 = as.Date(c("2018-01-01", "2015-03-01", "2015-03-01", "2012-12-15")),
                                        stringsAsFactors = FALSE))
    
    dfmt <- dfm(corp)
    grp1 <- c("D", "D", "A", "C")
    expect_equal(
         dfm_group(dfmt, grp1)@docvars,
         data.frame("docname_" = c("A", "C", "D"),
                    "docid_" = factor(c("A", "C", "D"), 
                                      levels = c("A", "C", "D")),
                    "segid_" = c(1L, 1L, 1L),
                    grp = c("A", "C", "D"),
                    var1 = c(2, 2, 1),
                    var3 = c("y", NA, "x"),
                    var5 = as.Date(c("2015-03-01", "2012-12-15", "2018-01-01")),
                    stringsAsFactors = FALSE)
    )
    
    grp2 <- factor(c("D", "D", "A", "C"), levels = c("A", "B", "C", "D"))
    expect_equal(
        dfm_group(dfmt, grp2, fill = TRUE)@docvars,
        data.frame("docname_" = c("A", "B", "C", "D"),
                   "docid_" = factor(c("A", "B", "C", "D"), 
                                       levels = c("A", "B", "C", "D")),
                   "segid_" = c(1L, 1L, 1L, 1L),
                   grp = c("A", NA, "C", "D"),
                   var1 = c(2, NA, 2, 1),
                   var3 = c("y", NA, NA, "x"),
                   var5 = as.Date(c("2015-03-01", NA, "2012-12-15", "2018-01-01")),
                   stringsAsFactors = FALSE)
    )
})

test_that("is_grouped is working", {
    expect_false(quanteda:::is_grouped(c(1, 2, 3, 4), 
                            c(1L, 1L, 2L, 2L)))
    expect_false(quanteda:::is_grouped(c(1, 2, 2, 2), 
                            c(1L, 1L, 2L, 2L)))
    expect_false(quanteda:::is_grouped(as.factor(c(1, 2, 2, 2)),
                            c(1L, 1L, 2L, 2L)))
    expect_true(quanteda:::is_grouped(numeric(), 
                           integer()))
    
    expect_true(quanteda:::is_grouped(c(1, 1, 2, 2), 
                           c(1L, 1L, 2L, 2L)))
    expect_true(quanteda:::is_grouped(c(0, 0, 1, 1), 
                           c(1L, 1L, 2L, 2L)))
    expect_true(quanteda:::is_grouped(c(0, 0, 0, 0), 
                           c(1L, 1L, 2L, 2L)))
    
    expect_false(quanteda:::is_grouped(c("a", "b", "c", "d"), 
                            c(1L, 1L, 2L, 2L)))
    expect_false(quanteda:::is_grouped(c("a", "b", "b", "b"), 
                            c(1L, 1L, 2L, 2L)))
    expect_true(quanteda:::is_grouped(c("a", "a", "b", "b"), 
                           c(1L, 1L, 2L, 2L)))
    
    expect_true(quanteda:::is_grouped(character(), 
                           integer()))
    expect_true(quanteda:::is_grouped(c("a", "a", "a", "a"), 
                           c(1L, 1L, 2L, 2L)))
    expect_true(quanteda:::is_grouped(c("", "", "b", "b"), 
                           c(1L, 1L, 2L, 2L)))
    expect_true(quanteda:::is_grouped(c("", "", "", ""), 
                           c(1L, 1L, 2L, 2L)))
    
})

test_that("dfm_group resets weighting scheme to count (#1545)", {
    mt1 <- dfm_weight(dfm(c("a b c c", "b c d", "a")), "boolean")
    expect_equal(mt1@meta$object$weight_tf$scheme, "boolean")
    
    mt2 <- dfm_group(mt1, c("doc1", "doc1", "doc2"))
    expect_equal(mt2@meta$object$weight_tf$scheme, "boolean")
    
    mt3 <- dfm_weight(mt2, "logcount", force = TRUE)
    expect_equal(mt3@meta$object$weight_tf$scheme, "logcount")
})

test_that("force argument works as expected (#1545)", {
    corp <- corpus(c("He went out to buy a car", 
                     "He went out and bought pickles and onions",
                     "He stayed home instead."),
                   docvars = data.frame(grp = c(1, 1, 2)))
    dfmat <- dfm(corp)
    dfmat_tfprop <- dfm_weight(dfmat, "prop")
    dfmat_tfidf <- dfm_tfidf(dfmat)

    expect_is(dfm_group(dfmat_tfprop, groups = "grp", force = FALSE), "dfm")
    expect_is(dfm_group(dfmat_tfprop, groups = "grp", force = TRUE), "dfm")
    
    expect_error(
        dfm_group(dfm_group(dfmat_tfidf, groups = "grp", force = FALSE)),
        "will not group a weighted dfm; use force = TRUE to override"
    )
    expect_is(dfm_group(dfmat_tfidf, groups = "grp", force = TRUE), "dfm")
})

test_that("group_docvar drops list column (#1553)", {
    data <- data.frame("docname_" = c("A", "B", "C", "D"),
                       "docid_" = factor(c("text1", "text2", "text2", "text3")),
                       "segid_" = c(1L, 1L, 1L, 1L),
                       vec1 = c(1, 3, 3, 6),
                       vec2 = c("a", "b", "b", "c"),
                       stringsAsFactors = FALSE)
    data$lis <- list(1:3, -5, 3:4, 1)
    expect_equal(quanteda:::group_docvars(data, factor(c(1, 2, 2, 3))),
                 data.frame("docname_" = c("1", "2", "3"),
                            "docid_" = factor(c("1", "2", "3")),
                            "segid_" = c(1L, 1L, 1L),
                            vec1 = c(1, 3, 6),
                            vec2 = c("a", "b", "c"),
                            stringsAsFactors = FALSE))
    
    corp <- corpus(c("a a c d", "s i k e", "k a i e", "z o p"),
                   docvars = data.frame(vec1 = c(1, 3, 3, 6),
                                        vec2 = c("a", "b", "b", "c"),
                                        stringsAsFactors = FALSE))
    mt <- dfm(corp)
    expect_equal(docvars(dfm_group(mt, c(1, 2, 2, 3))),
                 data.frame(data.frame(vec1 = c(1, 3, 6),
                                       vec2 = c("a", "b", "c"),
                                       stringsAsFactors = FALSE)))
})


test_that("restore original unit when groups = NULL", {
    corp <- corpus(data_corpus_inaugural) # normalize hyphens
    corp <- head(corp, 2)
    corp_sent <- corpus_reshape(corp)
    dfmt_sent <- dfm(corp_sent)
    dfmt <- dfm_group(dfmt_sent)
    expect_equal(ndoc(corp), ndoc(dfmt))
    expect_equal(ndoc(corp_sent), ndoc(dfmt_sent))
    expect_equal(as.matrix(dfmt), as.matrix(dfm(corp)))
})

test_that("dfm_group works with NA group labels", {
    corp <- corpus(c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA"),
                   docvars = data.frame(factorvar = c("Yes", "Yes", "No", NA, NA)))
    expect_identical(
        dfm(corp, groups = "factorvar"),
        dfm(corp[1:3], groups = "factorvar")
    )
    # expect_identical(
    #     dfm(corp) %>% dfm_group(groups = "factorvar"),
    #     dfm(corp[1:3]) %>% dfm_group(groups = "factorvar")
    # )
})

test_that("displayed matrix rownames are correct after dfm_group (#1949)", {
    expect_identical(
        docnames(dfm(letters[1:3], groups = c("x", "x", "y"))),
        c("x", "y")
    )
    expect_output(
        print(dfm(letters[1:3], groups = c("x", "x", "y"))),
        "Document-feature matrix of: 2 documents, 3 features (50.0% sparse).
    features
docs a b c
   x 1 1 0
   y 0 0 1",
        fixed = TRUE
    )
})
