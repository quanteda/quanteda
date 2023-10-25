test_that("test that corpus_group is working", {
    corp <- corpus(c("a b c d", "e f g h", "A B C", "X Y Z"),
                   docname = c("doc1", "doc1", "doc2", "doc2"),
                   unique_docnames = FALSE)
    expect_equal(
        as.character(corpus_group(corp, c(1, 1, 2, 2))),
        c("1" = "a b c d e f g h",
          "2" = "A B C X Y Z")
    )
    
    expect_equal(
        as.character(corpus_group(corp)),
        c("doc1" = "a b c d e f g h",
          "doc2" = "A B C X Y Z")
    )
    
    expect_equal(
        as.character(corpus_group(corp, c(1, 1, 2, 2))),
        c("1" = "a b c d e f g h",
          "2" = "A B C X Y Z")
    )

    expect_equal(
        as.character(corpus_group(corp, c(2, 1, 2, 1))),
        c("1" = "e f g h X Y Z",
          "2" = "a b c d A B C")
    )

    expect_equal(
        as.character(corpus_group(corp, c("Z", "A", "Z", "A"))),
        c("A" = "e f g h X Y Z",
          "Z" = "a b c d A B C")
    )

    group <- factor(c("Z", "A", "Z", "A"), levels = c("A", "B", "Z"))
    expect_equal(
        as.character(corpus_group(corp, group)),
        c("A" = "e f g h X Y Z",
          "Z" = "a b c d A B C")
    )
    
    expect_equal(
        as.character(corpus_group(corp, group, concatenator = " + ")),
        c("A" = "e f g h + X Y Z",
          "Z" = "a b c d + A B C")
    )

    expect_equal(
        as.character(corpus_group(corp, group, fill = TRUE)),
        c("A" = "e f g h X Y Z",
          "B" = "",
          "Z" = "a b c d A B C")
    )
})

test_that("corpus_group works with empty documents", {
    corp <- corpus(c(doc1 = "a b c c", doc2 = "b c d", doc3 = ""))
    expect_equivalent(
        as.character(corpus_group(corp, c("doc1", "doc1", "doc2"))),
        c(doc1 = "a b c c b c d", doc2 = "")
    )

    expect_equivalent(
        as.character(corpus_group(corp, c(1, 1, 2))),
        c(doc1 = "a b c c b c d", doc2 = "")
    )
})

test_that("corpus_group works with NA group labels", {
    corp <- corpus(c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA"),
                   docvars = data.frame(factorvar = c("Yes", "Yes", "No", NA, NA)))
    corp <- corpus(corp) |>
        corpus_group(groups = factorvar)
    expect_identical(
        as.character(corp),
        c(No = "Doc2", Yes = "Doc 1 Doc 1b")
    )
})

test_that("dfm_group and corpus_group are equivalent", {
    txt <- c("a b c c", "b c d", "a")
    corp <- corpus(txt)
    toks <- tokens(corp)
    expect_identical(
        dfm_group(dfm(toks), c("doc1", "doc1", "doc2")),
        dfm(tokens(corpus_group(corp, c("doc1", "doc1", "doc2")))))
    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 2)),
        dfm(tokens(corpus_group(corp, c(1, 1, 2)))))
    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 1)),
        dfm(tokens(corpus_group(corp, c(1, 1, 1)))))
})

test_that("test corpus_group with wrongly dimensioned groups variables", {
    grpvar <- c("D", "D", "A", "C")
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = grpvar, stringsAsFactors = FALSE))
    expect_error(
        corpus_group(corp, groups = c(1, 1, 2, 3, 3), fill = FALSE),
        "groups must have length ndoc(x)", fixed = TRUE
    )
    expect_error(
        corpus_group(corp, groups = c(1, 1, 2, 3, 3), fill = TRUE),
        "groups must have length ndoc(x)", fixed = TRUE
    )
    expect_error(
        corpus_group(corp, groups = c(1, 1, 2, 3, 4), fill = TRUE),
        "groups must have length ndoc(x)", fixed = TRUE
    )
})

test_that("corpus_group save grouping variable (#2037)", {
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = factor(c("D", "D", "A", "C"), levels = c("A", "B", "C", "D")), 
                                        var1 = c(1, 1, 2, 2),
                                        var2 = c(1, 1, 2, 2), 
                                        var3 = c("x", "x", "y", NA),
                                        var4 = c("x", "y", "y", "x"),
                                        var5 = as.Date(c("2018-01-01", "2018-01-01", "2015-03-01", "2012-12-15")),
                                        var6 = as.Date(c("2018-01-01", "2015-03-01", "2015-03-01", "2012-12-15")),
                                        stringsAsFactors = FALSE))
    
    grpvar <- factor(c("E", "E", "F", "G"), levels = c("E", "F", "G", "H"))
    corp_grp1 <- corpus_group(corp, grp)
    corp_grp2 <- corpus_group(corp, grpvar)
    corp_grp3 <- corpus_group(corp, var1)
    corp_grp4 <- corpus_group(corp, grp, fill = TRUE)
    corp_grp5 <- corpus_group(corp, grpvar, fill = TRUE)
    corp_grp6 <- corpus_group(corp, var1, fill = TRUE)
    corp_grp7 <- corpus_group(corp, groups = interaction(var1, var3))
    
    expect_equal(
        docvars(corp_grp1, "grp"), 
        factor(c("A", "C", "D"), levels = c("A", "C", "D"))
    )
    expect_equal(docvars(corp_grp1)$var1, c(2, 2, 1))
    expect_null(docvars(corp_grp2)$grpvar)
    expect_equal(docvars(corp_grp2)$var1, c(1, 2, 2))
    expect_equal(docvars(corp_grp3)$var1, c(1, 2))
    expect_equal(
        docvars(corp_grp4, "grp"), 
        factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D"))
    )
    expect_equal(docvars(corp_grp4)$var1, c(2, NA, 2, 1))
    expect_null(docvars(corp_grp5)$grpvar)
    expect_equal(docvars(corp_grp5)$var1, c(1, 2, 2, NA))
    expect_equal(docvars(corp_grp6)$var1, c(1, 2))
    expect_equal(
        docvars(corp_grp7, "grp"), 
        factor(c("D", "A"), levels = c("A", "B", "C", "D"))
    )
    expect_equal(docvars(corp_grp7)$var1, c(1, 2))
})


test_that("tokens_group drop document for NA", {
    
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = factor(c(NA, NA, "A", "C"), levels = c("A", "B", "C", "D")), 
                                        var1 = c(1, 1, 2, 2),
                                        var2 = c("x", "x", "y", NA),
                                        stringsAsFactors = FALSE))

    expect_equal(attr(corpus_group(corp, grp), "docvars"),
                 data.frame(docname_ = c("A", "C"),
                            docid_ = factor(c("A", "C"), levels = c("A", "C")),
                            segid_ = c(1L, 1L),
                            grp = factor(c("A", "C"), levels = c("A", "C")), 
                            var1 = c(2, 2),
                            var2 = c("y", NA),
                            stringsAsFactors = FALSE))
    
    expect_equal(attr(corpus_group(corp, grp, fill = TRUE), "docvars"),
                 data.frame(docname_ = c("A", "B", "C", "D"),
                            docid_ = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")),
                            segid_ = c(1L, 1L, 1L, 1L),
                            grp = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")), 
                            var1 = c(2, NA, 2, NA),
                            var2 = c("y", NA, NA, NA),
                            stringsAsFactors = FALSE))
})

