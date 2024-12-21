test_that("test that tokens_group is working", {
    corp <- corpus(c("a b c d", "e f g h", "A B C", "X Y Z"),
                   docname = c("doc1", "doc1", "doc2", "doc2"),
                   unique_docnames = FALSE)
    toks <- tokens(corp)
    expect_equal(
        as.list(tokens_group(toks, c(1, 1, 2, 2))),
        list("1" = c("a", "b", "c", "d", "e", "f", "g", "h"),
             "2" = c("A", "B", "C", "X", "Y", "Z"))
    )
    
    expect_equal(
        as.list(tokens_group(toks)),
        list("doc1" = c("a", "b", "c", "d", "e", "f", "g", "h"),
             "doc2" = c("A", "B", "C", "X", "Y", "Z"))
    )

    expect_equal(
        as.list(tokens_group(toks, c(2, 1, 2, 1))),
        list("1" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "2" = c("a", "b", "c", "d", "A", "B", "C"))
    )

    expect_equal(
        as.list(tokens_group(toks, c("Z", "A", "Z", "A"))),
        list("A" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "Z" = c("a", "b", "c", "d", "A", "B", "C"))
    )

    group <- factor(c("Z", "A", "Z", "A"), levels = c("A", "B", "Z"))
    expect_equal(
        as.list(tokens_group(toks, group)),
        list("A" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "Z" = c("a", "b", "c", "d", "A", "B", "C"))
    )

    expect_equal(
        as.list(tokens_group(toks, group, fill = TRUE)),
        list("A" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "B" = character(),
             "Z" = c("a", "b", "c", "d", "A", "B", "C"))
    )
    
    expect_message(
        tokens_group(toks, verbose = TRUE),
        "tokens_group() changed", fixed = TRUE
    )
})

test_that("tokens_group works with empty documents", {
    toks <- tokens(c(doc1 = "a b c c", doc2 = "b c d", doc3 = ""))
    expect_equivalent(
        as.list(tokens_group(toks, c("doc1", "doc1", "doc2"))),
        list(doc1 = c("a", "b", "c", "c", "b", "c", "d"), doc2 = character())
    )

    expect_equivalent(
        as.list(tokens_group(toks, c(1, 1, 2))),
        list(doc1 = c("a", "b", "c", "c", "b", "c", "d"), doc2 = character())
    )
})

test_that("dfm_group and tokens_group are equivalent", {
    txt <- c("a b c c", "b c d", "a")
    toks <- tokens(txt)

    expect_identical(
        dfm_group(dfm(toks), c("doc1", "doc1", "doc2")),
        dfm(tokens_group(toks, c("doc1", "doc1", "doc2"))))

    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 2)),
        dfm(tokens_group(toks, c(1, 1, 2))))

    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 1)),
        dfm(tokens_group(toks, c(1, 1, 1))))
})

test_that("test tokens_group with wrongly dimensioned groups variables", {
    grpvar <- c("D", "D", "A", "C")
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = grpvar, stringsAsFactors = FALSE))
    toks <- tokens(corp)
    expect_error(
        tokens_group(toks, groups = c(1, 1, 2, 3, 3), fill = FALSE),
        "groups must have length ndoc(x)", fixed = TRUE
    )
    expect_error(
        tokens_group(toks, groups = c(1, 1, 2, 3, 3), fill = TRUE),
        "groups must have length ndoc(x)", fixed = TRUE
    )
    expect_error(
        tokens_group(toks, groups = c(1, 1, 2, 3, 4), fill = TRUE),
        "groups must have length ndoc(x)", fixed = TRUE
    )
})

test_that("tokens_group works with NA group labels", {
    corp <- corpus(c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA"),
                   docvars = data.frame(factorvar = c("Yes", "Yes", "No", NA, NA)))
    toks <- tokens(corp) |>
        tokens_group(groups = factorvar)
    expect_identical(
        as.list(toks),
        list(No = "Doc2", Yes = c("Doc", "1", "Doc", "1b"))
    )
})

test_that("element names are correctly reset after tokens_group() - #1949", {
    expect_identical(
        tokens(letters[1:3]) |> 
            tokens_group(groups = c("x", "x", "y")) |>
            names(),
        c("x", "y")
    )
})


test_that("tokens_group save grouping variable (#2037)", {
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = factor(c("D", "D", "A", "C"), levels = c("A", "B", "C", "D")), 
                                        var1 = c(1, 1, 2, 2),
                                        var2 = c(1, 1, 2, 2), 
                                        var3 = c("x", "x", "y", NA),
                                        var4 = c("x", "y", "y", "x"),
                                        var5 = as.Date(c("2018-01-01", "2018-01-01", "2015-03-01", "2012-12-15")),
                                        var6 = as.Date(c("2018-01-01", "2015-03-01", "2015-03-01", "2012-12-15")),
                                        stringsAsFactors = FALSE))
    
    toks <- tokens(corp)
    grpvar <- factor(c("E", "E", "F", "G"), levels = c("E", "F", "G", "H"))
    toks_grp1 <- tokens_group(toks, grp)
    toks_grp2 <- tokens_group(toks, grpvar)
    toks_grp3 <- tokens_group(toks, var1)
    toks_grp4 <- tokens_group(toks, grp, fill = TRUE)
    toks_grp5 <- tokens_group(toks, grpvar, fill = TRUE)
    toks_grp6 <- tokens_group(toks, var1, fill = TRUE)
    toks_grp7 <- tokens_group(toks, groups = interaction(var1, var3))
    
    expect_equal(
        docvars(toks_grp1, "grp"), 
        factor(c("A", "C", "D"), levels = c("A", "C", "D"))
    )
    expect_equal(docvars(toks_grp1)$var1, c(2, 2, 1))
    expect_null(docvars(toks_grp2)$grpvar)
    expect_equal(docvars(toks_grp2)$var1, c(1, 2, 2))
    expect_equal(docvars(toks_grp3)$var1, c(1, 2))
    expect_equal(
        docvars(toks_grp4, "grp"), 
        factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D"))
    )
    expect_equal(docvars(toks_grp4)$var1, c(2, NA, 2, 1))
    expect_null(docvars(toks_grp5)$grpvar)
    expect_equal(docvars(toks_grp5)$var1, c(1, 2, 2, NA))
    expect_equal(docvars(toks_grp6)$var1, c(1, 2))
    expect_equal(
        docvars(toks_grp7, "grp"), 
        factor(c("D", "A"), levels = c("A", "B", "C", "D"))
    )
    expect_equal(docvars(toks_grp7)$var1, c(1, 2))
})

test_that("tokens_group drop document for NA", {
    
    corp <- corpus(c("a b c c", "b c d", "a", "b d d"),
                   docvars = data.frame(grp = factor(c(NA, NA, "A", "C"), levels = c("A", "B", "C", "D")), 
                                        var1 = c(1, 1, 2, 2),
                                        var2 = c("x", "x", "y", NA),
                                        stringsAsFactors = FALSE))
    toks <- tokens(corp)
    expect_equal(attr(tokens_group(toks, grp), "docvars"),
                 data.frame(docname_ = c("A", "C"),
                            docid_ = factor(c("A", "C"), levels = c("A", "C")),
                            segid_ = c(1L, 1L),
                            grp = factor(c("A", "C"), levels = c("A", "C")), 
                            var1 = c(2, 2),
                            var2 = c("y", NA),
                            stringsAsFactors = FALSE))
    
    expect_equal(attr(tokens_group(toks, grp, fill = TRUE), "docvars"),
                 data.frame(docname_ = c("A", "B", "C", "D"),
                            docid_ = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")),
                            segid_ = c(1L, 1L, 1L, 1L),
                            grp = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")), 
                            var1 = c(2, NA, 2, NA),
                            var2 = c("y", NA, NA, NA),
                            stringsAsFactors = FALSE))
})
