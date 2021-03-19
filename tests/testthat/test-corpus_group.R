test_that("test that corpus_group is working", {
    txt <- c("a b c d", "e f g h", "A B C", "X Y Z")
    corp <- corpus(txt)
    expect_equal(
        as.list(corpus_group(corp, c(1, 1, 2, 2))),
        list("1" = c("a b c d e f g h"),
             "2" = c("A B C X Y Z"))
    )

    expect_equal(
        as.list(corpus_group(corp, c(2, 1, 2, 1))),
        list("1" = c("e f g h X Y Z"),
             "2" = c("a b c d A B C"))
    )

    expect_equal(
        as.list(corpus_group(corp, c("Z", "A", "Z", "A"))),
        list("A" = c("e f g h X Y Z"),
             "Z" = c("a b c d A B C"))
    )

    group <- factor(c("Z", "A", "Z", "A"), levels = c("A", "B", "Z"))
    expect_equal(
        texts(corpus_group(corp, group)),
        c("A" = "e f g h X Y Z",
          "Z" = "a b c d A B C")
    )
    
    expect_equal(
        texts(corpus_group(corp, group, concatenator = " + ")),
        c("A" = "e f g h + X Y Z",
          "Z" = "a b c d + A B C")
    )

    expect_equal(
        texts(corpus_group(corp, group, fill = TRUE)),
        c("A" = "e f g h X Y Z",
          "B" = "",
          "Z" = "a b c d A B C")
    )
})

test_that("corpus_group works with empty documents", {
    corp <- corpus(c(doc1 = "a b c c", doc2 = "b c d", doc3 = ""))
    expect_equivalent(
        texts(corpus_group(corp, c("doc1", "doc1", "doc2"))),
        c(doc1 = "a b c c b c d", doc2 = "")
    )

    expect_equivalent(
        texts(corpus_group(corp, c(1, 1, 2))),
        c(doc1 = "a b c c b c d", doc2 = "")
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

test_that("corpus_group works with NA group labels", {
    corp <- corpus(c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA"),
                   docvars = data.frame(factorvar = c("Yes", "Yes", "No", NA, NA)))
    corp <- corpus(corp) %>%
        corpus_group(groups = factorvar)
    expect_identical(
        texts(corp),
        c(No = "Doc2", Yes = "Doc 1 Doc 1b")
    )
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
