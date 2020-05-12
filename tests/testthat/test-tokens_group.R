context("test tokens_group")

test_that("test that tokens_group is working", {
    txt <- c("a b c d", "e f g h", "A B C", "X Y Z")
    toks <- tokens(txt)
    expect_equal(
        as.list(quanteda:::tokens_group(toks, c(1, 1, 2, 2))),
        list("1" = c("a", "b", "c", "d", "e", "f", "g", "h"),
             "2" = c("A", "B", "C", "X", "Y", "Z"))
    )

    expect_equal(
        as.list(quanteda:::tokens_group(toks, c(2, 1, 2, 1))),
        list("1" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "2" = c("a", "b", "c", "d", "A", "B", "C"))
    )

    expect_equal(
        as.list(quanteda:::tokens_group(toks, c("Z", "A", "Z", "A"))),
        list("A" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "Z" = c("a", "b", "c", "d", "A", "B", "C"))
    )

    group <- factor(c("Z", "A", "Z", "A"), levels = c("A", "B", "Z"))
    expect_equal(
        as.list(quanteda:::tokens_group(toks, group)),
        list("A" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "Z" = c("a", "b", "c", "d", "A", "B", "C"))
    )

    expect_equal(
        as.list(quanteda:::tokens_group(toks, group, fill = TRUE)),
        list("A" = c("e", "f", "g", "h", "X", "Y", "Z"),
             "B" = character(),
             "Z" = c("a", "b", "c", "d", "A", "B", "C"))
    )
})

test_that("tokens_group works with empty documents", {
    toks <- tokens(c(doc1 = "a b c c", doc2 = "b c d", doc3 = ""))
    expect_equivalent(
        as.list(quanteda:::tokens_group(toks, c("doc1", "doc1", "doc2"))),
        list(doc1 = c("a", "b", "c", "c", "b", "c", "d"), doc2 = character())
    )

    expect_equivalent(
        as.list(quanteda:::tokens_group(toks, c(1, 1, 2))),
        list(doc1 = c("a", "b", "c", "c", "b", "c", "d"), doc2 = character())
    )
})

test_that("dfm_group and tokens_group are equivalent", {
    txts <- c("a b c c", "b c d", "a")
    toks <- tokens(txts)

    expect_identical(
        dfm_group(dfm(toks), c("doc1", "doc1", "doc2")),
        dfm(quanteda:::tokens_group(toks, c("doc1", "doc1", "doc2"))))

    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 2)),
        dfm(quanteda:::tokens_group(toks, c(1, 1, 2))))

    expect_identical(
        dfm_group(dfm(toks), c(1, 1, 1)),
        dfm(quanteda:::tokens_group(toks, c(1, 1, 1))))
})

test_that("generate_groups works for tokens objects", {
    corp <- tail(data_corpus_inaugural, 14)
    docvars(corp, "Party") <- factor(docvars(corp, "Party"))
    toks <- tokens(corp)
    expect_equal(
        quanteda:::generate_groups(toks, rep(c("A", "B"), each = 7)),
        factor(rep(c("A", "B"), each = 7))
    )
    expect_equal(
        quanteda:::generate_groups(toks, factor(rep(c("A", "B"), each = 7))),
        factor(rep(c("A", "B"), each = 7))
    )
    expect_equal(
        quanteda:::generate_groups(toks, factor(rep(c(1, 2), each = 7))),
        factor(rep(c(1, 2), each = 7))
    )
    expect_equal(
        quanteda:::generate_groups(toks, "Party"),
        factor(docvars(corp, "Party"))
    )
    expect_error(
        quanteda:::generate_groups(toks, rep(c("A", "B"), each = 6)),
        "groups must name docvars or provide data matching the documents in x"
    )
})

test_that("generate_groups works for corpus objects", {
    corp <- as.corpus(tail(data_corpus_inaugural, 14))
    docvars(corp, "Party") <- factor(docvars(corp, "Party"))
    expect_equal(
        quanteda:::generate_groups(corp, rep(c("A", "B"), each = 7)),
        factor(rep(c("A", "B"), each = 7))
    )
    expect_equal(
        quanteda:::generate_groups(corp, factor(rep(c("A", "B"), each = 7))),
        factor(rep(c("A", "B"), each = 7))
    )
    expect_equal(
        quanteda:::generate_groups(corp, factor(rep(c(1, 2), each = 7))),
        factor(rep(c(1, 2), each = 7))
    )
    expect_equal(
        quanteda:::generate_groups(corp, "Party"),
        factor(docvars(corp, "Party"))
    )
    expect_error(
        quanteda:::generate_groups(corp, rep(c("A", "B"), each = 6)),
        "groups must name docvars or provide data matching the documents in x"
    )
})

test_that("tokens_group works with NA group labels", {
    corp <- corpus(c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA"),
                   docvars = data.frame(factorvar = c("Yes", "Yes", "No", NA, NA)))
    toks <- tokens(corp) %>%
        quanteda:::tokens_group(groups = "factorvar")
    expect_identical(
        as.list(toks),
        list(No = "Doc2", Yes = c("Doc", "1", "Doc", "1b"))
    )
})

test_that("element names are correctly reset after tokens_group() - #1949", {
    expect_identical(
        tokens(letters[1:3]) %>% 
            quanteda:::tokens_group(groups = c("x", "x", "y")) %>%
            names(),
        c("x", "y")
    )
})
