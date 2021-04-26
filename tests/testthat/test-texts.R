txt_test <- c(d1 = "This is first document", 
              d2 = "This makes up a second text.",
              d3 = "something completely different")

    
test_that("test texts: general", {
    corp <- corpus(txt_test,
                   docvars = data.frame(bool = c(TRUE, FALSE, TRUE),
                                        label_factor = factor(c("A", "B", "A")),
                                        label_txt = c("A", "B", "A")))
    expect_equal(suppressWarnings(texts(corp)[1]), 
                 c(d1 = "This is first document"))
})

test_that("assignment works", {
    corp <- corpus(txt_test)
    expect_error(
        suppressWarnings(texts(corp) <- c("aaaa", "bbbbb")),
        "documents must be the same length as x"
    )
    suppressWarnings(texts(corp) <- c(12345, 0, Inf))
    expect_equal(
        suppressWarnings(texts(corp)),
        c(d1 = "12345", d2 = "0", d3 = "Inf")
    )
})

test_that("test texts with groups", {
    corp <- corpus(txt_test, docvars = data.frame(bool = c(TRUE, FALSE, TRUE),
                                             label_factor = factor(c("A", "B", "A")),
                                             label_txt = c("A", "B", "A")))

    # expect_identical(texts(txt_test, groups = c(TRUE, FALSE, TRUE))[2], 
    #                  c("TRUE" = "This is first document something completely different"))
    # expect_identical(texts(txt_test, groups = factor(c("A", "B", "A")))[1], 
    #                  c(A = "This is first document something completely different"))
    # expect_identical(texts(txt_test, groups = c("A", "B", "A"))[1], 
    #                  c(A = "This is first document something completely different"))

    expect_equal(suppressWarnings(texts(corp, groups = bool)[2]),
                 c("TRUE" = "This is first document something completely different"))
    expect_equal(suppressWarnings(texts(corp, groups = factor(corp$bool, levels = c("TRUE", "FALSE")))[1]),
                 c("TRUE" = "This is first document something completely different"))

    expect_equal(suppressWarnings(texts(corp, groups = label_factor)[1]),
                 c(A = "This is first document something completely different"))
    expect_equal(suppressWarnings(texts(corp, groups = docvars(corp, "label_factor"))[1]),
                 c(A = "This is first document something completely different"))
    
    expect_equal(suppressWarnings(texts(corp, groups = label_txt)[1]),
                 c(A = "This is first document something completely different"))
    expect_equal(suppressWarnings(texts(corp, groups = docvars(corp, "label_txt"))[1]),
                 c(A = "This is first document something completely different"))
    
    expect_error(suppressWarnings(texts(corp, groups = "label_txt2")),
                 "groups must have length ndoc(x)", fixed = TRUE)
    expect_error(suppressWarnings(texts(corp, groups = 1:4)),
                 "groups must have length ndoc(x)", fixed = TRUE)
})

test_that("as.character.corpus same as texts.corpus", {
    expect_identical(suppressWarnings(texts(data_corpus_inaugural)),
                     as.character(data_corpus_inaugural))
})

test_that("text assignment works for corpus", {
    corp <- corpus(c(a = "text one", b = "text two"))
    suppressWarnings(texts(corp) <- c("new text one", "new text two"))
    expect_identical(
        suppressWarnings(texts(corp)),
        c(a = "new text one", b = "new text two")
    )
})

test_that("groups drops NA", {
    txt <- c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA")
    grvar <- c("Yes", "Yes", "No", NA, NA)
    # expect_identical(
    #     texts(txt, groups = grvar),
    #     c(No = "Doc2", Yes = "Doc 1 Doc 1b")
    # )
    expect_identical(
        suppressWarnings(texts(corpus(txt), groups = grvar)),
        c(No = "Doc2", Yes = "Doc 1 Doc 1b")
    )
})

test_that("groups works with docvars", {
    expect_identical(
        names(suppressWarnings(texts(data_corpus_inaugural[1:5], groups = President))),
        c("Adams", "Jefferson", "Washington")
    )
    expect_identical(
        names(suppressWarnings(texts(data_corpus_inaugural[1:5], 
                    groups = factor(data_corpus_inaugural[1:5]$President, 
                                    levels = unique(data_corpus_inaugural[1:5]$President))))),
        c("Washington", "Adams", "Jefferson")
    )
    nc <- nchar(suppressWarnings(texts(data_corpus_inaugural[1:5])))
    expect_identical(
        nchar(suppressWarnings(texts(data_corpus_inaugural[1:5], groups = President, spacer = ""))),
        c(Adams = sum(nc[3]), Jefferson = sum(nc[4:5]), Washington = sum(nc[1:2]))
    )
})
