context("test texts")

test_that("test texts: general", {
    corp <- corpus(c(d1 = "This is first document", 
                     d2 = "This makes up a second text.",
                     d3 = "something completely different"),
                   docvars = data.frame(bool = c(TRUE, FALSE, TRUE),
                                        label_factor = factor(c("A", "B", "A")),
                                        label_txt = c("A", "B", "A")))
    expect_equal(texts(corp)[1], 
                 c(d1 = "This is first document"))
})

test_that("test texts with groups", {
    txt <- c(d1 = "This is first document", 
             d2 = "This makes up a second text.",
             d3 = "something completely different")
    corp <- corpus(txt, docvars = data.frame(bool = c(TRUE, FALSE, TRUE),
                                             label_factor = factor(c("A", "B", "A")),
                                             label_txt = c("A", "B", "A")))

    expect_identical(texts(txt, groups = c(TRUE, FALSE, TRUE))[2], 
                     c("TRUE" = "This is first document something completely different"))
    expect_identical(texts(txt, groups = factor(c("A", "B", "A")))[1], 
                     c(A = "This is first document something completely different"))
    expect_identical(texts(txt, groups = c("A", "B", "A"))[1], 
                     c(A = "This is first document something completely different"))

    expect_equal(texts(corp, groups = "bool")[2], 
                 c("TRUE" = "This is first document something completely different"))
    expect_equal(texts(corp, groups = as.factor(docvars(corp, "bool")))[2], 
                 c("TRUE" = "This is first document something completely different"))

    expect_equal(texts(corp, groups = "label_factor")[1], 
                 c(A = "This is first document something completely different"))
    expect_equal(texts(corp, groups = docvars(corp, "label_factor"))[1], 
                 c(A = "This is first document something completely different"))
    
    expect_equal(texts(corp, groups = "label_txt")[1], 
                 c(A = "This is first document something completely different"))
    expect_equal(texts(corp, groups = docvars(corp, "label_txt"))[1], 
                 c(A = "This is first document something completely different"))
    
    expect_error(texts(corp, groups = "label_txt2"),
                 "groups must name docvars or provide data matching the documents in x")
    expect_error(texts(corp, groups = 1:4),
                 "groups must name docvars or provide data matching the documents in x")
})

test_that("as.character.corpus same as texts.corpus", {
    expect_identical(texts(data_corpus_inaugural), 
                     as.character(data_corpus_inaugural))
})

test_that("text assignment works for corpus", {
    corp <- corpus(c(a = "text one", b = "text two"))
    texts(corp) <- c("new text one", "new text two")
    expect_identical(
        texts(corp),
        c(a = "new text one", b = "new text two")
    )
})

test_that("as.corpus.corpuszip works (legacy only", {
    load("../data/pre_v2_objects/data_corpus_zipprev2.rda")
    expect_identical(
        texts(as.corpus(data_corpus_zipprev2)),
        c(a = "text one", b = "text two")
    )
})

test_that("groups drops NA", {
    txt <- c("Doc 1", "Doc 1b", "Doc2", "Doc 3 with NA", "Doc 4, more NA")
    grvar <- c("Yes", "Yes", "No", NA, NA)
    expect_identical(
        texts(txt, groups = grvar),
        c(No = "Doc2", Yes = "Doc 1 Doc 1b")
    )
    expect_identical(
        texts(corpus(txt), groups = grvar),
        c(No = "Doc2", Yes = "Doc 1 Doc 1b")
    )
})
