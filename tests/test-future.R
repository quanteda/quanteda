context("test v2 corpus functions")

load("../tests/data/corpora/corpv2.rda")

test_that("docnames for v2 corpus works", {
    expect_identical(
        docnames(corpv2),
        c("fromDf_1", "fromDf_2", "fromDf_3", "fromDf_4", "fromDf_5", "fromDf_6")
    )
})

test_that("docvars for v2 corpus works", {
    dv <- attributes(corpv2)$docvars[, 4:6]
    rownames(dv) <- attributes(corpv2)$docvars[, "docname_"]
    expect_identical(
        docvars(corpv2),
        dv
    )
})

test_that("corpus constructor works on v2 corpus", {
    expect_true(is.corpus(corpus(corpv2)))
    
    expect_identical(
        corpus(corpv2)$documents$texts,
        paste0("This is text number ", 1:6, ".")
    )
    
    expect_identical(
        docvars(corpus(corpv2)),
        data.frame(letter_factor = c("a", "a", "b", "b", "c", "c"),
                   some_ints = 1:6,
                   some_logical = rep(c(TRUE, FALSE), 3),
                   row.names = paste0("fromDf_", 1:6))
    )
    
    expect_identical(
        docvars(corpus(corpv2, docnames = paste0("doc", 1:6))),
        data.frame(letter_factor = c("a", "a", "b", "b", "c", "c"),
                   some_ints = 1:6,
                   some_logical = rep(c(TRUE, FALSE), 3),
                   row.names = paste0("doc", 1:6))
    )
    
    expect_identical(
        docvars(corpus(corpv2, docvars = data.frame(mylog = rep(c(TRUE, FALSE), 3)))),
        data.frame(mylog = rep(c(TRUE, FALSE), 3),
                   row.names = paste0("fromDf_", 1:6))
    )
    
    newmeta <- metacorpus(corpus(corpv2, 
                                 metacorpus = list(notes = "Test notes.",
                                                   other = "Other meta.")))
    expect_identical(newmeta$notes, "Test notes.")
    expect_identical(newmeta$other, "Other meta.")
})
