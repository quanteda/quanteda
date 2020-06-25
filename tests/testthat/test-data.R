context("test data objects")

test_that("corpus data objects contain expected quantities", {
    expected_user_fields <- c("description", "source", "url", "author", "keywords", "title")
    expected_system_fields <- "summary"
    
    expect_true(all(expected_user_fields %in% names(meta(data_corpus_inaugural, type = "user"))))
    expect_true(all(expected_system_fields %in% names(meta(data_corpus_inaugural, type = "system"))))
})

test_that("data_dfm_lbgexmaple does not cause error with dfm_wordstem (#1970", {
    expect_identical(
        featnames(dfm_wordstem(data_dfm_lbgexample)),
        featnames(data_dfm_lbgexample),
    )
})
