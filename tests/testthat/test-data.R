context("test data objects")

test_that("corpus data objects contain expected quantities", {
    expected_user_fields <- c("description", "source", "url", "author", "keywords", "title")
    expected_system_fields <- "summary"
    
    expect_true(all(expected_user_fields %in% names(meta(data_corpus_inaugural, type = "user"))))
    expect_true(all(expected_system_fields %in% names(meta(data_corpus_inaugural, type = "system"))))
    
    expect_true(all(expected_user_fields %in% names(meta(data_corpus_irishbudget2010, type = "user"))))
    expect_true(all(expected_system_fields %in% names(meta(data_corpus_irishbudget2010, type = "system"))))
    
    expect_true(all(expected_user_fields %in% names(meta(data_corpus_dailnoconf1991, type = "user"))))
    expect_true(all(expected_system_fields %in% names(meta(data_corpus_dailnoconf1991, type = "system"))))
})
