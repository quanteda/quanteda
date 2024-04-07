test_that("message_select works as expected", {
    expect_message(
        quanteda:::message_select("remove", 10, 5, 0, 0),
        "removed 10 features"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 0, 0),
        " and 5 documents"
    )
    
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 0),
        "removed 1 feature"
    )
    expect_message(
        quanteda:::message_select("remove", 5, 1, 0, 0),
        " and 1 document$"
    )
    expect_message(
        quanteda:::message_select("remove", 0, 0, 0, 0),
        "removed 0 features$"
    )

    expect_message(
        quanteda:::message_select("select", 1000, 1000000, 0, 0),
        "removed 1,000 features"
    )
    expect_message(
        quanteda:::message_select("select", 1000, 1000000, 0, 0),
        " and 1,000,000 documents$"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 2, 3),
        "2 features"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 2, 3),
        "3 documents"
    )
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 1),
        ", padded"
    )
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 1),
        "1 document"
    )
    expect_message(
        quanteda:::message_select("remove", 5, 1, 1, 0),
        "1 feature"
    )
})

test_that("pipes work", {
    expect_true(!"package:magrittr" %in% search())
    expect_equal(
        tokens(char_tolower("A B C")),
        tokens("A B C") |> tokens_tolower()
    )
})

test_that("get_package_version works", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")
    expect_true(quanteda:::get_object_version(data_corpus_pre2) == "1.4.0")
    expect_true(quanteda:::is_pre2(data_corpus_pre2))
    expect_true(quanteda:::get_object_version(corpus("one")) > "1.4.9")
    
    load("../data/pre_v2_objects/data_tokens_pre2.rda")
    expect_true(quanteda:::get_object_version(data_tokens_pre2) == "1.4.0")
    expect_true(quanteda:::is_pre2(data_tokens_pre2))
    expect_true(quanteda:::get_object_version(tokens("one")) > "1.4.9")
    
    load("../data/pre_v2_objects/data_dfm_pre2.rda")
    expect_true(quanteda:::get_object_version(data_dfm_pre2) == "1.4.0")
    expect_true(quanteda:::is_pre2(data_dfm_pre2))
    expect_true(quanteda:::get_object_version(dfm(tokens("one"))) > "1.4.9")
})

test_that("resample works with sizes", {
    grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
    
    # resample works without group 
    tmp <- quanteda:::resample(1:10, size = 5, replace = FALSE)
    expect_equal(length(tmp), 5)
    
    tmp <- quanteda:::resample(1:10, size = 20, replace = TRUE)
    expect_equal(length(tmp), 20)

    expect_error(quanteda:::resample(1:10, size = 20, replace = FALSE),
                 "size cannot exceed the number of items when replace = FALSE")
    
    # sampling same size each group
    tmp <- quanteda:::resample(1:10, size = 2, replace = TRUE, by = grvec)
    expect_true(all(tmp[1:2] >= 0 & tmp[1:2] <= 3))
    expect_true(all(tmp[3:4] >= 4 & tmp[3:4] <= 7))
    expect_true(all(tmp[5:6] >= 8 & tmp[5:6] <= 10))
    
    # sampling from a vector of sizes
    tmp <- quanteda:::resample(1:10, size = c(1, 1, 3), replace = TRUE, by = grvec)
    expect_true(all(tmp[1] >= 0 & tmp[1] <= 3))
    expect_true(all(tmp[2] >= 4 & tmp[2] <= 7))
    expect_true(all(tmp[3:5] >= 8 & tmp[3:5] <= 10))
    
    # default group size sampling
    tmp <- quanteda:::resample(1:10, replace = TRUE, by = grvec)
    expect_true(all(tmp[1:3] >= 0 & tmp[1:3] <= 3))
    expect_true(all(tmp[4:7] >= 4 & tmp[4:7] <= 7))
    expect_true(all(tmp[8:10] >= 8 & tmp[8:10] <= 10))
    
    # sampling with prob
    tmp <- quanteda:::resample(1:2, 10, prob = c(1, 0), replace = TRUE)
    expect_equal(tmp, rep(1, 10))
    
    # oversampling within group
    tmp <- quanteda:::resample(c(1:2, c(101:102)), size = 5, replace = TRUE, 
                               by = rep(letters[1:2], each = 2))
    expect_true(all(tmp[1:5] >= 0 & tmp[1:5] <= 2))
    expect_true(all(tmp[6:10] >= 100 & tmp[6:10] <= 102))
    
    expect_error(
        quanteda:::resample(1:10, size = c(2, 3), replace = TRUE, by = grvec[1:6]),
        "x and by must have the same length"
    )
    expect_error(
        quanteda:::resample(1:10, size = c(2, 3), replace = TRUE, by = grvec),
        "size and by must have the same length"
    )
    expect_error(
        quanteda:::resample(1:10, size = 5, replace = FALSE, by = grvec),
        "size cannot exceed the number of items within group when replace = FALSE"
    )
    expect_error(
        quanteda:::resample(1:10, 10, prob = rep(0.1, 10), replace = TRUE, by = grvec),
        "prob cannot be used with by"
    )
})

test_that("resample works with groups of size 1", {
    grvec <- c(rep("a", 3), rep("b", 1))
    for (i in seq_len(10)) {
        tmp <- quanteda:::resample(1:4, replace = TRUE, by = grvec)
        expect_true(all(tmp[1:3] >= 1 & tmp[1:3] <= 3))
        expect_equal(tmp[4], 4)
    }
})

test_that("max_load_factor can be configured", {
    expect_equal(quanteda:::cpp_get_load_factor(),
                 list(pattern = 0.05, ngrams = 0.25), tolerance = 0.001)
    
})

test_that("is_pre2 can detect docvars failed to upgrade (#2097)", {
    
    corp <- data_corpus_inaugural[1:5]
    toks <- tokens(corp)
    dfmt <- dfm(toks)
    id <- docid(data_corpus_inaugural[1:5])
    name <- docnames(data_corpus_inaugural[1:5])
    
    attr(corp, "docvars")[c("docname_", "docid_", "segid_")] <- NULL
    expect_equal(docid(as.corpus(corp)), id)
    expect_equal(docnames(as.corpus(corp)), name)
    
    attr(toks, "docvars")[c("docname_", "docid_", "segid_")] <- NULL
    expect_equal(docid(as.tokens(toks)), id)
    expect_equal(docnames(as.tokens(toks)), name)
    
    attr(dfmt, "docvars")[c("docname_", "docid_", "segid_")] <- NULL
    expect_equal(docid(as.dfm(dfmt)), id)
    expect_equal(docnames(as.dfm(dfmt)), name)
    
})

test_that("info_tbb() is working", {
    lis <- info_tbb()
    expect_equal(names(lis), c("enabled", "max_threads"))
    expect_type(lis$enabled, "logical")
    expect_type(lis$max_threads, "integer")
})
