skip_if_not_installed("torch")
skip_if(inherits(try(torch::torch_tensor(1L), silent = TRUE), "try-error"),
        "torch is not fully configured")


test_that("as.tensor.tokens works correctly", {
    
    toks <- tokens(c(doc1 = "a b c d e f g",
                     doc2 = "g d e e",
                     doc3 = ""))
    
    # Test basic conversion
    tens1 <- as.tensor(toks)
    expect_true(inherits(tens1, "torch_tensor"))
    expect_true(tens1$is_sparse())
    expect_equal(
        as.matrix(tens1$to_dense()),
        rbind(c(1, 2, 3, 4, 5, 6, 7),
              c(7, 4, 5, 5, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0))
    )
    
    # Test with length parameter
    tens2 <- as.tensor(toks, length = 8)
    expect_true(inherits(tens2, "torch_tensor"))
    expect_true(tens2$is_sparse())
    expect_equal(
        as.matrix(tens2$to_dense()),
        rbind(c(1, 2, 3, 4, 5, 6, 7, 0),
              c(7, 4, 5, 5, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0))
    )
    
    tens3 <- as.tensor(toks, length = 5)
    expect_true(inherits(tens3, "torch_tensor"))
    expect_true(tens3$is_sparse())
    expect_equal(
        as.matrix(tens3$to_dense()),
        rbind(c(1, 2, 3, 4, 5),
              c(6, 4, 5, 5, 0),
              c(0, 0, 0, 0, 0))
    )
    
    # Test empty tokens
    tens4 <- as.tensor(tokens(c(doc1 = "", doc2 = "")))
    expect_true(inherits(tens4, "torch_tensor"))
    expect_true(tens4$is_sparse())
    expect_equal(
        as.matrix(tens4$to_dense()),
        matrix(integer(), nrow = 2, ncol = 0)
    )
    
    # Test single document
    tens5 <- as.tensor(tokens(c(doc1 = "a b c")))
    expect_true(inherits(tens5, "torch_tensor"))
    expect_true(tens5$is_sparse())
    expect_equal(
        as.matrix(tens5$to_dense()),
        rbind(c(1, 2, 3))
    )
    
    # Test empty tokens
    tens6 <- as.tensor(as.tokens(list()))
    expect_true(inherits(tens6, "torch_tensor"))
    expect_true(tens6$is_sparse())
    expect_equal(
        as.matrix(tens6$to_dense()),
        matrix(integer(), nrow = 0, ncol = 0)
    )
    
    # Test real data
    toks_inau <- tokens(data_corpus_inaugural)
    tens7 <- as.tensor(toks_inau)
    expect_true(inherits(tens7, "torch_tensor"))
    expect_true(tens7$is_sparse())
    expect_equal(
        tens7$size(), 
        c(ndoc(toks_inau), max(ntoken(toks_inau)))
    )
})

test_that("as.tensor.tokens requires torch package", {
    
    skip_if(requireNamespace("torch", quietly = TRUE), "torch is installed")
    
    toks <- tokens(c(doc1 = "a b c"))
    expect_error(
        as.tensor(toks),
        "Package 'torch' is required"
    )
})
