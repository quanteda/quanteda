
test_that("as.Matrix.tokens works correctly", {
    
    toks <- tokens(c(doc1 = "a b c d e f g",
                     doc2 = "g d e e",
                     doc3 = ""))
    
    # Test basic conversion
    mat1 <- as.Matrix(toks)
    
    expect_true(inherits(mat1, "dgRMatrix"))
    expect_equal(
        as.matrix(mat1),
        rbind(doc1 = c(1, 2, 3, 4, 5, 6, 7),
              doc2 = c(7, 4, 5, 5, 0, 0, 0),
              doc3 = c(0, 0, 0, 0, 0, 0, 0))
    )
    
    # Test with length parameter
    mat2 <- as.Matrix(toks, length = 8)
    expect_true(inherits(mat2, "dgRMatrix"))
    expect_equal(
        as.matrix(mat2),
        rbind(doc1 = c(1, 2, 3, 4, 5, 6, 7, 0),
              doc2 = c(7, 4, 5, 5, 0, 0, 0, 0),
              doc3 = c(0, 0, 0, 0, 0, 0, 0, 0))
    )
    
    mat3 <- as.Matrix(toks, length = 5)
    expect_true(inherits(mat3, "dgRMatrix"))
    expect_equal(
        as.matrix(mat3),
        rbind(doc1 = c(1, 2, 3, 4, 5),
              doc2 = c(6, 4, 5, 5, 0),
              doc3 = c(0, 0, 0, 0, 0))
    )
    
    # Test empty tokens
    mat4 <- as.Matrix(tokens(c(doc1 = "", doc2 = "")))
    expect_true(inherits(mat4, "dgRMatrix"))
    expect_equal(
        as.matrix(mat4),
        matrix(integer(), nrow = 2, ncol = 0, 
               dimnames = list(c("doc1", "doc2"), NULL))
    )
    
    # Test single document
    mat5 <- as.Matrix(tokens(c(doc1 = "a b c")))
    expect_true(inherits(mat5, "dgRMatrix"))
    expect_equal(
        as.matrix(mat5),
        rbind(doc1 = c(1, 2, 3))
    )
    
    # Test empty tokens
    mat6 <- as.Matrix(as.tokens(list()))
    expect_true(inherits(mat6, "dgRMatrix"))
    expect_equal(
        as.matrix(mat6),
        matrix(integer(), nrow = 0, ncol = 0)
    )
    
    # Test real data
    toks_inau <- tokens(data_corpus_inaugural)
    mat7 <- as.Matrix(toks_inau)
    expect_true(inherits(mat7, "dgRMatrix"))
    expect_equal(
        dim(mat7), 
        c(ndoc(toks_inau), max(ntoken(toks_inau)))
    )
})
