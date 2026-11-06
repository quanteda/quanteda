test_that("as.matrix.tokens works correctly", {
    
    toks <- tokens(c(doc1 = "a b c d e f g",
                     doc2 = "g d e e",
                     doc3 = ""))
    
    # Test basic conversion
    mat1 <- as.matrix(toks)
    expect_equal(
        mat1,
        rbind(doc1 = c(1, 2, 3, 4, 5, 6, 7),
              doc2 = c(7, 4, 5, 5, 0, 0, 0),
              doc3 = c(0, 0, 0, 0, 0, 0, 0))
    )
    
    # Test with length parameter
    mat2 <- as.matrix(toks, length = 8)
    expect_equal(
        mat2,
        rbind(doc1 = c(1, 2, 3, 4, 5, 6, 7, 0),
              doc2 = c(7, 4, 5, 5, 0, 0, 0, 0),
              doc3 = c(0, 0, 0, 0, 0, 0, 0, 0))
    )
    
    mat3 <- as.matrix(toks, length = 5)
    expect_equal(
        mat3,
        rbind(doc1 = c(1, 2, 3, 4, 5),
              doc2 = c(7, 4, 5, 5, 0),
              doc3 = c(0, 0, 0, 0, 0))
    )
    
    # Test empty tokens
    mat4 <- as.matrix(tokens(c(doc1 = "", doc2 = "")))
    expect_equal(
        mat4,
        rbind(doc1 = integer(),
              doc2 = integer())
    )
    
    # Test single document
    mat5 <- as.matrix(tokens(c(doc1 = "a b c")), drop = FALSE)
    expect_equal(
        mat5,
        rbind(doc1 = c(1, 2, 3))
    )
    
    # Test empty tokens
    mat6 <- as.matrix(as.tokens(list()))
    expect_equal(
        mat6,
        matrix(integer(), nrow = 0, ncol = 0, 
               dimnames = list(character(), character()))
    )
    
    # Test real data
    toks_inau <- tokens(data_corpus_inaugural)
    mat7 <- as.matrix(toks_inau)
    expect_equal(
        dim(mat7),
        c(ndoc(toks_inau), max(ntoken(toks_inau)))
    )
    
    # Test extract
    mat8 <- as.matrix(toks, length = 5, extract = c(1, 3))
    expect_equal(
        mat8,
        rbind(doc1 = c(1, 2, 3, 4, 5),
              doc3 = c(0, 0, 0, 0, 0))
    )
    
    mat9 <- as.matrix(toks, length = 5, extract = 2, drop = FALSE)
    expect_equal(
        mat9,
        rbind(doc2 = c(7, 4, 5, 5, 0))
    )
    
    mat10 <- as.matrix(toks, length = 5, extract = 2, drop = TRUE)
    expect_equal(
        mat10,
        c(7, 4, 5, 5, 0)
    )
})
