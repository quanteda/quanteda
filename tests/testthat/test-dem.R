library(quanteda)
library(testthat)

dfmt <- dfm(tokens(data_corpus_inaugural))
k <- 5
femt <- matrix(rnorm(ncol(dfmt) * k), ncol = k, 
               dimnames = list(featnames(dfmt), NULL)) # dummy

test_that("dem works", {
    
    demt <- dem(dfmt, y = femt)
    
    expect_s4_class(
        demt,
        "dem"
    )
    expect_identical(
        dim(demt),
        c(60L, 5L)
    )
    expect_identical(
        dim(head(demt, 10)),
        c(10L, 5L)
    )
    expect_identical(
        dim(tail(demt, 10)),
        c(10L, 5L)
    )
    expect_identical(
        dim(demt[11:20,]),
        c(10L, 5L)
    )
    expect_s4_class(
        demt[11:20,],
        "dem"
    )
    expect_s4_class(
        demt[1,],
        "dem"
    )
    expect_s4_class(
        demt[],
        "dem"
    )
    expect_equal(
        rowSums(demt),
        rowSums(as.matrix(demt))
    )
    expect_equal(
        colSums(demt),
        colSums(as.matrix(demt))
    )
    expect_equal(
        rowMeans(demt),
        rowMeans(as.matrix(demt))
    )
    expect_equal(
        colMeans(demt),
        colMeans(as.matrix(demt))
    )
    
    # verbose
    expect_silent(
        dem(dfmt, y = femt, verbose = FALSE)
    )
    expect_message(
        dem(dfmt, y = femt, verbose = TRUE),
        "Creating a dem from a dfm object",
        fixed = TRUE
    )
    expect_message(
        dem(dfmt, y = femt, verbose = TRUE),
        "Finished constructing dem of 60 documents x 5 dimensions",
        fixed = TRUE
    )
    
    # errors
    expect_error(
        demt[100:110,],
        "Subscript out of bounds",
        fixed = TRUE
    )
    expect_error(
        demt[,1:2],
        "Cannot subset columns of dem objects",
        fixed = TRUE
    )
    expect_error(
        demt[[1]],
        "[[ not defined for dem objects",
        fixed = TRUE
    )
    
})


test_that("rbind works with dem", {
    
    demt0 <- dem(dfmt, y = femt)
    demt1 <- dem(dfm_subset(dfmt, Year < 1991), y = femt)
    demt2 <- dem(dfm_subset(dfmt, Year >= 1991 & Year < 2001), y = femt)
    demt3 <- dem(dfm_subset(dfmt, Year >= 2001), y = femt)
    demt4 <- rbind(demt1, demt2, demt3)
    
    expect_equal(
        rbind(demt0),
        demt0
    )
    expect_equal(
        docvars(rbind(demt0)),
        docvars(demt0)
    )
    
    expect_equal(
        demt4,
        demt0
    )
    expect_equal(
        docvars(demt4),
        docvars(demt0)
    )
    
    # errors
    expect_error(
        rbind(demt1, demt2, matrix()),
        "all arguments must be dem objects"
    )
})

test_that("as.dem works", {
    
    mat1 <- matrix(rnorm(100), nrow = 5)
    demt1 <- as.dem(mat1)
    expect_s4_class(demt1, "dem")
    expect_true(is.dem(demt1))
    expect_equivalent(as.matrix(demt1), mat1)
    
    mat2 <- Matrix::Matrix(rnorm(100), nrow = 5)
    demt2 <- as.dem(mat2)
    expect_s4_class(demt2, "dem")
    expect_true(is.dem(demt2))
    expect_equivalent(demt2, mat2)
    
    mat3 <- matrix(nrow = 0, ncol = 0)
    demt3 <- as.dem(mat3)
    expect_s4_class(demt3, "dem")
    expect_true(is.dem(demt3))
    expect_equivalent(as.matrix(demt3), mat3)
    
})

test_that("arithematic operators work with dem", {
    
    demt <- dem(dfmt, y = femt)
    
    expect_s4_class(demt + 0.1, "dem")
    expect_s4_class(demt - 0.1, "dem")
    expect_s4_class(demt * 0.1, "dem")
    expect_s4_class(demt / 0.1, "dem")
    expect_s4_class(demt ^ 0.1, "dem")
    
    expect_s4_class(0.1 + demt, "dem")
    expect_s4_class(0.1 - demt, "dem")
    expect_s4_class(0.1 * demt, "dem")
    expect_s4_class(0.1 / demt, "dem")
    expect_s4_class(0.1 ^ demt, "dem")
    
    demt_empty <- demt[0,]
    
    expect_s4_class(demt_empty + 0.1, "dem")
    expect_s4_class(demt_empty - 0.1, "dem")
    expect_s4_class(demt_empty * 0.1, "dem")
    expect_s4_class(demt_empty / 0.1, "dem")
    expect_s4_class(demt_empty ^ 0.1, "dem")
    
    expect_s4_class(0.1 + demt_empty, "dem")
    expect_s4_class(0.1 - demt_empty, "dem")
    expect_s4_class(0.1 * demt_empty, "dem")
    expect_s4_class(0.1 / demt_empty, "dem")
    expect_s4_class(0.1 ^ demt_empty, "dem")
    
})

test_that("docnames work", {
    
    demt <- dem(dfmt, y = femt)
    expect_identical(
        docnames(demt),
        docnames(dfmt)
    )
    
    docnames(demt) <- stringi::stri_extract_first_regex(docnames(demt), "\\d+")
    expect_identical(
        docnames(demt),
        stringi::stri_extract_first_regex(docnames(dfmt), "\\d+")
    )
})

test_that("docvars work", {
    
    demt <- dem(dfmt, y = femt)
    expect_identical(
        attr(demt, "docvars"),
        attr(dfmt, "docvars")
    )
    expect_identical(
        docvars(demt),
        docvars(dfmt)
    )
    extra1 <- sample(nrow(demt))
    docvars(demt, "Extra1") <- extra1
    expect_identical(
        docvars(demt, "Extra1"),
        extra1
    )
    
    extra2 <- sample(nrow(demt))
    docvars(demt, "Extra2") <- extra2
    expect_identical(
        demt$Extra2,
        extra2
    )
    
    # errors
    expect_error(
        docvars(demt, "xxx"),
        "field(s) xxx not found",
        fixed = TRUE
    )
        
})

test_that("dem_suset work", {
    
    demt <- dem(dfmt, y = femt)
    
    expect_identical(
        dim(dem_subset(demt, Party == "Democratic")),
        c(22L, 5L)
    )
    expect_identical(
        dim(dem_subset(demt, demt$Year > 2000)),
        c(7L, 5L)
    )
    expect_identical(
        dim(dem_subset(demt)),
        c(60L, 5L)
    )
    
    # verbose
    expect_message(
        dem_subset(demt, Party == "Democratic", verbose = TRUE),
        'Returning dem of 22 documents x 5 dimensions from dem_subset'
    )
})
