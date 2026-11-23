library(quanteda)
library(testthat)

dfmt <- dfm(tokens(data_corpus_inaugural))
k <- 5
femt <- matrix(rnorm(ncol(dfmt) * k), ncol = k, 
               dimnames = list(featnames(dfmt), NULL)) # dummy

test_that("dem works", {
    
    demt <- dem(x = dfmt, y = femt)
    
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

test_that("dem works", {
    
    demt <- dem(x = dfmt, y = femt)
    
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
    
    demt <- dem(x = dfmt, y = femt)
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
    
    demt <- dem(x = dfmt, y = femt)
    expect_identical(
        attr(demt, "docvars"),
        attr(dfmt, "docvars")
    )
    expect_identical(
        docvars(demt),
        docvars(dfmt)
    )
    expect_error(
        docvars(demt, "xxx"),
        "field(s) xxx not found",
        fixed = TRUE
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
        
})

test_that("dem_suset work", {
    
    demt <- dem(x = dfmt, y = femt)
    
    expect_identical(
        dim(dem_subset(demt, Party == "Democratic")),
        c(22L, 5L)
    )
    expect_identical(
        dim(dem_subset(demt, demt$Year > 2000)),
        c(7L, 5L)
    )

})
