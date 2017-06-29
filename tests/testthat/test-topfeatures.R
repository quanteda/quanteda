context("tests of teopfeatures")

test_that("topfeatures works with both scheme types", {
    mydfm <- dfm(c("This is a test", "Also also also a test", "This is also an odd test"))
    expect_equal(
        topfeatures(mydfm),
        c(also = 4, test = 3, this = 2, is = 2, a = 2, an = 1, odd = 1) 
    )
    expect_equal(
        topfeatures(mydfm, scheme = "docfreq"),
        c(test = 3, this = 2, is = 2, a = 2, also = 2, an = 1, odd = 1) 
    )
})

test_that("topfeatures works with decreasing = FALSE", {
    mydfm <- dfm(c("This is a test", "Also also also a test", "This is also an odd test"))
    expect_equal(
        topfeatures(mydfm, decreasing = FALSE),
        sort(c(also = 4, test = 3, this = 2, is = 2, a = 2, an = 1, odd = 1)) 
    )
    expect_equal(
        topfeatures(mydfm, scheme = "docfreq", decreasing = FALSE),
        sort(c(test = 3, this = 2, is = 2, a = 2, also = 2, an = 1, odd = 1))
    )
})

test_that("topfeatures works with n greater than length of features", {
    mydfm <- dfm(c("This is a test", "Also also also a test", "This is also an odd test"))
    expect_equal(
        topfeatures(mydfm, n = 20),
        c(also = 4, test = 3, this = 2, is = 2, a = 2, an = 1, odd = 1)  
    )
})

test_that("topfeatures grouping is working", {
    mydfm <- dfm(data_corpus_inaugural[1:10])
    expect_equal(names(topfeatures(mydfm, groups = docnames(mydfm))),
                 docnames(mydfm))
    expect_equal(topfeatures(mydfm, groups = docnames(mydfm))[[5]],
                 topfeatures(mydfm[5,]))
    expect_equal(topfeatures(mydfm, decreasing = TRUE, groups = docnames(mydfm))[[10]],
                 topfeatures(mydfm[10,], decreasing = TRUE))

    mydfm <- dfm(c("This is a test", "Also also also a test", "This is also an odd test"))
    grps <- c("a", "a", "b")
    expect_equal(topfeatures(mydfm, scheme = "count", groups = grps)[["a"]],
                 sort(colSums(mydfm[1:2, ]), decreasing = TRUE))
    expect_equal(topfeatures(mydfm, scheme = "count", groups = grps)[["b"]],
                 sort(colSums(mydfm[3, ]), decreasing = TRUE))
    
    ## not implemented yet
    # expect_equal(topfeatures(mydfm, scheme = "docfreq", groups = grps)[["a"]],
    #              sort(docfreq(mydfm[1:2, ]), decreasing = TRUE))
    # expect_equal(topfeatures(mydfm, scheme = "docfreq", groups = grps)[["b"]],
    #              sort(docfreq(mydfm[3, ]), decreasing = TRUE))
    expect_error(
        topfeatures(mydfm, scheme = "docfreq", groups = grps),
        "docfreq not yet implemented for groups"
    )
})
