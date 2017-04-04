

context('test fcm indexing')

test_that("test fcm indexing", {
    
    x <- fcm(tokens(c("this contains lots of stopwords",
                      "no if, and, or but about it: lots"),
                    remove_punct = TRUE))
    expect_equivalent(
        as.matrix(x[1:3, 1:3]),
        matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0), nrow = 3)
    )
    expect_equivalent(
        as.matrix(x[1:3, 1:3, drop = FALSE]),
        matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0), nrow = 3)
    )
    expect_equal(
        x[], x
    )
    expect_equal(
        x[, drop = FALSE], x
    )
    expect_equivalent(dim(x[, 1:3]), c(12, 3))
    expect_equivalent(dim(x[1:3, ]), c(3, 12))
    expect_equivalent(dim(x[, 1:3, drop = FALSE]), c(12, 3))
    expect_equivalent(dim(x[1:3, drop = FALSE]), c(3, 12))

})
