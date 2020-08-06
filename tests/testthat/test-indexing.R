context("test indexing")

test_that("test dfm indexing", {

    x <- dfm(tokens(c("this contains lots of stopwords",
                      "no if, and, or but about it: lots"),
                    remove_punct = TRUE))
    expect_equivalent(
        as.matrix(x[1:2, 1:3]),
        matrix(c(1, 0, 1, 0, 1, 1), nrow = 2)
    )
    expect_equivalent(
        as.matrix(x[1:2, 1:3, drop = FALSE]),
        matrix(c(1, 0, 1, 0, 1, 1), nrow = 2)
    )
    expect_equal(
        x[], x
    )
    expect_equal(
        x[, drop = FALSE], x
    )
    expect_error(x[1])
    expect_error(x[1:2])
    expect_error(x[i = 1])
    expect_error(x[j = 1])

    expect_equivalent(dim(x[, 1:3]), c(2, 3))
    expect_equivalent(dim(x[1:2, ]), c(2, 12))
    expect_equivalent(dim(x[, 1:3, drop = FALSE]), c(2, 3))
    expect_equivalent(dim(x[1:2, drop = FALSE]), c(2, 12))

})

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
    expect_error(x[1])
    expect_error(x[1:2])
    expect_error(x[i = 1])
    expect_error(x[j = 1])

    expect_equivalent(dim(x[, 1:3]), c(12, 3))
    expect_equivalent(dim(x[1:3, ]), c(3, 12))
    expect_equivalent(dim(x[, 1:3, drop = FALSE]), c(12, 3))
    expect_equivalent(dim(x[1:3, drop = FALSE]), c(3, 12))

})

test_that("test dfm indexing with docvar selection", {

    testcorp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
                         d3 = "b b c e", d4 = "e e f a b"),
                       docvars = data.frame(grp = c(1, 1, 2, 3)))
    testdfm <- dfm(testcorp)
    expect_equal(
        docvars(testdfm[1:2, ]),
        data.frame(grp = c(1, 1))
    )
    expect_equal(
        docvars(testdfm[c(2, 4), ]),
        data.frame(grp = c(1, 3))
    )
    expect_equal(
        docvars(testdfm[c(2, 4), c(1, 3, 5)]),
        data.frame(grp = c(1, 3))
    )
})
