
test_that("textstat_lexdiv computation is correct", {
    mydfm <- dfm(c(d1 = "b a b a b a b a",
                   d2 = "a a b b"))
    
    expect_equivalent(
        c(0.25, 0.5),
        textstat_lexdiv(mydfm, "TTR")
    )
})

test_that("textstat_lexdiv drop works", {
    mydfm <- dfm(c(d1 = "b a b a b a b a",
                   d2 = "a a b b"))

    results <- textstat_lexdiv(mydfm, "TTR", drop = FALSE)
    
    expect_equivalent(
        c(0.25, 0.5),
        results$TTR
    )
    
    expect_equal(
        row.names(results)[1], "d1"
    )
})

test_that("textstat_lexdiv CTTR works correct", {
    mydfm <- dfm(c(d1 = "b a b a b a b a",
                   d2 = "a a b b"))
    
    expect_equivalent(
        round(c(2/sqrt(2*8), 2/sqrt(2*4)), 6),
        round(textstat_lexdiv(mydfm, "CTTR"), 6)
    )
})

test_that("textstat_lexdiv R works correct", {
    mydfm <- dfm(c(d1 = "b a b a b a b a",
                   d2 = "a a b b"))
    
    expect_equivalent(
        round(c(2/sqrt(8), 2/sqrt(4)), 6),
        round(textstat_lexdiv(mydfm, "R"), 6)
    )
})

test_that("textstat_lexdiv C works correct", {
    mydfm <- dfm(c(d1 = "b a b a b a b a",
                   d2 = "a a b b"))
    
    expect_equivalent(
        round(c(log10(2)/log10(8), log10(2)/log10(4)), 6),
        round(textstat_lexdiv(mydfm, "C"), 6)
    )
})

test_that("textstat_lexdiv Maas works correct", {
    mydfm <- dfm(c(d1 = "b a b a b a b a",
                   d2 = "a a b b"))
    
    expect_equivalent(
        round(sqrt((log10(8) - log10(2))/log10(8)^2), 6),
        round(textstat_lexdiv(mydfm, "Maas")$Maas[1], 6)
    )
})

test_that("textstat_lexdiv works with a single document dfm (#706)", {
    mytxt <- "one one two one one two one"
    mydfm <- dfm(mytxt)
    expect_equal(
        round(textstat_lexdiv(mydfm, c("TTR", "C")), 3),
        data.frame(TTR = 0.286, C = 0.356, row.names = "text1")
    )
})
