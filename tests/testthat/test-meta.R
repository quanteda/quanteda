context("test meta functions")

test_that("meta/meta<- works user data", {
    txt <- c(d1 = "a b c", d2 = "x y z")
    corp <- corpus(txt, docvars = data.frame(dv = 1:2))
    toks <- tokens(corp)
    dfmat <- dfm(toks)
    targetmeta3 <- list(usermeta1 = "test1", usermeta2 = "test2", usermeta3 = "test3")
    targetmeta2 <- list(usermeta1 = "test1", usermeta3 = "test3")
    
    meta(corp, "usermeta1") <- "test1"
    meta(corp)["usermeta2"] <- "test2"
    meta(corp)$usermeta3    <- "test3"
    expect_identical(meta(corp), targetmeta3)
    meta(corp)[2] <- NULL
    expect_identical(meta(corp), targetmeta2)

    meta(toks, "usermeta1") <- "test1"
    meta(toks)["usermeta2"] <- "test2"
    meta(toks)$usermeta3    <- "test3"
    expect_identical(meta(toks), targetmeta3)
    meta(toks)[2] <- NULL
    expect_identical(meta(toks), targetmeta2)
    
    meta(dfmat, "usermeta1") <- "test1"
    meta(dfmat)["usermeta2"] <- "test2"
    meta(dfmat)$usermeta3    <- "test3"
    expect_identical(meta(dfmat), targetmeta3)
    meta(dfmat)[2] <- NULL
    expect_identical(meta(dfmat), targetmeta2)
})
