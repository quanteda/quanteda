context("testing tokens_hashed_recompile")

test_that("tokens_hashed_recompile: tokens_tolower", {
    toks1 <- tokens(c(one = "a b c d A B C D",
                     two = "A B C d"))
    attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
    expect_equal(
        attr(quanteda:::tokens_hashed_recompile(toks1), "types"),
        letters[1:4]
    )
    expect_equal(
        unique(unlist(unclass(quanteda:::tokens_hashed_recompile(toks1)))),
        1:4
    )
    expect_equal(
        quanteda:::tokens_hashed_recompile(toks1, method = "C++"),
        quanteda:::tokens_hashed_recompile(toks1, method = "R")
    )
})

