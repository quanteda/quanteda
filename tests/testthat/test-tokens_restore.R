require(stringi)

test_that("tokens_restore works with ireqgular markers", {
    
    # remove stray marks
    toks1 <- as.tokens(stri_split_boundaries("\uE002aaa\uE001bbb\uE001", type = "word")) |> 
        tokens_restore()
    expect_equal(as.list(toks1), 
                 list("text1" = c("aaa", "bbb")))
    
    # only extract innermost pairs
    toks2 <- as.tokens(stri_split_boundaries("\uE001\uE001aaa bbb\uE002ccc\uE002", type = "word")) |> 
        tokens_restore()
    expect_equal(as.list(toks2), 
                 list("text1" = c("aaa bbb", "ccc")))
    
    # empty tokens
    expect_equal(ndoc(quanteda:::tokens_restore(as.tokens(list()))), 0)
})

test_that("tokens_restore restore tags", {
    
    txt <- "オリンピック延期決定！ #politics @abe #政治# #政治 #安倍政権 @安倍政権 ！"
    toks1 <- tokens(txt, remove_separators = FALSE)
    
    txt2 <- stri_replace_all_regex(txt, "@[a-zA-Z0-9_]+|#[\\p{L}\\p{N}]+#?", "\uE001$0\uE002")
    toks2 <- as.tokens(stri_split_boundaries(txt2, type = "word")) |> 
        tokens_restore() 
    
    expect_equal(as.list(toks1), as.list(toks2))
})
