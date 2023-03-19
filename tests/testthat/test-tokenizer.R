test_that("tokenizer works correctly", {
    
    txt <- c("a b c 12345 ! @ # $ % ^ & * ( ) _ + { } | : \' \" < > ? ! , . \t \n \u2028 \u00A0 \u2003 \uFE0F",
             "#tag @user", "abc be-fg hi 100kg 2017", "https://github.com/kbenoit/quanteda", "a b c d e",
             "The URL was http://t.co/something.", "sci- fi every-4-year",
             "The URL was http://quanteda.io", "https://cran.r-project.org/incoming/",
             "https://github.com/quanteda/quanteda/issue/1 is another URL")
    
    # NOTE: still slightly different
    expect_identical(as.list(tokens(txt)), 
                     as.list(tokens(txt, what = "word4")))
    
    expect_identical(as.list(tokens(txt, split_tags = TRUE)), 
                     as.list(tokens(txt, what = "word4", split_tags = TRUE)))
    
})
