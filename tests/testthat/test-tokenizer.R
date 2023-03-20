test_that("stri_split_boundaries produces the same results", {
    
    txt <- c("a b c 12345 ! @ # $ % ^ & * ( ) _ + { } | : \' \" < > ? ! , . \t \n \u2028 \u00A0 \u2003 \uFE0F",
             "#tag @user", "abc be-fg hi 100kg 2017", "https://github.com/kbenoit/quanteda", "a b c d e",
             "The URL was http://t.co/something.", "sci- fi every-4-year",
             "The URL was http://quanteda.io", "https://cran.r-project.org/incoming/",
             "https://github.com/quanteda/quanteda/issue/1 is another URL",
             "i \u2764\ufe0f you \u2764\ufe0f\ufe0f\u2764",
             "übër u\u0308be\u0308r \u0308ubër")
    
    lis_word <- stri_split_boundaries(txt, type = "word")
    lis_rule <- stri_split_boundaries(txt, type = data_breakrules_word$word)
    
    expect_true(identical(lis_word[1], lis_rule[1]))
    expect_false(identical(lis_word[2], lis_rule[2])) # rules have been updated
    expect_true(identical(lis_word[3:12], lis_rule[3:12]))

})
