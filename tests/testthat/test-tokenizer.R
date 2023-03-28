require(stringi)
require(quanteda)

rules <- data_breakrules_word
#rules <- list(base = paste0(readLines("rules/word.txt"), collapse = "\n"))
#rules <- c(rules, yaml::read_yaml("rules/custom.yml"))

txt <- c("a b c 12345 ! @ # $ % ^ & * ( ) _ + { } | : \' \" < > ? ! , . \t \n \u2028 \u00A0 \u2003 \uFE0F",
         "abc be-fg hi 100kg 2017", "sci- fi every-4-year",
         "#twitter #weibo# @user", "koheiw@quanteda.org",
         "https://github.com/kbenoit/quanteda",
         "The URL was http://t.co/something.", 
         "The URL was http://quanteda.io", "https://cran.r-project.org/incoming/",
         "https://github.com/quanteda/quanteda/issue/1 is another URL",
         "i \u2764\ufe0f you \u2764\ufe0f\ufe0f\u2764",
         "übër u\u0308be\u0308r \u0308ubër")

test_that("the base rule produces the same results as type = 'word'", {
    
    lis_word <- stri_split_boundaries(txt, type = "word")
    lis_rule <- tokenize_custom(txt, rules["base"])
    
    expect_true(identical(lis_word[1:3], lis_rule[1:3]))
    expect_false(identical(lis_word[4:5], lis_rule[4:5])) # ICU rules changed
    expect_true(identical(lis_word[6:12], lis_rule[6:12]))

})


# test_that("user-defined tokenizer works", {
#     quanteda_options("tokens_tokenizer_word" = "my_tokenizer")
#     my_tokenizer <- function(x, ...) {
#         tokenize_custom(x, ".;")
#     }
#     expect_identical(tokens("abc defg")[[1]],
#                      c("a", "b", "c", "d", "e", "f", "g"))
# 
#     quanteda_options("tokens_tokenizer_word" = "some_tokenizer")
#     expect_error(tokens("abc defg"), 
#                  "Invalid value in tokens_tokenizer_word")
#     quanteda_options(reset = TRUE)
# })
