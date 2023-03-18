test_that("customized tokenizer works correctly", {
    
    txt <- c("a b c 12345 ! @ # $ % ^ & * ( ) _ + { } | : \' \" < > ? ! , . \t \n \u2028 \u00A0 \u2003 \uFE0F",
             "#tag @user", "abc be-fg hi 100kg 2017", "https://github.com/kbenoit/quanteda", "a b c d e",
             "The URL was http://t.co/something.", "sci- fi every-4-year",
             "The URL was http://quanteda.io", "https://cran.r-project.org/incoming/",
             "https://github.com/quanteda/quanteda/issue/1 is another URL")
    
    quanteda:::tokenize_word(txt)
    quanteda:::tokenize_word("#aaa @bbb", character())
    quanteda_options("pattern_hashtag" = "#[\\w]+#?")
    quanteda_options("pattern_hashtag" = "#[\\p{L}]+#?")
    quanteda:::tokenize_word4("#aaa# @bbb", c("hashtag", "username"))
    quanteda:::tokenize_word4("#aaa# @bbb", c("username"))
    quanteda:::tokenize_word4("#aaa# @bbb", c("hashtag"))
    quanteda:::tokenize_word4(txt)
    
    # # prevents test failing on Ubuntu 20.04 on GitHub Actions
    # if (!(as.numeric(stringi::stri_info()$Unicode.version) > 10 &&
    #       as.numeric(stringi::stri_info()$ICU.version) > 61.1))
    #     txt <- c("support@quanteda.io K.Watanabe@qi1234.co.jp", txt)
    
    # With default parameters
    vanilla <- tokens(txt)
    customized <- tokens(txt,
                         customized_tokenizer("word"))
    attr(customized, "meta")$object$what <- "word"
    expect_equal(vanilla, customized)
    
    # With customized ICU -- skipping preserve/restore_special
    customized <- tokens(txt,
                         customized_tokenizer("ICU_word", split_hyphens = TRUE,
                                              custom_rules = hyphen_rule_lax))
    attr(customized, "meta")$object$what <- "word"
    expect_equal(vanilla, customized)
    
    # With splitting hyphens & tags
    vanilla <- tokens(txt, split_hyphens = TRUE, split_tags = TRUE)
    customized <- tokens(txt, split_hyphens = TRUE,
                         split_tags = TRUE,
                         customized_tokenizer("word"))
    attr(customized, "meta")$object$what <- "word"
    expect_equal(vanilla, customized)
    
    # Splitting hyphens -- skipping preserve/restore_special
    customized <- tokens(txt,
                         customized_tokenizer("ICU_word", split_hyphens = TRUE,
                                              split_tags = TRUE))
    attr(customized, "meta")$object$what <- "word"
    expect_equal(vanilla, customized)
    
    # DIFFERENCES
    expect_false(isTRUE(all.equal(
        tokens("www.r-project.org/about.html"),
        tokens("www.r-project.org/about.html", customized_tokenizer("ICU_word")),
        check.attributes = FALSE
    )))
    expect_false(isTRUE(all.equal(
        tokens("sci- fi"),
        tokens("sci- fi", customized_tokenizer("ICU_word")),
        check.attributes = FALSE
    )))
    
})

test_that("customized sentence tokenizer works correctly", {
    
    txt <- "This is a sentence about Mr. Smith. This is another sentence."
    customized <- tokens(txt, customized_tokenizer("sentence"))
    expect_identical(
        as.list(customized),
        list(text1 = c("This is a sentence about Mr. ", "Smith. ", "This is another sentence."))
    )
    
})

