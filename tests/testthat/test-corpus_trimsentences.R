context("test trimsentences functions")

test_that("corpus_trimsentences works", {
    txt <- c("PAGE 1. This is a single sentence.  Short sentence. Three word sentence.",
             "PAGE 2. Very short! Shorter.",
             "Very long sentence, with multiple parts, separated by commas.  PAGE 3.")
    mycorp <- corpus(txt, docvars = data.frame(serial = 1:3))
    expect_equal(
        texts(corpus_trimsentences(mycorp, min_length = 3)),
        c(text1 = "This is a single sentence.  Three word sentence.",
          text3 = "Very long sentence, with multiple parts, separated by commas.")
    )
    expect_equal(
        texts(corpus_trimsentences(mycorp, exclude_pattern = "^PAGE \\d+")),
        c(text1 = "This is a single sentence.  Short sentence.  Three word sentence.",
          text2 = "Very short!  Shorter.",
          text3 = "Very long sentence, with multiple parts, separated by commas.")
    )

})

test_that("char_trimsentences works", {
    txt <- c(d1 = "PAGE 1. This is a single sentence.  Short sentence. Three word sentence.",
             d2 = "PAGE 2. Very short! Shorter.",
             d3 = "Very long sentence, with multiple parts, separated by commas.  PAGE 3.")
    expect_equal(
        char_trimsentences(txt, min_length = 3),
        c(d1 = "This is a single sentence.  Three word sentence.",
          d3 = "Very long sentence, with multiple parts, separated by commas.")
    )
    expect_equal(
        char_trimsentences(txt, exclude_pattern = "^PAGE \\d+"),
        c(d1 = "This is a single sentence.  Short sentence.  Three word sentence.",
          d2 = "Very short!  Shorter.",
          d3 = "Very long sentence, with multiple parts, separated by commas.")
    )
})

