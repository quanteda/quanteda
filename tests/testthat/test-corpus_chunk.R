test_that("corpus_chunk works", {

  corp <- corpus(c("One two three four! Two more.",
                   "Four, five six seven eight.",
                   ""),
                 docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))

  corp_chunk1 <- corpus_chunk(corp, size = 3)
  expect_identical(
    as.character(corp_chunk1),
    c(text1.1 = "One two three", text1.2 = "four! Two", text1.3 = "more.",
      text2.1 = "Four, five six", text2.2 = "seven eight.", text3.1 = ""
    )
  )

  corp_chunk2 <- corpus_chunk(corp, size = 5)
  expect_identical(
    as.character(corp_chunk2),
    c(text1.1 = "One two three four! Two", text1.2 = "more.",
      text2.1 = "Four, five six seven eight.",
      text3.1 = "")
  )

  expect_error(
    corpus_chunk(corp, size = c(3, 1)),
    "The length of size must be 1"
  )

  expect_error(
    corpus_chunk(corp, size = -1),
    "The value of size must be between 0 and Inf"
  )

  expect_error(
      corpus_chunk(corp, size = 2, truncate = "yes"),
      "The value of truncate cannot be NA"
  )

  expect_message(
      corpus_chunk(corp, size = 2, verbose = TRUE),
      "corpus_chunk() changed", fixed = TRUE
  )

})

test_that("corpus_chunk preserves docvars", {
  corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
                 docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))

  corp_chunk1 <- corpus_chunk(corp, size = 3)
  expect_identical(
    docvars(corp_chunk1),
    structure(list(dv1 = c("value 1", "value 1", "value 1", "value 1",
                           "value 1"),
                   dv2 = c("value 2", "value 2", "value 2", "value 2",
                           "value 2")), row.names = c(NA, -5L),
              class = "data.frame")
  )

  corp_chunk2 <- corpus_chunk(corp, size = 5)
  expect_identical(
    docvars(corp_chunk2),
    structure(list(dv1 = c("value 1", "value 1", "value 1"),
                   dv2 = c("value 2", "value 2", "value 2")),
              row.names = c(NA, -3L),
              class = "data.frame")
  )

  corp_chunk3 <- corpus_chunk(corp, size = 5, use_docvars = FALSE)
  expect_identical(
    docvars(corp_chunk3),
    structure(list(), names = character(0), row.names = c(NA, -3L),
              class = "data.frame")
  )
})

test_that("corpus_chunk truncate works", {
  corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
                 docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))
  corp_chunk <- corpus_chunk(corp, size = 2, truncate = TRUE)

  expect_equal(
    as.character(corp_chunk),
    c(text1 = "One two",
      text2 = "Four, five"
    )
  )
})

test_that("corpus_group works with chuncked corpus", {

    corp <- corpus(c("One two three four! Two more.",
                     "Four, five six seven eight."),
                   docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))
    corp_chunk <- corpus_chunk(corp, size = 5)

    expect_equal(
        corpus_group(corp_chunk),
        corp
    )
})

# test_that("corpus_chunk overlap works", {
#   corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
#                  docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))
#   corp_chunk <- corpus_chunk(corp, size = 7, inflation_factor = 1, overlap = 3)
#
#   expect_equal(
#     as.character(corp_chunk),
#     c(text1.1 = "One two three four! Two more", text1.2 = "! Two more.",
#       text2.1 = "Four, five six seven eight.", text2.2 = "seven eight."
#     )
#   )
# })

test_that("corpus_chunk() works with non-English langauge", {

    skip("stri_wrap() does not work with Japanese")

    corp <- corpus("日本語もトークンで分割できる。")
    expect_identical(
        as.list(corpus_chunk(corp, size = 4)),
        list(text1.1 = c("日本語", "も","トーク", "ン"),
             text1.2 = c("で", "分割", "できる", "。"))
    )
})

test_that("the number of characters and tokens are roughly the same", {

    skip_on_cran()

    # number of characters
    corp <- data_corpus_inaugural
    corp_chunk <- corpus_chunk(corp, 512)
    expect_equal(
        sum(stringi::stri_length(corp_chunk)) / sum(stringi::stri_length(corp)),
        0.99,
        tolerance = 0.02
    )

    # number of tokens
    toks <- tokens(corp)
    toks_chunk <- tokens(corp_chunk)
    expect_equal(
        sum(ntoken(toks)) / sum(ntoken(toks_chunk)),
        1,0
    )

    expect_equal(
        mean(ntoken(toks_chunk)) / 512,
        0.97,
        tolerance = 0.01
    )
})
