test_that("corpus_chunk works with inflation_factor", {
  corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
                 docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))

  corpchnk1 <- corpus_chunk(corp, size = 3, inflation_factor = 0.8)
  expect_identical(
    as.character(corpchnk1),
    c(text1.1 = "One two three", text1.2 = "four! Two more.",
      text2.1 = "Four, five six seven", text2.2 = "eight.")
  )

  corpchnk2 <- corpus_chunk(corp, size = 5, inflation_factor = 1.0)
  expect_identical(
    as.character(corpchnk2),
    c(text1.1 = "One two three four! Two", text1.2 = "more.",
      text2.1 = "Four, five six seven eight.")
  )
})

test_that("corpus_chunk preserves docvars", {
  corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
                 docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))

  corpchnk1 <- corpus_chunk(corp, size = 3, inflation_factor = 0.8)
  expect_identical(
    docvars(corpchnk1),
    structure(list(dv1 = c("value 1", "value 1", "value 1", "value 1"),
                   dv2 = c("value 2", "value 2", "value 2", "value 2")),
              row.names = c(NA, -4L), class = "data.frame")
  )

  corpchnk2 <- corpus_chunk(corp, size = 5, inflation_factor = 1.0)
  expect_identical(
    docvars(corpchnk2),
    structure(list(dv1 = c("value 1", "value 1", "value 1"),
                   dv2 = c("value 2", "value 2", "value 2")),
              row.names = c(NA, -3L), class = "data.frame")
  )

  corpchnk3 <- corpus_chunk(corp, size = 5, use_docvars = FALSE)
  expect_identical(
    docvars(corpchnk3),
    structure(list(), names = character(0), row.names = c(NA, -3L),
              class = "data.frame")
  )
})

test_that("corpus_chunk truncate works", {
  corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
                 docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))
  corpchnk <- corpus_chunk(corp, size = 2, truncate = TRUE)

  expect_equal(
    as.character(corpchnk),
    c(text1 = "One two",
      text2 = "Four, five"
    )
  )
})

# test_that("corpus_chunk overlap works", {
#   corp <- corpus(c("One two three four! Two more.", "Four, five six seven eight."),
#                  docvars = data.frame(dv1 = "value 1", dv2 = "value 2"))
#   corpchnk <- corpus_chunk(corp, size = 7, inflation_factor = 1, overlap = 3)
#
#   expect_equal(
#     as.character(corpchnk),
#     c(text1.1 = "One two three four! Two more", text1.2 = "! Two more.",
#       text2.1 = "Four, five six seven eight.", text2.2 = "seven eight."
#     )
#   )
# })


