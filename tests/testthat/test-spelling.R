test_that("spell check passes", {
  skip_on_cran()
  skip_if_not_installed("spelling")

  spelling::expect_spell_check(vignettes = TRUE)
})
