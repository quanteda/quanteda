test_that("TBB is enabled", {

  skip_on_cran()
    
  info <- info_tbb()
  expect_true(info$enabled)
  expect_true(info$max_threads > 1)
  
})
