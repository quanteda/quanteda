test_that("TBB is enabled", {
    
  info <- info_tbb()
  if (info$enabled) {
    expect_true(info$enabled)
    expect_true(info$max_threads > 1)
  } else {
    expect_false(info$enabled)
    expect_false(info$max_threads > 1)
  }
  
})
