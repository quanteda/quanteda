test_that("drop0.dfm zeros out small values", {
  toks <- tokens(data_corpus_inaugural[1:3])
  dfmt <- dfm(toks)
  
  expect_true(any(dfmt < 2))
  
  dfmt_drop <- drop0(dfmt, tol = 2)
  
  m <- as.matrix(dfmt_drop)
  expect_true(all(m == 0 | m >= 2))
})

test_that("drop0.dfm preserves docvars", {
  toks <- tokens(data_corpus_inaugural[1:3])
  dfmt <- dfm(toks)
  
  dv_before <- docvars(dfmt)
  dfmt_drop <- drop0(dfmt, tol = 2)
  
  expect_identical(docvars(dfmt_drop), dv_before)
})

test_that("drop0.dfm preserves dim and names", {
  toks <- tokens(data_corpus_inaugural[1:3])
  dfmt <- dfm(toks)
  
  dim_before <- dim(dfmt)
  fn_before  <- featnames(dfmt)
  dn_before  <- docnames(dfmt)
  
  dfmt_drop <- drop0(dfmt, tol = 2)
  
  expect_identical(dim(dfmt_drop), dim_before)
  expect_identical(featnames(dfmt_drop), fn_before)
  expect_identical(docnames(dfmt_drop), dn_before)
})

test_that("drop0.dfm with tol = 0 leaves dfm unchanged", {
  toks <- tokens(data_corpus_inaugural[1:3])
  dfmt <- dfm(toks)
  
  dfmt_drop <- drop0(dfmt, tol = 0)
  
  expect_equal(as.matrix(dfmt_drop), as.matrix(dfmt))
  expect_identical(docvars(dfmt_drop), docvars(dfmt))
})

