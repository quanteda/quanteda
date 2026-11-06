toks <- tokens(data_corpus_inaugural[1:3])

test_that("drop0 works with dfm", {
  
  dfmt <- dfm(toks)
  
  expect_true(
      is.dfm(drop0(dfmt, 2))
  )
  
  expect_equal(
      drop0(dfmt, 2)@x, dfmt[dfmt > 2]
  )
  
  expect_equal(
      dimnames(drop0(dfmt, 2)), 
      dimnames(dfmt)
  )
  
  expect_equal(
      docvars(drop0(dfmt, 2)), 
      docvars(dfmt)
  )
  
  expect_equal(
      meta(drop0(dfmt, 2)), 
      meta(dfmt)
  )
})

test_that("drop0 works with fcm", {
    
    fcmt <- fcm(toks)
    
    expect_true(
        is.fcm(drop0(fcmt, 2))
    )
    
    expect_equal(
        drop0(fcmt, 2)@x, fcmt[fcmt > 2]
    )
    
    expect_equal(
        dimnames(drop0(fcmt, 2)), 
        dimnames(fcmt)
    )
    
    expect_equal(
        meta(drop0(fcmt, 2)), 
        meta(fcmt)
    )
})
