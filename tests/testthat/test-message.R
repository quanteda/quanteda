test_that("msg works", {
    
    # no indices
    expect_equal(
        quanteda:::msg("there are %d %s", list(3, "documents")),
        "there are 3 documents"
    )
    
    # NULL indices
    expect_equal(
        quanteda:::msg("there are %d %s", list(3, "documents"),
                       indices = list(NULL, NULL)),
        "there are 3 documents"
    )
    
    # ignore indices
    expect_equal(
        quanteda:::msg("there is %s", "a document", indices = FALSE),
        "there is a document"
    )
    expect_equal(
        suppressWarnings(quanteda:::msg("there is %s", "a document", indices = c(1, 2))),
        "there is a document"
    )
    
    # logical indices
    expect_equal(
        quanteda:::msg("there is %s", c("a document", "documents"), indices = FALSE),
        "there is a document"
    )
    expect_equal(
        quanteda:::msg("there are %s", c("a document", "documents"), indices = TRUE),
        "there are documents"
    )
    
    # numeric indices
    expect_equal(
        quanteda:::msg("there is %s", c("a document", "documents"), indices = 1),
        "there is a document"
    )
    expect_equal(
        quanteda:::msg("there are %s", c("a document", "documents"), indices = 2),
        "there are documents"
    )
    
    # multiple numeric indices
    expect_equal(
        quanteda:::msg("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(2, 1)),
        "there is a document"
    )
    expect_equal(
        quanteda:::msg("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(3, 2)),
        "there are documents"
    )
    expect_equal(
        quanteda:::msg("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = c(3, 2)),
        "there are documents"
    )
    
    # mixed indices types
    expect_equal(
        quanteda:::msg("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(2, FALSE)),
        "there is a document"
    )
    expect_equal(
        quanteda:::msg("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(3, TRUE)),
        "there are documents"
    )
    expect_equal(
        quanteda:::msg("there are 10000 features"),
        "there are 10,000 features"
    )
    expect_equal(
        quanteda:::msg("there are 10000 features", pretty = FALSE),
        "there are 10000 features"
    )
})
