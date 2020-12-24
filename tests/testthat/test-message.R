context("test messaging functions")

test_that("info works", {
    
    # no indices
    expect_message(
        quanteda:::info("there are %d %s", list(3, "documents")),
        "there are 3 documents"
    )
    
    # ignore indices
    expect_message(
        quanteda:::info("there is %s", "a document", indices = FALSE),
        "there is a document"
    )
    expect_message(
        quanteda:::info("there is %s", "a document", indices = c(1, 2)),
        "there is a document"
    )
    
    # logical indices
    expect_message(
        quanteda:::info("there is %s", c("a document", "documents"), indices = FALSE),
        "there is a document"
    )
    expect_message(
        quanteda:::info("there are %s", c("a document", "documents"), indices = TRUE),
        "there are documents"
    )
    
    # numeric indices
    expect_message(
        quanteda:::info("there is %s", c("a document", "documents"), indices = 1),
        "there is a document"
    )
    expect_message(
        quanteda:::info("there are %s", c("a document", "documents"), indices = 2),
        "there are documents"
    )
    
    # multiple numeric indices
    expect_message(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(2, 1)),
        "there is a document"
    )
    expect_message(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(3, 2)),
        "there are documents"
    )
    expect_message(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = c(3, 2)),
        "there are documents"
    )
    
    # mixed indices types
    expect_message(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(2, FALSE)),
        "there is a document"
    )
    expect_message(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(3, TRUE)),
        "there are documents"
    )
    
    # warning
    expect_warning(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = list(1, 1), warn = TRUE),
        "there isn't a document"
    )
    expect_warning(
        quanteda:::info("there %s %s", 
                        list(c("isn't", "is", "are"), c("a document", "documents")), 
                        indices = c(3, 2), warn = TRUE),
        "there are documents"
    )
    
})
