test_that("index returns the correct matches in a data.frame", {
    toks <- tokens(c(d1 = paste(LETTERS, collapse = " "),
                     d2 = paste(letters, collapse = " ")))
    
    loc <- index(toks, c("A", "D", "z", "h", phrase("H I J")))
    expect_equivalent(
        loc,
        data.frame(docname = c("d1", "d1", "d1", "d1", "d1", "d2", "d2", "d2", "d2", "d2"), 
                   from = c(1L, 4L, 8L, 8L, 26L, 1L, 4L, 8L, 8L, 26L), 
                   to = c(1L, 4L, 8L, 10L, 26L, 1L, 4L, 8L, 10L, 26L), 
                   pattern = factor(c("A", "D", "h", "H I J", "z", "A", "D", "h", "H I J", "z")),
                   stringsAsFactors = FALSE)
    )
    
    loc <- index(toks, c("A", "D", "z", "h", phrase("H I J")), case_insensitive = FALSE)
    expect_equivalent(
        loc,
        data.frame(docname = c("d1", "d1", "d1", "d2", "d2"), 
                   from = c(1L, 4L, 8L, 8L, 26L), 
                   to = c(1L, 4L, 10L, 8L, 26L),
                   pattern = factor(c("A", "D", "H I J", "h", "z")),
                   stringsAsFactors = FALSE)
    )

    loc <- index(toks, "[b-c]", valuetype = "regex")
    expect_equivalent(
        loc,
        data.frame(docname = c("d1", "d1", "d2", "d2"), 
                   from = c(2L, 3L, 2L, 3L), 
                   to = c(2L, 3L, 2L, 3L),
                   pattern = factor(rep("[b-c]", 4)),
                   stringsAsFactors = FALSE)
    )
})
