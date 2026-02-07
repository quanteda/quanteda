test_that("summary and print.summary work", {
    
    # a second time for the cache
    summ <- summary(data_corpus_inaugural[1:2])
    expect_is(summ, "data.frame")
    expect_equal(
        names(summ),
        c("Text", "Types", "Tokens", "Sentences", "Year", "President", "FirstName", "Party")
    )
    expect_output(
        print(summ),
        paste(c(
        "Corpus consisting of 2 documents, showing 2 documents:",
        "",
        "            Text Types Tokens Sentences Year  President FirstName Party",
        " 1789-Washington   625   1537        24 1789 Washington    George  none",
        " 1793-Washington    96    147         5 1793 Washington    George  none"
    ), collapse = "\n"), fixed = TRUE)
    
})

test_that("summary is consistent with other functions", {
    
    # a second time for the cache
    summ <- summary(data_corpus_inaugural)
    toks <- tokens(data_corpus_inaugural)
    corp <- corpus_reshape(data_corpus_inaugural)
    
    expect_equivalent(
        summ$Types,
        as.integer(ntype(toks))
    )
    
    expect_equivalent(
        summ$Tokens,
        as.integer(ntoken(toks))
    )
    
    expect_equivalent(
        summ$Sentences,
        as.integer(table(docid(corp)))
    )
})

