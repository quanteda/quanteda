test_that("summary and print.summary work", {
    summary(data_corpus_inaugural[1:2])
    # a second time for the cache
    summ <- summary(data_corpus_inaugural[1:2])
    expect_is(summ, "data.frame")
    expect_equal(
        names(summ),
        c("Text", "Types", "Tokens", "Sentences", "Year", "President", "FirstName", "Party")
    )
    expect_output(
        print(summ),
        "Corpus consisting of 2 documents, showing 2 documents:

            Text Types Tokens Sentences Year  President FirstName Party
 1789-Washington   625   1537        23 1789 Washington    George  none
 1793-Washington    96    147         4 1793 Washington    George  none"
    )
})
