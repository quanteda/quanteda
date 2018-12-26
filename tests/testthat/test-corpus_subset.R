context('test corpus_subset')

test_that("head, tail.corpus work as expected", {
    corp <- corpus_subset(data_corpus_inaugural, Year < 2018)
    expect_equal(
        docnames(head(corp, 3)),
        c("1789-Washington", "1793-Washington", "1797-Adams")
    )
    expect_equal(
        docnames(head(corp, -55)),
        c("1789-Washington", "1793-Washington", "1797-Adams")
    )
    expect_equal(
        docnames(tail(corp, 3)),
        c("2009-Obama", "2013-Obama", "2017-Trump")
    )
    expect_equal(
        docnames(tail(corp, -55)),
        c("2009-Obama", "2013-Obama", "2017-Trump")
    )
})


test_that("corpus_subset works", {
    txt <- c(doc1 = "This is a sample text.\nIt has three lines.\nThe third line.",
             doc2 = "one\ntwo\tpart two\nthree\nfour.",
             doc3 = "A single sentence.",
             doc4 = "A sentence with \"escaped quotes\".")
    df <- data.frame(varnumeric = 10:13, varfactor = factor(c("A", "B", "A", "B")), 
                     varchar = letters[1:4])
    
    data_corpus_test <- corpus(txt, docvars = df)
    expect_equal(ndoc(corpus_subset(data_corpus_test, varfactor == "B")), 2)
    expect_equal(docnames(corpus_subset(data_corpus_test, varfactor == "B")), 
                 c("doc2", "doc4"))
    
    data_corpus_test_nodv <- corpus(txt)
    expect_equal(ndoc(corpus_subset(data_corpus_test_nodv, LETTERS[1:4] == "B")), 1)
    expect_equal(docnames(corpus_subset(data_corpus_test_nodv, LETTERS[1:4] == "B")), 
                 c("doc2"))
    
})