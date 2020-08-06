context('test extractor')

txt <- c(doc1 = "This is a sample text.\nIt has three lines.\nThe third line.",
         doc2 = "one\ntwo\tpart two\nthree\nfour.",
         doc3 = "A single sentence.",
         doc4 = "A sentence with \"escaped quotes\".")
dat <- data.frame(var_numeric = 10:13, var_factor = factor(c("A", "B", "A", "B")), 
                  var_char = letters[1:4])
corp <- corpus(txt, docvars = dat) 
toks <- tokens(corp)
dfmt <- dfm(toks)

test_that("extractor for corpus works", {
    
    expect_equal(docnames(corp[c(1, 3)]), c("doc1", "doc3"))
    expect_equal(docid(corp[c(1, 3)]), droplevels(docid(corp)[c(1, 3)]))
    expect_equal(docnames(corp[c(1, 1)]), c("doc1.1", "doc1.2"))
    expect_equal(docid(corp[c(1, 1)]), droplevels(docid(corp)[c(1, 1)]))
    
})

test_that("extractor for tokens works", {
    
    expect_equal(docnames(toks[c(1, 3)]), c("doc1", "doc3"))
    expect_equal(docid(toks[c(1, 3)]), droplevels(docid(toks)[c(1, 3)]))
    expect_equal(docnames(toks[c(1, 1)]), c("doc1.1", "doc1.2"))
    expect_equal(docid(toks[c(1, 1)]), droplevels(docid(toks)[c(1, 1)]))
    
})

test_that("extractor for dfm works", {
    
    expect_equal(docnames(dfmt[c(1, 3),]), c("doc1", "doc3"))
    expect_equal(docid(dfmt[c(1, 3),]), droplevels(docid(dfmt)[c(1, 3)]))
    expect_equal(docnames(dfmt[c(1, 1),]), c("doc1.1", "doc1.2"))
    expect_equal(docid(dfmt[c(1, 1),]), droplevels(docid(dfmt)[c(1, 1)]))
    
})
