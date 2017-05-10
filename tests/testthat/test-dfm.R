context('Testing dfm*.R')

mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900)
mydict <- dictionary(list(christmas=c("Christmas", "Santa", "holiday"),
                          opposition=c("Opposition", "reject", "notincorpus"),
                          taxing="taxing",
                          taxation="taxation",
                          taxregex="tax*",
                          country="united_states"))
dictDfm <- dfm(mycorpus, dictionary = mydict, valuetype = "glob")
dictDfm[1:10, ]
thesDfm <- dfm(mycorpus, thesaurus = mydict, valuetype = "glob")
thesDfm[1:10, (nfeature(thesDfm)-8) : nfeature(thesDfm)]

preDictDfm <- dfm(mycorpus, remove_punct = TRUE, remove_numbers = TRUE)
dfm_lookup(preDictDfm, mydict)

txt <- tokenize(char_tolower(c("My Christmas was ruined by your opposition tax plan.", 
                               "The United_States has progressive taxation.")),
                remove_punct = TRUE)


dfm(txt, dictionary = mydict, verbose = TRUE)
dfm(txt, thesaurus = mydict, verbose = TRUE)
dfm(txt, thesaurus = mydict, verbose = TRUE)

(txtDfm <- dfm(txt, verbose = FALSE))
dfm_lookup(txtDfm, mydict, valuetype = "glob") 
dfm_lookup(txtDfm, mydict, exclusive = FALSE, valuetype = "glob", verbose = FALSE) 


inaugTextsTokenized <- tokens(data_corpus_inaugural, remove_punct = TRUE)
inaugTextsTokenized <- tokens_tolower(inaugTextsTokenized)

# microbenchmark::microbenchmark(
#     dfm(inaugTextsTokenized, verbose = FALSE),
#     dfm(inaugTextsTokenized, dictionary = mydict, verbose = FALSE),
# )

## need to be carefully inspected!
txt <- "The tall brown trees with pretty leaves in its branches."
dfm(txt)
dfm(txt, stem = TRUE)
dfm(txt, remove = stopwords("english"))
dfm(txt, stem = TRUE, remove = stopwords("english"))


myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                          opposition = c("Opposition", "reject", "notincorpus"),
                          taxglob = "tax*",
                          taxregex = "tax.+$",
                          country = c("United_States", "Sweden")))
myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
               "Does the United_States or Sweden have more progressive taxation?"),
             remove = stopwords("english"), remove_punct = TRUE, tolower = FALSE,
             verbose = FALSE)
myDfm
# glob format
(tmp <- dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = TRUE))
expect_equal(as.vector(tmp[, c("christmas", "country")]), c(1, 0, 0, 2))
(tmp <- dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE))
expect_equal(as.vector(tmp[, c("christmas", "country")]), c(0, 0, 0, 0))
# regex v. glob format
(tmp <- dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = TRUE))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex")]), c(1, 1, 0, 0))
(tmp <- dfm_lookup(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex")]), c(1, 2, 0, 1))
## note: "united_states" is a regex match for "tax*"!!

(tmp <- dfm_lookup(myDfm, myDict, valuetype = "fixed"))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex", "country")]), c(0, 0, 0, 0, 0, 2))
(tmp <- dfm_lookup(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex", "country")]), c(0, 0, 0, 0, 0, 0))


test_that("dfm_trim", {

    mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900 & Year < 2017)
    preDictDfm <- dfm(mycorpus, remove_punct = TRUE, remove_numbers = TRUE)
    
    nfeature(dfm_trim(preDictDfm, min_count = 7))
    nfeature(dfm_trim(preDictDfm, min_count = 0.001))

    expect_equal(nfeature(dfm_trim(preDictDfm, min_count = 0.001)), 1045)
    expect_equal(nfeature(dfm_trim(preDictDfm, min_count = 7)), 1045)

    expect_equal(nfeature(dfm_trim(preDictDfm, min_docfreq = 0.05)), 3077)
    expect_equal(nfeature(dfm_trim(preDictDfm, min_docfreq = 2)), 3077)
    
    expect_equal(nfeature(dfm_trim(preDictDfm, sparsity = 0.95)), 3077)
    expect_equal(nfeature(dfm_trim(preDictDfm, sparsity = 0.95)), nfeature(dfm_trim(preDictDfm, min_docfreq = 0.05)))
    expect_equal(nfeature(dfm_trim(preDictDfm, min_docfreq = 0.05)), 3077)
    
})

test_that("dfm_trim works as expected", {
    mydfm <- dfm(c("This is a sentence.", "This is a second sentence.", "Third sentence.", "Fouth sentence.", "Fifth sentence."))
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "Removing features occurring:")
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "fewer than 2 times: 4")
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "in fewer than 2 documents: 4")
    expect_message(dfm_trim(mydfm, min_count =2, min_docfreq=2, verbose=T),
                   regexp = "  Total features removed: 4 \\(44.4%\\).")
})

test_that("dfm_trim works as expected", {
    mydfm <- dfm(c("This is a sentence.", "This is a second sentence.", "Third sentence.", "Fouth sentence.", "Fifth sentence."))
    expect_message(dfm_trim(mydfm, max_count =2, max_docfreq=2, verbose=T),
                   regexp = "more than 2 times: 2")
    expect_message(dfm_trim(mydfm, max_count =2, max_docfreq=2, verbose=T),
                   regexp = "in more than 2 documents: 2")
    
    expect_message(dfm_trim(mydfm, max_count =5, max_docfreq=5, verbose=T),
                   regexp = "No features removed.")
})

test_that("dfm_trim works without trimming arguments #509", {
    mydfm <- dfm(c("This is a sentence.", "This is a second sentence.", "Third sentence."))
    expect_equal(dim(mydfm[-2, ]), c(2, 7))
    expect_equal(dim(dfm_trim(mydfm[-2, ], verbose = FALSE)), c(2, 6))
})

test_that("test c.corpus",
    expect_that(
        matrix(dfm(corpus(c('What does the fox say?', 'What does the fox say?', '')), remove_punct = TRUE)),
        equals(matrix(rep(c(1, 1, 0), 5), nrow=15, ncol=1))
    )
)

## rbind.dfm
## TODO: Test classes returned

test_that("test rbind.dfm with the same columns", {

    fox <-'What does the fox say?'
    foxdfm <- rep(1, 20)
    dim(foxdfm) <- c(4,5)
    colnames(foxdfm) <- c('does', 'fox', 'say', 'the', 'what')
    rownames(foxdfm) <-  rep(c('text1', 'text2'), 2)

    dfm1 <- dfm(c(fox, fox), remove_punct = TRUE)

    expect_true(
        all(rbind(dfm1, dfm1) == foxdfm)
    )
    expect_that(
        rbind(dfm1, dfm1),
        is_a('dfmSparse')
    )

})

# TODO: Add function for testing the equality of dfms

test_that("test rbind.dfm with different columns", {
    dfm1 <- dfm('What does the fox?', remove_punct = TRUE)
    dfm2 <- dfm('fox say', remove_punct = TRUE)

    foxdfm <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0)
    dim(foxdfm) <- c(2,5)
    colnames(foxdfm) <- c('does', 'fox', 'say', 'the', 'what')
    rownames(foxdfm) <-  c('text1', 'text2')
    foxdfm <- as.matrix(foxdfm)

    testdfm <- rbind(dfm1, dfm2)

    expect_true(
        ##  Order of the result is not guaranteed
        all(testdfm[,order(colnames(testdfm))] == foxdfm[,order(colnames(foxdfm))])
    )

    expect_that(
        rbind(dfm1, dfm2),
        is_a('dfmSparse')
    )

})

test_that("test rbind.dfm with different columns, three args and repeated words", {

    dfm1 <- dfm('What does the?', remove_punct = TRUE)
    dfm2 <- dfm('fox say fox', remove_punct = TRUE)
    dfm3 <- dfm('The quick brown fox', remove_punct = TRUE)

    foxdfm <- c(0, 0, 1, 1, 0, 0, 0, 2, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0)
    dim(foxdfm) <- c(3,7)
    colnames(foxdfm) <- c('brown', 'does', 'fox', 'quick', 'say', 'the', 'what')
    rownames(foxdfm) <-  c('text1', 'text1', 'text1')
    foxdfm <- as.matrix(foxdfm)

    testdfm <- rbind(dfm1, dfm2, dfm3)
    expect_true(
        all(testdfm[,order(colnames(testdfm))] == foxdfm[,order(colnames(foxdfm))])
    )

    expect_that(
        rbind(dfm1, dfm2, dfm3),
        is_a('dfmSparse')
    )

})

test_that("test rbind.dfm with a single argument returns the same dfm", {
    fox <-'What does the fox say?'
    expect_true(
        all(
            rbind(dfm(fox)) == dfm(fox)
        )
    )
    expect_that(
        rbind(dfm(fox, remove_punct = TRUE)),
        is_a('dfmSparse')
    )
})

test_that("test that rbind.dfm with a single argument prints a warning", {
    fox <-'What does the fox say?'
    expect_that(
        rbind(dfm(fox, remove_punct = TRUE)),
        gives_warning('rbind.dfm called on single dfm')
        )

})



test_that("test rbind.dfm with the same features, but in a different order", {

    fox <-'What does the fox say?'
    xof <-'say fox the does What??'
    foxdfm <- rep(1, 20)
    dim(foxdfm) <- c(4,5)
    colnames(foxdfm) <- c('does', 'fox', 'say', 'the', 'what')
    rownames(foxdfm) <-  rep(c('text1', 'text2'), 2)

    dfm1 <- dfm(c(fox, xof), remove_punct = TRUE)

    expect_true(
        all(rbind(dfm1, dfm1) == foxdfm)
    )


})


test_that("dfm_weight works", {
    str <- c("apple is better than banana", "banana banana apple much better")
    w <- c(apple = 5, banana = 3, much = 0.5)
    mydfm <- dfm(str, remove = stopwords("english"))
    expect_equivalent(as.matrix(dfm_weight(mydfm, weights = w)),
                      matrix(c(5, 5, 1, 1, 3, 6, 0, 0.5), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "frequency")), 2),
                      matrix(c(1, 1, 1, 1, 1, 2, 0, 1), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "relFreq")), 2),
                      matrix(c(0.33, 0.2, 0.33, 0.2, 0.33, 0.4, 0, 0.2), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "relMaxFreq")), 2),
                      matrix(c(1, 0.5, 1, 0.5, 1, 1, 0, 0.5), nrow = 2))
    
    expect_equivalent(round(as.matrix(dfm_weight(mydfm, type = "logFreq")), 2),
                      matrix(c(1, 1, 1, 1, 1, 1.30, 0, 1), nrow = 2))
    
    # replication of worked example from
    # https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
    str <- c("this is a  a sample", "this is another example another example example")
    wikiDfm <- dfm(str)
    expect_equivalent(round(as.matrix(tfidf(wikiDfm, normalize = TRUE)), 2),
                      matrix(c(0, 0, 0, 0, 0.12, 0, 0.06, 0, 0, 0.09, 0, 0.13), nrow = 2))
})

test_that("dfm keeps all types with > 10,000 documents (#438) (a)", {
    generate_testdfm <- function(n) {
        dfm(paste('X', 1:n, sep=''))
    }
    expect_equal(nfeature(generate_testdfm(10000)), 10000)
    expect_equal(nfeature(generate_testdfm(20000)), 20000)
})

test_that("dfm keeps all types with > 10,000 documents (#438) (b)", {
    set.seed(10)
    generate_testdfm <- function(n) {
        dfm(paste(sample(letters, n, replace = TRUE), 1:n))
    }
    expect_equal(nfeature(generate_testdfm(10000)), 10026)
    expect_equal(nfeature(generate_testdfm(10001)), 10027)
})

test_that("dfm print works as expected", {
    testdfm <- dfm(data_corpus_irishbudget2010)
    expect_output(print(testdfm),
                  "^Document-feature matrix of: 14 documents, 5,058 features \\(80.9% sparse\\)")
    expect_output(print(testdfm[1:5, 1:5]),
                  "^Document-feature matrix of: 5 documents, 5 features \\(28% sparse\\).*")
    expect_output(head(testdfm, 1),
                  "Document-feature matrix of: 14 documents, 5,058 features.*showing first document and first 6 features.*")
    expect_output(tail(testdfm, 1),
                  "Document-feature matrix of: 14 documents, 5,058 features.*showing last document and last 6 features.*")
})
# 
# test_that("dfm.dfm works as expected", {
#     testdfm <- dfm(data_corpus_irishbudget2010, tolower = TRUE)
#     expect_identical(testdfm, dfm(testdfm, tolower = FALSE))
#     expect_identical(testdfm, dfm(testdfm, tolower = TRUE))
#     groupeddfm <- dfm(testdfm, 
#                       groups =  ifelse(docvars(data_corpus_irishbudget2010, "party") %in% c("FF", "Green"), "Govt", "Opposition"),
#                       tolower = FALSE)
#     expect_identical(colSums(groupeddfm), colSums(groupeddfm))
#     expect_identical(docnames(groupeddfm), c("Govt", "Opposition"))
#     expect_identical(testdfm, dfm(testdfm))
# 
#     dict <- dictionary(articles = c("the", "a", "an"),
#                        preps = c("of", "for", "in"))
#     expect_identical(
#         dfm(data_corpus_irishbudget2010, dictionary = dict),
#             dfm(testdfm, dictionary = dict)
#     )
#     expect_identical(
#         dfm(data_corpus_irishbudget2010, stem = TRUE),
#         dfm(testdfm, stem = TRUE)
#     )
# })

test_that("dfm-methods works as expected", {
    mydfm <- dfm(c("This is a test", "This is also a test", "This is an odd test"))
    expect_equivalent(as.matrix(topfeatures(mydfm)),
                      matrix(c(3,3,3,2,1,1,1)))
    
})

test_that("dfm_sample works as expected",{
    myDfm <- dfm(data_corpus_inaugural[1:10], verbose = FALSE)
    expect_error(dfm_sample(myDfm, what="documents", size = 20),
                  "size cannot exceed the number of documents \\(10\\)")
    expect_error(dfm_sample(myDfm, what="features", size = 3500),
                 "size cannot exceed the number of features \\(3358\\)")
    expect_error(dfm_sample(data_corpus_inaugural[1:10]),
                 "x must be a dfm object")
})


test_that("cbind.dfm works as expected",{
    dfm1 <- dfm("This is one sample text sample")
    dfm2 <- dfm("More words here")
    dfm12 <- cbind(dfm1, dfm2)

    expect_equal(nfeature(dfm12), 8)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("rbind.dfm works as expected",{
    dfm1 <- dfm("This is one sample text sample")
    dfm2 <- dfm("More words here")
    dfm12 <- rbind(dfm1, dfm2)
    
    expect_equal(nfeature(dfm12), 8)
    expect_equal(ndoc(dfm12), 2)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("dfm(x, dictionary = mwvdict) works with multi-word values", {
    mwvdict <- dictionary(list(sequence1 = "a b", sequence2 = "x y", notseq = c("d", "e")))
    txt <- c(d1 = "a b c d e f g x y z",
             d2 = "a c d x z",
             d3 = "x y",
             d4 = "f g")

    # as dictionary
    dfm1 <- dfm(txt, dictionary = mwvdict, verbose = TRUE)
    expect_identical(
        as.matrix(dfm1), 
        matrix(c(1, 0, 0, 0, 1, 0, 1, 0, 2, 1, 0, 0),
               nrow = 4,
               dimnames = list(docs = paste0("d", 1:4), 
                               features = c("sequence1", "sequence2", "notseq")))
    )
    
    # as thesaurus
    dfm2 <- dfm(txt, thesaurus = mwvdict, verbose = TRUE)
    expect_identical(
        as.matrix(dfm2), 
        matrix(c(0, 1, 0, 0,  1, 1, 0, 0,  1, 0, 0, 1,  1, 0, 0, 1,  0, 1, 0, 0, 1, 1, 0, 0, 
                 1, 0, 0, 0, 1, 0, 1, 0, 2, 1, 0, 0),
               nrow = 4,
               dimnames = list(docs = paste0("d", 1:4), 
                               features = c("a", "c", "f", "g", "x", "z", 
                                            "SEQUENCE1", "SEQUENCE2", "NOTSEQ")))
    )
})


test_that("dfm works with relational operators", {
    testdfm <- dfm(c("This is an example.", "This is a second example."))
    expect_is(testdfm == 0, "lgCMatrix")
    expect_is(testdfm >= 0, "lgCMatrix")
    expect_is(testdfm <= 0, "lgCMatrix")
    expect_is(testdfm < 0, "lgCMatrix")
    expect_is(testdfm < 1, "lgCMatrix")
    expect_is(testdfm > 0, "lgCMatrix")
    expect_is(testdfm > 1, "lgCMatrix")
    expect_is(testdfm > -1, "lgCMatrix")
    expect_is(testdfm < -1, "lgCMatrix")
})


test_that("dfm's document counts in verbose message is correct", {
    txt <- c(d1 = "a b c d e f g x y z",
             d2 = "a c d x z",
             d3 = "x y",
             d4 = "f g")
    expect_message(dfm(txt, remove = c('a', 'f'), verbose = TRUE),
                   'removed 2 features and 0 documents')
    expect_message(dfm(txt, select = c('a', 'f'), verbose = TRUE),
                   'kept 2 features and 4 documents')
})

test_that("dfm print works with options as expected", {
    tmp <- dfm(data_corpus_irishbudget2010)
    expect_output(
        head(tmp),
        "Document-feature matrix of: 14 documents, 5,058 features.*\\(showing first 6 documents and first 6 features\\)"
    )
    expect_output(
        head(tmp[1:5, 1:5]),
        "Document-feature matrix of: 5 documents, 5 features.*\\(showing first 5 documents and first 5 features\\)"
    )
    expect_output(
        print(tmp[1:5, 1:5]),
        "Document-feature matrix of: 5 documents, 5 features.*5 x 5 sparse Matrix"
    )
    expect_output(
        print(tmp[1:5, 1:5], show.values = FALSE),
        "^Document-feature matrix of: 5 documents, 5 features \\(28% sparse\\)\\.$"
    )
    expect_output(
        print(tmp[1:3, 1:3], ndoc = 2, nfeature = 2, show.values = TRUE),
        "^Document-feature matrix of: 3 documents, 3 features.*3 x 3 sparse Matrix"
    )
    expect_output(
        print(tmp[1:5, 1:5], show.summary = FALSE),
        "^5 x 5 sparse Matrix"
    )
})

