context('Testing dfm*.R')

mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900)
mydict <- dictionary(list(christmas=c("Christmas", "Santa", "holiday"),
                          opposition=c("Opposition", "reject", "notincorpus"),
                          taxing="taxing",
                          taxation="taxation",
                          taxregex="tax*",
                          country="united_states"))
dictDfm <- dfm(mycorpus, dictionary = mydict, valuetype = "glob")
dictDfm[1:10,]
thesDfm <- dfm(mycorpus, thesaurus = mydict, valuetype = "glob")
thesDfm[1:10, (nfeature(thesDfm)-8) : nfeature(thesDfm)]

preDictDfm <- dfm(mycorpus, removePunct = TRUE, removeNumbers = TRUE)
dfm_lookup(preDictDfm, mydict)

txt <- tokenize(toLower(c("My Christmas was ruined by your opposition tax plan.", 
                          "The United_States has progressive taxation.")),
                removePunct = TRUE)


dfm(txt, dictionary = mydict, verbose = TRUE)
dfm(txt, thesaurus = mydict, verbose = TRUE)
dfm(txt, dictionary = mydict, verbose = TRUE, codeType = "old")
dfm(txt, thesaurus = mydict, verbose = TRUE)

(txtDfm <- dfm(txt, verbose = FALSE))
dfm_lookup(txtDfm, mydict, valuetype = "glob") 
dfm_lookup(txtDfm, mydict, exclusive = FALSE, valuetype = "glob", verbose = FALSE) 


inaugTextsTokenized <- tokenize(toLower(inaugTexts[1:10]), removePunct = TRUE)
# microbenchmark::microbenchmark(
#     dfm(inaugTextsTokenized, verbose = FALSE),
#     dfm(inaugTextsTokenized, verbose = FALSE, codeType = "old"),
#     dfm(inaugTextsTokenized, dictionary = mydict, verbose = FALSE),
#     dfm(inaugTextsTokenized, dictionary = mydict, verbose = FALSE, codeType = "old")
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
             remove = stopwords("english"), removePunct = TRUE,
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

    mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900)
    preDictDfm <- dfm(mycorpus, removePunct = TRUE, removeNumbers = TRUE)
    
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

test_that("test c.corpus",
    expect_that(
        matrix(dfm(corpus(c('What does the fox say?', 'What does the fox say?', '')), removePunct = TRUE)),
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

    dfm1 <- dfm(c(fox, fox), removePunct = TRUE)

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
    dfm1 <- dfm('What does the fox?', removePunct = TRUE)
    dfm2 <- dfm('fox say', removePunct = TRUE)

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

    dfm1 <- dfm('What does the?', removePunct = TRUE)
    dfm2 <- dfm('fox say fox', removePunct = TRUE)
    dfm3 <- dfm('The quick brown fox', removePunct = TRUE)

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
        rbind(dfm(fox, removePunct = TRUE)),
        is_a('dfmSparse')
    )
})

test_that("test that rbind.dfm with a single argument prints a warning", {
    fox <-'What does the fox say?'
    expect_that(
        rbind(dfm(fox, removePunct = TRUE)),
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

    dfm1 <- dfm(c(fox, xof), removePunct = TRUE)

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
})

test_that("dfm keeps all types with > 10,000 documents (#438)", {
    generate_testdfm <- function(n) {
        dfm(paste('X', 1:n, sep=''))
    }
    expect_equal(nfeature(generate_testdfm(10000)), 10000)
    expect_equal(nfeature(generate_testdfm(20000)), 20000)
})
