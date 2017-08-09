context('Testing dfm*.R')

test_that("oldest dfm test", {
    mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900)
    mydict <- dictionary(list(christmas=c("Christmas", "Santa", "holiday"),
                              opposition=c("Opposition", "reject", "notincorpus"),
                              taxing="taxing",
                              taxation="taxation",
                              taxregex="tax*",
                              country="united_states"))
    dictDfm <- dictDfm <- dfm(mycorpus, dictionary = mydict, valuetype = "glob")
    dictDfm <- dictDfm[1:10, ]
    dictDfm <- thesDfm <- dfm(mycorpus, thesaurus = mydict, valuetype = "glob")
    dictDfm <- thesDfm[1:10, (nfeature(thesDfm)-8) : nfeature(thesDfm)]
    
    preDictDfm <- dfm(mycorpus, remove_punct = TRUE, remove_numbers = TRUE)
    dfm_lookup(preDictDfm, mydict)
    
    txt <- tokens(char_tolower(c("My Christmas was ruined by your opposition tax plan.", 
                                   "The United_States has progressive taxation.")),
                    remove_punct = TRUE)
    
    
    dictDfm <- dfm(txt, dictionary = mydict, verbose = FALSE)
    dictDfm <- dfm(txt, thesaurus = mydict, verbose = FALSE)
    dictDfm <- dfm(txt, thesaurus = mydict, verbose = FALSE)
    
    txtDfm <- dfm(txt, verbose = FALSE)
    dictDfm <- dfm_lookup(txtDfm, mydict, valuetype = "glob") 
    dictDfm <- dfm_lookup(txtDfm, mydict, exclusive = FALSE, valuetype = "glob", verbose = FALSE) 
    
    
    inaugTextsTokenized <- tokens(data_corpus_inaugural, remove_punct = TRUE)
    inaugTextsTokenized <- tokens_tolower(inaugTextsTokenized)
    
    # microbenchmark::microbenchmark(
    #     dfm(inaugTextsTokenized, verbose = FALSE),
    #     dfm(inaugTextsTokenized, dictionary = mydict, verbose = FALSE),
    # )
    
    ## need to be carefully inspected!
    txt <- "The tall brown trees with pretty leaves in its branches."
    txtDfm <- dfm(txt)
    txtDfm <- dfm(txt, stem = TRUE)
    txtDfm <- dfm(txt, remove = stopwords("english"))
    txtDfm <- dfm(txt, stem = TRUE, remove = stopwords("english"))
    
    
    myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                              opposition = c("Opposition", "reject", "notincorpus"),
                              taxglob = "tax*",
                              taxregex = "tax.+$",
                              country = c("United_States", "Sweden")))
    myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
                   "Does the United_States or Sweden have more progressive taxation?"),
                 remove = stopwords("english"), remove_punct = TRUE, tolower = FALSE,
                 verbose = FALSE)
    
    # glob format
    tmp <- dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = TRUE)
    expect_equal(as.vector(tmp[, c("christmas", "country")]), c(1, 0, 0, 2))
    tmp <- dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE)
    expect_equal(as.vector(tmp[, c("christmas", "country")]), c(0, 0, 0, 0))
    # regex v. glob format
    tmp <- dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = TRUE)
    expect_equal(as.vector(tmp[, c("taxglob", "taxregex")]), c(1, 1, 0, 0))
    tmp <- dfm_lookup(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE)
    expect_equal(as.vector(tmp[, c("taxglob", "taxregex")]), c(1, 2, 0, 1))
    ## note: "united_states" is a regex match for "tax*"!!
    
    tmp <- dfm_lookup(myDfm, myDict, valuetype = "fixed")
    expect_equal(as.vector(tmp[, c("taxglob", "taxregex", "country")]), c(0, 0, 0, 0, 0, 2))
    tmp <- dfm_lookup(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE)
    expect_equal(as.vector(tmp[, c("taxglob", "taxregex", "country")]), c(0, 0, 0, 0, 0, 0))

})

test_that("test c.corpus", {
    expect_that(
        matrix(dfm(corpus(c('What does the fox say?', 'What does the fox say?', '')), remove_punct = TRUE)),
        equals(matrix(rep(c(1, 1, 0), 5), nrow=15, ncol=1))
    )
})

## rbind.dfm
## TODO: Test classes returned

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

# TODO: Add function for testing the equality of dfms

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
        ## Order of the result is not guaranteed
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
    testdfm <- dfm(tokens(data_corpus_irishbudget2010, old= TRUE))
    expect_output(print(testdfm),
                  "^Document-feature matrix of: 14 documents, 5,055 features \\(80.9% sparse\\)")
    expect_output(print(testdfm[1:5, 1:5]),
                  "^Document-feature matrix of: 5 documents, 5 features \\(28% sparse\\).*")
    expect_output(head(testdfm, 1),
                  "Document-feature matrix of: 14 documents, 5,055 features.*showing first document and first 6 features.*")
    expect_output(tail(testdfm, 1),
                  "Document-feature matrix of: 14 documents, 5,055 features.*showing last document and last 6 features.*")
})

test_that("dfm print works as expected", {
    testdfm <- dfm(data_corpus_irishbudget2010)
    expect_output(print(testdfm),
                  "^Document-feature matrix of: 14 documents, 5,140 features \\(8.{3}% sparse\\)")
    expect_output(print(testdfm[1:5, 1:5]),
                  "^Document-feature matrix of: 5 documents, 5 features \\(28% sparse\\).*")
    expect_output(head(testdfm, 1),
                  "Document-feature matrix of: 14 documents, 5,\\d{3} features.*showing first document and first 6 features.*")
    expect_output(tail(testdfm, 1),
                  "Document-feature matrix of: 14 documents, 5,\\d{3} features.*showing last document and last 6 features.*")
})


test_that("dfm.dfm works as expected", {
    testdfm <- dfm(data_corpus_irishbudget2010, tolower = TRUE)
    expect_identical(testdfm, dfm(testdfm, tolower = FALSE))
    expect_identical(testdfm, dfm(testdfm, tolower = TRUE))
    groupeddfm <- dfm(testdfm,
                      groups =  ifelse(docvars(data_corpus_irishbudget2010, "party") %in% c("FF", "Green"), "Govt", "Opposition"),
                      tolower = FALSE)
    expect_identical(colSums(groupeddfm), colSums(groupeddfm))
    expect_identical(docnames(groupeddfm), c("Govt", "Opposition"))
    expect_identical(testdfm, dfm(testdfm))

    dict <- dictionary(list(articles = c("the", "a", "an"),
                            preps = c("of", "for", "in")))
    expect_equivalent(
        dfm(data_corpus_irishbudget2010, dictionary = dict),
        dfm(testdfm, dictionary = dict)
    )
    expect_identical(
        dfm(data_corpus_irishbudget2010, stem = TRUE),
        dfm(testdfm, stem = TRUE)
    )
})

test_that("dfm_sample works as expected",{
    myDfm <- dfm(data_corpus_inaugural[1:10], verbose = FALSE)
    expect_error(dfm_sample(myDfm, margin = "documents", size = 20),
                  "size cannot exceed the number of documents \\(10\\)")
    expect_error(dfm_sample(myDfm, margin = "features", size = 3500),
                 "size cannot exceed the number of features \\(33\\d{2}\\)")
    expect_error(dfm_sample(data_corpus_inaugural[1:10]))
})


test_that("cbind.dfm works as expected",{
    dfm1 <- dfm("This is one sample text sample")
    dfm2 <- dfm("More words here")
    dfm12 <- cbind(dfm1, dfm2)

    expect_equal(nfeature(dfm12), 8)
    expect_equal(names(dimnames(dfm12)),
                 c("docs", "features"))
})

test_that("cbind.dfm works with non-dfm objects",{
    dfm1 <- dfm(c("a b c", "c d e"))
    
    vec <- c(10, 20)
    expect_equal(
        as.matrix(cbind(dfm1, vec)),
        matrix(c(1,1,1,0,0,10, 0,0,1,1,1,20), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat")))
    )
    expect_equal(
        as.matrix(cbind(vec, dfm1)),
        matrix(c(10,1,1,1,0,0, 20, 0,0,1,1,1), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c("feat", letters[1:5])))
    )
    
    mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("f1", "f2")))
    expect_equal(
        as.matrix(cbind(dfm1, mat)),
        matrix(c(1,1,1,0,0,1,3, 0,0,1,1,1,2,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "f1", "f2")))
    )
    expect_equal(
        as.matrix(cbind(mat, dfm1)),
        matrix(c(1,3,1,1,1,0,0, 2,4,0,0,1,1,1), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c("f1", "f2", letters[1:5])))
    )
    
    expect_equal(
        as.matrix(cbind(dfm1, vec, mat)),
        matrix(c(1,1,1,0,0,10,1,3, 0,0,1,1,1,20,2,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat", "f1", "f2")))
    )

    expect_equal(
        as.matrix(cbind(vec, dfm1, vec)),
        matrix(c(10,1,1,1,0,0,10, 20,0,0,1,1,1,20), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c("feat", letters[1:5], "feat1")))
    )

    expect_silent(cbind(vec, dfm1, vec))
    expect_warning(
        cbind(dfm1, dfm1),
        "cbinding dfms with overlapping features will result in duplicated features"
    )
    
    expect_equal(
        as.matrix(cbind(dfm1, 100)),
        matrix(c(1,1,1,0,0,100, 0,0,1,1,1,100), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat")))
    )
    
})

test_that("more cbind tests for dfms", {
    txts <- c("a b c d", "b c d e") 
    mydfm <- dfm(txts)
    
    expect_equal(
        as.matrix(cbind(mydfm, as.dfm(cbind("ALL" = ntoken(mydfm))))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
                  dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "ALL")))
    )
    
    expect_equal(
        as.matrix(cbind(mydfm, cbind("ALL" = ntoken(mydfm)))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "ALL")))
    )

    expect_equal(
        as.matrix(cbind(mydfm, "ALL" = ntoken(mydfm))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "ALL")))
    )
    expect_equal(
        as.matrix(cbind(mydfm, ntoken(mydfm))),
        matrix(c(1,1,1,1,0,4, 0,1,1,1,1,4), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c("text1", "text2"), features = c(letters[1:5], "feat")))
    )
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
                   'removed 2 features')
    expect_message(dfm(txt, select = c('a', 'f'), verbose = TRUE),
                   'kept 2 features')
})

test_that("dfm print works with options as expected", {
    tmp <- dfm(data_corpus_irishbudget2010, remove_punct = FALSE, remove_numbers = FALSE, remove_hyphens = TRUE)
    expect_output(
        head(tmp),
        "Document-feature matrix of: 14 documents, 5,\\d{3} features.*\\(showing first 6 documents and first 6 features\\)"
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
        "^Document-feature matrix of: 3 documents, 3 features.*2 x 3 sparse Matrix.*features"
    )
    expect_output(
        print(tmp[1:3, 1:3], ndoc = 2, nfeature = 2),
        "^Document-feature matrix of: 3 documents, 3 features \\(22.2% sparse\\)\\.$"
    )
    expect_output(
        print(tmp[1:5, 1:5], show.summary = FALSE),
        "^5 x 5 sparse Matrix"
    )

    # with options (#756)
    quanteda_options(print_dfm_max_ndoc = 22L)
    quanteda_options(print_dfm_max_nfeature = 22L)
    expect_output(
        print(tmp),
        "Document-feature matrix of: 14 documents, 5,\\d{3} features \\(8\\d\\.\\d% sparse\\)\\.$"
    )
    expect_output(
        print(tmp[, 1:21]),
        "Document-feature matrix of: 14 documents, 21 features \\(20.4% sparse\\)\\..*14 x 21 sparse Matrix"
    )
})

test_that("cannot supply remove and select in one call (#793)", {
    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    corp <- corpus(txt, docvars = data.frame(grp = c(1, 1, 2)))
    toks <- tokens(corp)
    expect_error(
        dfm(txt, select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
    expect_error(
        dfm(corp, select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
    expect_error(
        dfm(toks, select = "one", remove = "two"),
        "only one of select and remove may be supplied at once"
    )
})

test_that("printing an empty dfm produces informative result (#811)", {
    my_dictionary <- dictionary(list( a = c( "asd", "dsa" ),
                                      b = c( "foo", "jup" ) ) )
    raw_text <- c( "Wow I can't believe it's not raining!", 
                   "Today is a beautiful day. The sky is blue and there are burritos" )
    my_corpus <- corpus( raw_text )
    my_dfm <- dfm( my_corpus, dictionary = my_dictionary )
    
    expect_output(
        print(my_dfm),
        "^Document-feature matrix of: 2 documents, 2 features \\(100% sparse\\)\\.\\n2 x 2 sparse Matrix of class \"dfmSparse\""
    )
    expect_output(
        print(my_dfm[-c(1, 2), ]),
        "^Document-feature matrix of: 0 documents, 2 features\\.\\n0 x 2 sparse Matrix of class \"dfmSparse\""
    )
    expect_output(
        print(my_dfm[, -c(1, 2)]),
        "^Document-feature matrix of: 2 documents, 0 features\\.\\n2 x 0 sparse Matrix of class \"dfmSparse\""
    )
})

test_that("dfm with selection options produces correct output", {
    txt <- c(d1 = 'a b', d2 = 'a b c d e')
    toks <- tokens(txt)
    dfmt <- dfm(toks)
    feat <- c('b', 'c', 'd', 'e', 'f', 'g')
    expect_message(
        dfm(txt, remove = feat, verbose = TRUE),
        "removed 4 features" 
    )
    expect_message(
        dfm(toks, remove = feat, verbose = TRUE),
        "removed 4 features" 
    )
    expect_message(
        dfm(dfmt, remove = feat, verbose = TRUE),
        "removed 4 features" 
    )
})

test_that("dfm works with stem options", {
    txt_english <- "running ran runs"
    txt_french <- "courant courir cours"
    
    quanteda_options(language_stemmer = "english")
    expect_equal(
        as.character(tokens_wordstem(tokens(txt_english))),
        c("run", "ran", "run")
    )
    expect_equal(
        featnames(dfm(txt_english)),
        c("running", "ran", "runs")
    )
    expect_equal(
        featnames(dfm(txt_english, stem = TRUE)),
        c("run", "ran")
    )

    quanteda_options(language_stemmer = "french")
    expect_equal(
        as.character(tokens_wordstem(tokens(txt_french))),
        rep("cour", 3)
    )
    expect_equal(
        featnames(dfm(txt_french)),
        c("courant", "courir", "cours")
    )
    expect_equal(
        featnames(dfm(txt_french, stem = TRUE)),
        "cour"
    )
    quanteda_options(reset = TRUE)
})
