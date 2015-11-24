
mycorpus <- subset(inaugCorpus, Year > 1900)
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

preDictDfm <- dfm(mycorpus)
applyDictionary(preDictDfm, mydict)

txt <- tokenize(toLower(c("My Christmas was ruined by your opposition tax plan.", 
                          "The United_States has progressive taxation.")),
                removePunct = TRUE)


dfm(txt, dictionary = mydict, verbose = TRUE)
dfm(txt, thesaurus = mydict, verbose = TRUE)
dfm(txt, dictionary = mydict, verbose = TRUE, codeType = "old")
dfm(txt, thesaurus = mydict, verbose = TRUE)

(txtDfm <- dfm(txt, verbose = FALSE))
applyDictionary(txtDfm, mydict, valuetype = "glob") 
applyDictionary(txtDfm, mydict, exclusive = FALSE, valuetype = "glob", verbose = FALSE) 


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
dfm(txt, ignoredFeatures = stopwords("english"))  
dfm(txt, stem = TRUE, ignoredFeatures = stopwords("english"))


myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                          opposition = c("Opposition", "reject", "notincorpus"),
                          taxglob = "tax*",
                          taxregex = "tax.+$",
                          country = c("United_States", "Sweden")))
myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
               "Does the United_States or Sweden have more progressive taxation?"),
             ignoredFeatures = stopwords("english"),
             verbose = FALSE)
myDfm
# glob format
(tmp <- applyDictionary(myDfm, myDict, valuetype = "glob", case_insensitive = TRUE))
expect_equal(as.vector(tmp[, c("christmas", "country")]), c(1, 0, 0, 2))
(tmp <- applyDictionary(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE))
expect_equal(as.vector(tmp[, c("christmas", "country")]), c(0, 0, 0, 0))
# regex v. glob format
(tmp <- applyDictionary(myDfm, myDict, valuetype = "glob", case_insensitive = TRUE))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex")]), c(1, 1, 0, 0))
(tmp <- applyDictionary(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex")]), c(1, 2, 0, 1))
## note: "united_states" is a regex match for "tax*"!!

(tmp <- applyDictionary(myDfm, myDict, valuetype = "fixed"))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex", "country")]), c(0, 0, 0, 0, 0, 2))
(tmp <- applyDictionary(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE))
expect_equal(as.vector(tmp[, c("taxglob", "taxregex", "country")]), c(0, 0, 0, 0, 0, 0))

nfeature(trim(preDictDfm, minCount = 7))
nfeature(trim(preDictDfm, minCount = 0.001))
#
# Trim function
#
expect_equal(nfeature(trim(preDictDfm, minDoc = 0.05)), 3077)
expect_equal(nfeature(trim(preDictDfm, minDoc = 2)), 3077)
expect_equal(nfeature(trim(preDictDfm, minCount = 0.001)), 1045)
expect_equal(nfeature(trim(preDictDfm, minCount = 7)), 1045)

expect_equal(nfeature(trim(preDictDfm, sparsity = 0.95)), 3077)
expect_equal(nfeature(trim(preDictDfm, sparsity = 0.95)), nfeature(trim(preDictDfm, minDoc = 0.05)))
expect_equal(nfeature(trim(preDictDfm, minDoc = 0.05)), 3077)




