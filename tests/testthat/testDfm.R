
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
microbenchmark::microbenchmark(
    dfm(inaugTextsTokenized, verbose = FALSE),
    dfm(inaugTextsTokenized, verbose = FALSE, codeType = "old"),
    dfm(inaugTextsTokenized, dictionary = mydict, verbose = FALSE),
    dfm(inaugTextsTokenized, dictionary = mydict, verbose = FALSE, codeType = "old")
)

## need to be carefully inspected!
txt <- "The tall brown trees with pretty leaves in its branches."
dfm(txt)
dfm(txt, stem = TRUE)
dfm(txt, ignoredFeatures = stopwords("english"))  ## FAILS
dfm(txt, stem = TRUE, ignoredFeatures = stopwords("english"))  ## FAILS
