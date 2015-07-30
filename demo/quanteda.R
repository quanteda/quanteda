# a demonstration of the capabilities of quanteda

str(ukimmigTexts)
mycorpus <- corpus(ukimmigTexts,
                   docvars=list(party=names(ukimmigTexts), year=2010L, country="UK"),
                   notes="Immigration-related sections from UK 2010 party manifestos.")
encoding(mycorpus) <- "UTF-8"
language(mycorpus) <- "english"
summary(mycorpus, showmeta=TRUE)

kwic(mycorpus, "deport", 3)
kwic(mycorpus, "deport", 3, wholeword=TRUE)

sort(lexdiv(dfm(mycorpus, verbose=FALSE), "TTR"))

mydfm <- dfm(mycorpus, stem=TRUE, ignoredFeatures=stopwords("english"))
docnames(mydfm)
features(mydfm)

# open a nicer quartz device on a mac
if (Sys.info()[1]=="Darwin") quartz("BNP 2010 word cloud", 7,7)
# this is necessary currently because no subset is yet implemented for dfm objects
plot(dfm(subset(mycorpus, party=="BNP"), 
         ignoredFeatures = stopwords("english")))

# some examples of tokenization and string cleaning
tokenize(exampleString, removePunct = TRUE)
wordstem(exampleString)

