# The main demo file for quanteda

str(uk2010immig)
mycorpus <- corpus(uk2010immig, 
                   docvars=list(party=names(uk2010immig)),
                   notes="Immigration-related sections from UK 2010 party manifestos.")
encoding(mycorpus) <- "UTF-8"
language(mycorpus) <- "english"
summary(mycorpus, showmeta=TRUE)

kwic(mycorpus, "deport", 3)

mydfm <- dfm(mycorpus, stem=TRUE, stopwords=TRUE)
docnames(mydfm)
features(mydfm)
# open a nicer quartz device on a mac
if (Sys.info()[1]=="Darwin") quartz("BNP 2010 word cloud", 7,7)
# this is necessary currently because no subset is yet implemented for dfm objects
plot(dfm(subset(mycorpus, party=="BNP"), stem=TRUE, stopwords=TRUE))

# some examples of tokenization and string cleaning
library(quantedaData)
data(exampleString)
tokenize(exampleString)
clean(exampleString)
wordstem(exampleString)


