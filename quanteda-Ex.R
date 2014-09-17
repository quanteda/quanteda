pkgname <- "quanteda"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('quanteda')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("MCMCirtPoisson1d")
### * MCMCirtPoisson1d

flush(stderr()); flush(stdout())

### Name: MCMCirtPoisson1d
### Title: Bayesian-MCMC version of a 1-dimensional Poisson IRT scaling
###   model
### Aliases: MCMCirtPoisson1d

### ** Examples

## Not run: 
##D data(iebudgets)
##D # extract just the 2010 debates
##D iebudgets2010 <- subset(iebudgets, year==2010)
##D 
##D # create a document-term matrix and set the word margin to the columns
##D dtm <- dfm(iebudgets2010)
##D 
##D # estimate the maximium likelihood wordfish model from austin
##D require(austin)
##D iebudgets2010_wordfish <- wordfish(as.wfm(dtm, word.margin=2), dir=c(2,1))
##D 
##D # estimate the MCMC model, default values
##D iebudgets2010_wordfishMCMC <- MCMCirtPoisson1d(dtm, itembase="the", dir=c(2,1))
##D iebudgets2010_wordfishMCMC_unconstrained <- MCMCirtPoisson1d(dtm, dir=c(2,1))
##D 
##D # compare the estimates of \eqn{\theta_i}
##D require(psych)
##D pairs.panels(data.frame(ML=iebudgets2010_wordfish$theta,
##D                         PoissonThe=iebudgets2010_wordfishMCMC$theta,
##D                         PoissonUnconst=iebudgets2010_wordfishMCMC_unconstrained$theta),
##D              smooth=FALSE, scale=FALSE, ellipses=FALSE, lm=TRUE, cex.cor=2.5)
##D # inspect a known "opposition" word beta values
##D iebudgets2010_wordfish$beta[which(iebudgets2010_wordfishMCMC_unconstrained$words=="fianna")]
##D iebudgets2010_wordfishMCMC$beta[which(iebudgets2010_wordfishMCMC_unconstrained$words=="fianna")]
##D iebudgets2010_wordfishMCMC_unconstrained$beta[which(iebudgets2010_wordfishMCMC_unconstrained$words=="fianna")]
##D 
##D # random starting values, for three chains
##D dtm.sample <- trim(dtm, sample=200)
##D iebudgets2010_wordfishMCMC_sample <- MCMCirtPoisson1d(dtm.sample, dir=c(2,1), startRandom=TRUE, nChains=3)
## End(Not run)



cleanEx()
nameEx("bigrams")
### * bigrams

flush(stderr()); flush(stdout())

### Name: bigrams
### Title: Create bigrams
### Aliases: bigrams

### ** Examples

bigrams("The quick brown fox jumped over the lazy dog.")
bigrams("The quick brown fox jumped over the lazy dog.", window=2)



cleanEx()
nameEx("clean")
### * clean

flush(stderr()); flush(stdout())

### Name: clean
### Title: clean a set of texts
### Aliases: clean clean.character clean.corpus

### ** Examples

clean(inaugTexts, removeDigits=FALSE)
clean(inaugCorpus, removeDigits=FALSE)



cleanEx()
nameEx("collocations")
### * collocations

flush(stderr()); flush(stdout())

### Name: collocations
### Title: Detect collocations in a text
### Aliases: collocations

### ** Examples

data(inaugCorpus)
collocations(texts(inaugCorpus)[1], top=50)
collocations(texts(inaugCorpus)[1], top=50, method="chi2")



cleanEx()
nameEx("corpus")
### * corpus

flush(stderr()); flush(stdout())

### Name: corpus
### Title: Constructor for corpus objects
### Aliases: corpus corpus.character corpus.directory is.corpus

### ** Examples

## Not run: 
##D # import texts from a directory of files
##D corpus(directory("~/Dropbox/QUANTESS/corpora/ukManRenamed"),
##D        enc="UTF-8",
##D        source="Ken's UK manifesto archive")
##D 
##D # choose a directory using a GUI
##D corpus(directory())
## End(Not run)
#
# create a corpus from texts
corpus(inaugTexts)

# create a corpus from texts and assign meta-data and document variables
uk2010immigCorpus <- corpus(uk2010immig,
                            docvars=data.frame(party=names(uk2010immig)),
                            enc="UTF-8")



cleanEx()
nameEx("countSyllables")
### * countSyllables

flush(stderr()); flush(stdout())

### Name: countSyllables
### Title: Returns a count of the number of syllables in the input This
###   function takes a text and returns a count of the number of syllables
###   it contains. For British English words, the syllable count is exact
###   and looked up from the CMU pronunciation dictionary. For any word not
###   in the dictionary the syllable count is estimated by counting vowel
###   clusters.
### Aliases: countSyllables

### ** Examples

countSyllables("This is an example sentence.")
myTexts <- c("Text one.", "Superduper text number two.", "One more for the road.")
names(myTexts) <- paste("myText", 1:3, sep="")
countSyllables(myTexts)



cleanEx()
nameEx("describeTexts")
### * describeTexts

flush(stderr()); flush(stdout())

### Name: describeTexts
### Title: print a summary of texts Prints to the console a desription of
###   the texts, including number of types, tokens, and sentences
### Aliases: describeTexts

### ** Examples

describeTexts(c("testing this text", "and this one"))
describeTexts(uk2010immig)



cleanEx()
nameEx("dfm")
### * dfm

flush(stderr()); flush(stdout())

### Name: dfm
### Title: Create a document-feature matrix from a corpus object
### Aliases: dfm dfm.character dfm.corpus is.dfm

### ** Examples

data(inaugCorpus)
wfm <- dfm(inaugCorpus)

## by president, after 1960
wfmByPresfrom1900 <- dfm(subset(inaugCorpus, Year>1900), groups="President")
docnames(wfmByPresfrom1900)

## with dictionaries
data(inaugCorpus)
mycorpus <- subset(inaugCorpus, Year>1900)
mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
               opposition=c("Opposition", "reject", "notincorpus"),
               taxing="taxing",
               taxation="taxation",
               taxregex="tax*")
dictDfm <- dfm(mycorpus, dictionary=mydict)
dictDfm

## removing stopwords
testText <- "The quick brown fox named Seamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."
testCorpus <- corpus(testText)
settings(testCorpus, "stopwords")
dfm(testCorpus, stopwords=TRUE)
if (require(tm)) {
}



cleanEx()
nameEx("dfm2ldaformat")
### * dfm2ldaformat

flush(stderr()); flush(stdout())

### Name: dfm2ldaformat
### Title: Convert a quanteda 'dfm' (document feature matrix) into a the
###   data format needed by lda
### Aliases: dfm2ldaformat

### ** Examples

data(inaugCorpus)
inaugCorpus <- subset(inaugCorpus, year>1960)
# create document-feature matrix, remove stopwords
d <- dfm(inaugCorpus, stopwords=TRUE)
# trim low frequency words
d <- dfmTrim(d, minCount=5, minDoc=3)
td <- dfm2ldaformat(d)
if (require(lda)) {
    tmodel.lda <- result <- lda.collapsed.gibbs.sampler(documents=td$documents,
                                                        K=10,
                                                        vocab=td$vocab,
                                                        num.iterations=50, alpha=0.1, eta=0.1)
}
top.topic.words(tmodel.lda$topics, 10, by.score=TRUE) # top five words in each topic



cleanEx()
nameEx("dfm2tmformat")
### * dfm2tmformat

flush(stderr()); flush(stdout())

### Name: dfm2tmformat
### Title: Convert a quanteda 'dfm' (document feature matrix) into a 'tm'
###   DocumentTermMatrix
### Aliases: dfm2tmformat

### ** Examples

data(inaugCorpus)
inaugCorpus <- subset(inaugCorpus, year==2010)
d <- dfmTrim(dfm(inaugCorpus), minCount=5, minDoc=3)
dim(d)
td <- dfm2tmformat(d)
length(td$v)
if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)



cleanEx()
nameEx("dfmSort")
### * dfmSort

flush(stderr()); flush(stdout())

### Name: dfmSort
### Title: sort a dfm by one or more margins
### Aliases: dfmSort

### ** Examples

data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dtm[, 1:10]
dtm <- dfmSort(dtm, "words")
dfmSort(dtm)[, 1:10]
dfmSort(dtm, "both")[, 1:10]



cleanEx()
nameEx("dfmTrim")
### * dfmTrim

flush(stderr()); flush(stdout())

### Name: dfmTrim
### Title: Trim a dfm based on a subset of features and words
### Aliases: dfmTrim

### ** Examples

data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dim(dtm)
dtmReduced <- dfmTrim(dtm, minCount=10, minDoc=2) # only words occuring at least 5 times and in at least 2documents
dim(dtmReduced)
dtmSampled <- dfmTrim(dtm, sample=50)  # top 200 words
dim(dtmSampled)  # 196 x 200 words



cleanEx()
nameEx("directory")
### * directory

flush(stderr()); flush(stdout())

### Name: directory
### Title: Function to declare a connection to a directory (containing
###   files)
### Aliases: directory

### ** Examples

## Not run: 
##D # name a directory of files
##D mydir <- directory("~/Dropbox/QUANTESS/corpora/ukManRenamed")
##D corpus(mydir)
##D 
##D # choose a directory using a GUI
##D corpus(directory())
## End(Not run)



cleanEx()
nameEx("docnames")
### * docnames

flush(stderr()); flush(stdout())

### Name: docnames
### Title: extract document names
### Aliases: docnames docnames.corpus docnames.dfm docnames<-

### ** Examples

# query the document names of the inaugural speech corpus
docnames(inaugCorpus) <- paste("Speech", 1:ndoc(inaugCorpus), sep="")

# reassign the document names of the inaugural speech corpus
docnames(inaugCorpus) <- paste("Speech", 1:ndoc(inaugCorpus), sep="")
#
# query the document names of a dfm
docnames(dfm(inaugTexts[1:5]))



cleanEx()
nameEx("docvars")
### * docvars

flush(stderr()); flush(stdout())

### Name: docvars
### Title: get or set for document-level variables
### Aliases: docvars docvars<-

### ** Examples

head(docvars(inaugCorpus))
docvars(inaugCorpus, "President") <- paste("prez", 1:ndoc(inaugCorpus), sep="")
head(docvars(inaugCorpus))



cleanEx()
nameEx("features.dfm")
### * features.dfm

flush(stderr()); flush(stdout())

### Name: features.dfm
### Title: extract the feature labels from a dfm
### Aliases: features features.dfm

### ** Examples

features(dfm(inaugTexts))[1:50]  # first 50 features (alphabetically sorted)



cleanEx()
nameEx("flatten.dictionary")
### * flatten.dictionary

flush(stderr()); flush(stdout())

### Name: flatten.dictionary
### Title: Flatten a hierarchical dictionary into a list of character
###   vectors
### Aliases: flatten.dictionary

### ** Examples

dictPopulismEN <-
    list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
                    "corrupt*", "propagand", "politici*", "*deceit*",
                    "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
                    "dishonest*", "establishm*", "ruling*"))
flatten.dictionary(dictPopulismEN)

hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
                             level1a2 = c("l1a21", "l1a22")),
              level1b = list(level1b1 = c("l1b11", "l1b12"),
                             level1b2 = c("l1b21", "l1b22", "l1b23")),
              level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
                             level1c1b = list(level1c1b1 = c("lowestalone"))))
flatten.dictionary(hdict)



cleanEx()
nameEx("getRootFileNames")
### * getRootFileNames

flush(stderr()); flush(stdout())

### Name: getRootFileNames
### Title: Truncate absolute filepaths to root filenames
### Aliases: getRootFileNames

### ** Examples

## Not run: 
##D getRootFileNames('/home/paul/documents/libdem09.txt')
## End(Not run)



cleanEx()
nameEx("getTextDir")
### * getTextDir

flush(stderr()); flush(stdout())

### Name: getTextDir
### Title: loads all text files from a given directory
### Aliases: getTextDir

### ** Examples

## Not run: 
##D getTextDir('/home/paul/documents/')
## End(Not run)



cleanEx()
nameEx("getTextDirGui")
### * getTextDirGui

flush(stderr()); flush(stdout())

### Name: getTextDirGui
### Title: provides a gui interface to choose a gui to load texts from
### Aliases: getTextDirGui

### ** Examples

## Not run: 
##D getTextFiles('/home/paul/documents/libdem09.txt')
## End(Not run)



cleanEx()
nameEx("getTextFiles")
### * getTextFiles

flush(stderr()); flush(stdout())

### Name: getTextFiles
### Title: load text files from disk into a vector of character vectors
###   points to files, reads them into a character vector of the texts with
###   optional names, default being filenames returns a named vector of
###   complete, unedited texts
### Aliases: getTextFiles

### ** Examples

## Not run: 
##D getTextFiles('/home/paul/documents/libdem09.txt')
## End(Not run)



cleanEx()
nameEx("inaugCorpus")
### * inaugCorpus

flush(stderr()); flush(stdout())

### Name: inaugCorpus
### Title: A corpus of US presidential inaugural addresses from 1789-2013
### Aliases: inaugCorpus inaugTexts

### ** Examples

# some operations on the inaugural corpus
data(inaugCorpus)
summary(inaugCorpus)
head(docvars(inaugCorpus), 10)
# working with the character vector only
data(inaugTexts)
str(inaugTexts)
head(docvars(inaugCorpus), 10)
mycorpus <- corpus(inaugTexts)



cleanEx()
nameEx("kwic")
### * kwic

flush(stderr()); flush(stdout())

### Name: kwic
### Title: List key words in context from a text or a corpus of texts.
### Aliases: kwic kwic.character kwic.corpus

### ** Examples

kwic(inaugTexts, "terror")
kwic(inaugTexts, "terror", regex=FALSE)  # returns only whole word, without trailing punctuation
data(iebudgets)
kwic(subset(iebudgets, year==2010), "Christmas", window=4) # on a corpus



cleanEx()
nameEx("kwic2")
### * kwic2

flush(stderr()); flush(stdout())

### Name: kwic2
### Title: This function is an alternative KWIC
### Aliases: kwic2

### ** Examples

## Not run: 
##D kwic2(texts, "we", filter = '_2010', location=TRUE)
## End(Not run)



cleanEx()
nameEx("metacorpus")
### * metacorpus

flush(stderr()); flush(stdout())

### Name: metacorpus
### Title: get or set corpus metadata
### Aliases: metacorpus metacorpus<-

### ** Examples

metacorpus(inaugCorpus)
metacorpus(inaugCorpus, "source")
metacorpus(inaugCorpus, "citation") <- "Presidential Speeches Online Project (2014)."
metacorpus(inaugCorpus, "citation")



cleanEx()
nameEx("metadoc")
### * metadoc

flush(stderr()); flush(stdout())

### Name: metadoc
### Title: get or set document-level meta-data
### Aliases: metadoc

### ** Examples

mycorp <- subset(inaugCorpus, Year>1990)
summary(mycorp, showmeta=TRUE)
metadoc(mycorp, "encoding") <- "UTF-8"
metadoc(mycorp)
metadoc(mycorp, "language") <- "english"
summary(mycorp, showmeta=TRUE)



cleanEx()
nameEx("ndoc")
### * ndoc

flush(stderr()); flush(stdout())

### Name: ndoc
### Title: get the number of documents
### Aliases: ndoc ndoc.corpus ndoc.dfm

### ** Examples

ndoc(inaugCorpus)
ndoc(dfm(inaugCorpus))



cleanEx()
nameEx("ngrams")
### * ngrams

flush(stderr()); flush(stdout())

### Name: ngrams
### Title: Create ngrams
### Aliases: ngrams

### ** Examples

ngrams("The quick brown fox jumped over the lazy dog.", n=2)
ngrams("The quick brown fox jumped over the lazy dog.", n=3)
ngrams("The quick brown fox jumped over the lazy dog.", n=3, concatenator="~")
ngrams("The quick brown fox jumped over the lazy dog.", n=3, include.all=TRUE)



cleanEx()
nameEx("preprocess")
### * preprocess

flush(stderr()); flush(stdout())

### Name: preprocess
### Title: preprocess the tokens in a corpus
### Aliases: preprocess

### ** Examples

mycorpus <- corpus(uk2010immig)
mycorpus
preprocess(mycorpus)
mycorpus
mydfm <- dfm(mycorpus)



cleanEx()
nameEx("selectFeatures")
### * selectFeatures

flush(stderr()); flush(stdout())

### Name: selectFeatures
### Title: extract feature words This function takes type of feature
###   extractor and a word freaquency matrix with binary class (1/0) to
###   select features in class one. 'wsll' and 'wschisq' replicates of
###   'Keyness' of Wordsmith Tools.
### Aliases: selectFeatures

### ** Examples

## Not run: 
##D texts <- getTextDir("/home/kohei/Documents/budget_2010/")
##D class  <- rep(0, length(texts))
##D class[grep("_LAB", names(texts))] <- 1
##D class[grep("_FF", names(texts))] <- 0
##D corpus <- corpusCreate(texts, attribs=list(class=class))
##D dfm <- dfm(corpus)
##D features <- selectFeatures('ll', dfm, corpus$attribs$class, smooth=1)
## End(Not run)
## Not run: 
##D texts <- getTextDir("/home/kohei/Documents/budget_2010/")
##D class  <- rep(0, length(texts))
##D class[grep("_LAB", names(texts))] <- 1
##D class[grep("_FF", names(texts))] <- 0
##D corpus <- corpusCreate(texts, attribs=list(class=class))
##D dfm <- dfm(corpus)
##D features <- selectFeatures('ll', dfm, corpus$attribs$class, smooth=1)
## End(Not run)



cleanEx()
nameEx("sentenceSeg")
### * sentenceSeg

flush(stderr()); flush(stdout())

### Name: sentenceSeg
### Title: split a text into sentences This function takes a text and
###   splits it into sentences.
### Aliases: sentenceSeg

### ** Examples

test <- "This is a sentence! Several sentences. It's designed by a Dr. to test whether this function works. Or not? Or not."
sentenceSeg(test)



cleanEx()
nameEx("settings")
### * settings

flush(stderr()); flush(stdout())

### Name: settings
### Title: Get or set the corpus settings
### Aliases: settings settings.corpus settings.dfm settings<-

### ** Examples

settings(tempcorpus, "stopwords")
tempdfm <- dfm(inaugCorpus)
tempdfmSW <- dfm(inaugCorpus, stopwords=TRUE)
settings(inaugCorpus, "stopwords") <- TRUE
tempdfmSW <- dfm(inaugCorpus)
tempdfm <- dfm(inaugCorpus, stem=TRUE)
settings(tempdfm)



cleanEx()
nameEx("sort.dfm")
### * sort.dfm

flush(stderr()); flush(stdout())

### Name: sort.dfm
### Title: sort a dfm by one or more margins
### Aliases: sort.dfm

### ** Examples

dtm <- dfm(inaugCorpus)
dtm[1:10, 1:5]
dtm <- sort(dtm)
sort(dtm)[1:10, 1:5]
sort(dtm, TRUE, "both")[1:10, 1:5]  # note that the decreasing=TRUE argument
                                    # must be second, because of the order of the
                                    # formals in the generic method of sort()



cleanEx()
nameEx("stopwordsGet")
### * stopwordsGet

flush(stderr()); flush(stdout())

### Name: stopwordsGet
### Title: access stopwords
### Aliases: stopwordsGet

### ** Examples

stopwordsGet()
stopwordsGet("italian")



cleanEx()
nameEx("stopwordsRemove")
### * stopwordsRemove

flush(stderr()); flush(stdout())

### Name: stopwordsRemove
### Title: remove stopwords from a text or dfm
### Aliases: stopwordsRemove stopwordsRemove.character
###   stopwordsRemove.matrix

### ** Examples

## examples for character objects
someText <- "Here is an example of text containing some stopwords we want to remove."
itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
stopwordsRemove(someText)
stopwordsRemove(someText, stopwordsGet("SMART"))
stopwordsRemove(itText, stopwordsGet("italian"))
stopwordsRemove(someText, c("containing", "example"))

## example for dfm objects
data(iebudgets)
wfm <- dfm(subset(iebudgets, year==2010))
wfm.nostopwords <- stopwordsRemove(wfm)
dim(wfm)
dim(wfm.nostopwords)
dim(stopwordsRemove(wfm, stopwordsGet("SMART")))



cleanEx()
nameEx("subset.corpus")
### * subset.corpus

flush(stderr()); flush(stdout())

### Name: subset.corpus
### Title: extract a subset of a corpus
### Aliases: subset.corpus

### ** Examples

## Not run: 
##D data(inaugCorpus)
##D summary(subset(inaugCorpus, Year>1980))
## End(Not run)



cleanEx()
nameEx("summary.corpus")
### * summary.corpus

flush(stderr()); flush(stdout())

### Name: summary.corpus
### Title: Corpus summary
### Aliases: summary.corpus

### ** Examples

summary(inaugCorpus)
summary(inaugCorpus, n=10)
mycorpus <- corpus(uk2010immig, docvars=data.frame(party=names(uk2010immig)), enc="UTF-8")
summary(mycorpus, showmeta=TRUE)  # show the meta-data
mysummary <- summary(mycorpus, verbose=FALSE)  # (quietly) assign the results
mysummary$Types / mysummary$Tokens             # crude type-token ratio



cleanEx()
nameEx("syllableCounts")
### * syllableCounts

flush(stderr()); flush(stdout())

### Name: syllableCounts
### Title: A named list mapping words to counts of their syllables
### Aliases: syllableCounts

### ** Examples

data(syllableCounts)
syllableCounts["sixths"]
syllableCounts["onomatopeia"]



cleanEx()
nameEx("tagPos")
### * tagPos

flush(stderr()); flush(stdout())

### Name: tagPos
### Title: Returns a table of the occurrences of different parts of speech
###   in a sentence This function takes a sentence and tags each word with
###   it's part of speech using openNLP's POS tagger, then returns a table
###   of the parts of speech
### Aliases: tagPos

### ** Examples

## Not run: 
##D tagPos("This is an example sentence with nouns and verbs for tagging.")
## End(Not run)



cleanEx()
nameEx("texts")
### * texts

flush(stderr()); flush(stdout())

### Name: texts
### Title: get or set corpus texts
### Aliases: texts texts<-

### ** Examples

texts(inaugCorpus)[1]
sapply(texts(inaugCorpus), nchar)  # length in characters of the inaugual corpus texts

## this doesn't work yet - need to overload `[` for this replacement function
# texts(inaugTexts)[55] <- "GW Bush's second inaugural address, the condensed version."



cleanEx()
nameEx("tf")
### * tf

flush(stderr()); flush(stdout())

### Name: tf
### Title: normalizes the term frequencies a dfm
### Aliases: tf

### ** Examples

data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dtm[1:10, 100:110]
tf(dtm)[1:10, 100:110]



cleanEx()
nameEx("tfidf")
### * tfidf

flush(stderr()); flush(stdout())

### Name: tfidf.dfm
### Title: compute the tf-idf weights of a dfm
### Aliases: tfidf.dfm

### ** Examples

data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dtm[1:10, 100:110]
tfidf(dtm)[1:10, 100:110]
tfidf(dtm, normalize=FALSE)[1:10, 100:110]



cleanEx()
nameEx("tfidf.dfm")
### * tfidf.dfm

flush(stderr()); flush(stdout())

### Name: tfidf.dfm
### Title: compute the tf-idf weights of a dfm
### Aliases: tfidf.dfm

### ** Examples

data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dtm[1:10, 100:110]
tfidf(dtm)[1:10, 100:110]
tfidf(dtm, normalize=FALSE)[1:10, 100:110]



cleanEx()
nameEx("topfeatures")
### * topfeatures

flush(stderr()); flush(stdout())

### Name: topfeatures
### Title: list the most frequent features
### Aliases: topfeatures topfeatures.dfm

### ** Examples

topfeatures(dfm(inaugCorpus))
topfeatures(dfm(inaugCorpus, stopwords=TRUE))
# least frequent features
topfeatures(dfm(inaugCorpus), decreasing=FALSE)



cleanEx()
nameEx("translate")
### * translate

flush(stderr()); flush(stdout())

### Name: translate
### Title: Send text to the google translate research API This function
###   translates a text by sending it to the google translate API.
### Aliases: translate

### ** Examples

## Not run: translation <- translate(original, fr, de, key='insertkeyhere')



cleanEx()
nameEx("translate.corpus")
### * translate.corpus

flush(stderr()); flush(stdout())

### Name: translate.corpus
### Title: Send a corpus to the google translate research API This function
###   translates a the texts in a corpus by sending them to the google
###   translate API.
### Aliases: translate.corpus

### ** Examples

## Not run: 
##D translation <- translate(original, fr, de, key='insertkeyhere')
## End(Not run)



cleanEx()
nameEx("trimdfm")
### * trimdfm

flush(stderr()); flush(stdout())

### Name: trimdfm
### Title: Trim a dfm based on a subset of features and words
### Aliases: trimdfm

### ** Examples

data(inaugCorpus)
dtm <- dfm(inaugCorpus)
dim(dtm)
dtmReduced <- trimdfm(dtm, minCount=10, minDoc=2) # only words occuring at least 5 times and in at least 2documents
dim(dtmReduced)
dtmSampled <- trimdfm(dtm, sample=200)  # top 200 words
dim(dtmSampled)  # 196 x 200 words



cleanEx()
nameEx("twitterTerms")
### * twitterTerms

flush(stderr()); flush(stdout())

### Name: twitterTerms
### Title: make a corpus object from results of a twitter REST search
### Aliases: twitterTerms

### ** Examples

## Not run: 
##D twCorp <- twitterTerms('example', 10, key, cons_secret, token, access_secret)
## End(Not run)



cleanEx()
nameEx("uk2010immig")
### * uk2010immig

flush(stderr()); flush(stdout())

### Name: uk2010immig
### Title: Immigration-related sections of 2010 UK party manifestos
### Aliases: uk2010immig

### ** Examples

data(uk2010immig)
uk2010immigCorpus <- corpus(uk2010immig, docvars=list(party=names(uk2010immig)))
language(uk2010immigCorpus) <- "english"
encoding(uk2010immigCorpus) <- "UTF-8"
summary(uk2010immigCorpus)



cleanEx()
nameEx("wordcloud.dfm")
### * wordcloud.dfm

flush(stderr()); flush(stdout())

### Name: wordcloud.dfm
### Title: Plot a word cloud for a dfm
### Aliases: wordcloud.dfm

### ** Examples

data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
wfm <- dfm(iebudgets2010, stopwords=TRUE)
wordcloudDfm(wfm, 1)  # plot the finance minister's speech as a wordcloud



cleanEx()
nameEx("wordcloudDfm")
### * wordcloudDfm

flush(stderr()); flush(stdout())

### Name: wordcloudDfm
### Title: Plot a word cloud for a dfm
### Aliases: wordcloudDfm

### ** Examples

data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
wfm <- dfm(iebudgets2010, stopwords=TRUE)
wordcloudDfm(wfm, 1)  # plot the finance minister's speech as a wordcloud



cleanEx()
nameEx("wordfishMCMC")
### * wordfishMCMC

flush(stderr()); flush(stdout())

### Name: wordfishMCMC
### Title: Bayesian-MCMC version of the "wordfish" Poisson scaling model
### Aliases: wordfishMCMC

### ** Examples

## Not run: 
##D data(iebudgets)
##D # extract just the 2010 debates
##D iebudgets2010 <- corpus.subset(iebudgets, year==2010)
##D 
##D # create a document-term matrix and set the word margin to the columns
##D dtm <- create.fvm.corpus(iebudgets2010)
##D dtm <- wfm(t(dtm), word.margin=2)
##D 
##D # estimate the maximium likelihood wordfish model from austin
##D iebudgets2010_wordfish <- wordfish(dtm, dir=c(2,1))
##D 
##D # estimate the MCMC model, default values
##D iebudgets2010_wordfishMCMC <- wordfishMCMC(dtm, dir=c(2,1))
##D 
##D # compare the estimates of \eqn{\theta_i}
##D plot(iebudgets2010_wordfish$theta, iebudgets2010_wordfishMCMC$theta)
##D 
##D # MCMC with a partition of the word parameters according to govt and opposition
##D # (FF and Greens were in government in during the debate over the 2010 budget)
##D # set the constraint on word partitioned parameters to be the same for "the" and "and"
##D iebudgets2010_wordfishMCMC_govtopp <-
##D     wordfishMCMC(dtm, dir=c(2,1),
##D     wordPartition=(iebudgets2010$attribs$party=="FF" | iebudgets2010$attribs$party=="Green"),
##D     betaPartition=TRUE, wordConstraints=which(words(dtm)=="the"))
## End(Not run)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
