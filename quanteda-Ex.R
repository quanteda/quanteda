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
### Title: Perform basic cleanup on a character object
### Aliases: clean

### ** Examples

## Not run: 
##D s <- "A cursed £$&^!€ Exclamation! point; paragraph 1.2, which I wrote."
##D clean(s)
## End(Not run)



cleanEx()
nameEx("collocations")
### * collocations

flush(stderr()); flush(stdout())

### Name: collocations
### Title: Detect collocations in a text
### Aliases: collocations

### ** Examples

data(iebudgets)
collocations(iebudgets$attribs$texts[1], top=50)
collocations(iebudgets$attribs$texts[1], top=50, method="chi2")



cleanEx()
nameEx("corpusAppend")
### * corpusAppend

flush(stderr()); flush(stdout())

### Name: corpusAppend
### Title: function to add new texts and attributes to an existing corpus
###   Accepts a list of texts and a list of associated attributes and adds
###   them to the corpus
### Aliases: corpusAppend

### ** Examples

data(iebudgets)
data(ieAttribs)
data(ieTexts)
budgets <- corpusAppend(iebudgets, ieTexts, ieAttribs)



cleanEx()
nameEx("corpusCreate")
### * corpusCreate

flush(stderr()); flush(stdout())

### Name: corpusCreate
### Title: Create a new corpus This function creates a corpus from a
###   character vector (of texts), adds text-specific variables (which we
###   term "attributes"), along with optional meta-data and notes.
### Aliases: corpusCreate

### ** Examples

data(ieTexts)
data(ieAttribs)
budgets <- corpusCreate(ieTexts, attribs=ieAttribs)
summary(budgets)



cleanEx()
nameEx("corpusFromFilenames")
### * corpusFromFilenames

flush(stderr()); flush(stdout())

### Name: corpusFromFilenames
### Title: create a new corpus with attribute-value pairs taken from
###   filenames
### Aliases: corpusFromFilenames

### ** Examples

## Not run: 
##D new_corpus <- corpusFromFilenames(dirname, c("country", "electionType", "year", "language", "party"), sep='_')
## End(Not run)



cleanEx()
nameEx("corpusFromHeaders")
### * corpusFromHeaders

flush(stderr()); flush(stdout())

### Name: corpusFromHeaders
### Title: create a new corpus with attribute-value pairs taken from
###   document headers
### Aliases: corpusFromHeaders

### ** Examples

data(ieTextsHeaders)
budgets <- corpusFromHeaders(ieTextsHeaders)



cleanEx()
nameEx("corpusReshape")
### * corpusReshape

flush(stderr()); flush(stdout())

### Name: corpusReshape
### Title: Transform a corpus by splitting texts into sentences
### Aliases: corpusReshape

### ** Examples

data(iebudgets)
ie2010document <- subset(iebudgets, year==2010)
summary(ie2010document)
ie2010sentence <- corpusReshape(ie2010document)  # reshape to sentence units
summary(ie2010sentence, 20)



cleanEx()
nameEx("corpusSample")
### * corpusSample

flush(stderr()); flush(stdout())

### Name: corpusSample
### Title: Corpus sampling
### Aliases: corpusSample

### ** Examples

data(movies)
movieSamp <- sample(movies, 200, replace=TRUE)



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

texts <- c("testing this text", "and this one")
describeTexts(texts)



cleanEx()
nameEx("dfm")
### * dfm

flush(stderr()); flush(stdout())

### Name: dfm
### Title: Create a document-feature matrix from a corpus object
### Aliases: dfm dfm.character dfm.corpus

### ** Examples

data(iebudgets)
wfm <- dfm(iebudgets)

## by party, subset for 2010
wfmByParty2010 <- dfm(subset(iebudgets, year==2010), groups="party")

## with dictionaries
corpus <- subset(iebudgets, year==2010)
mydict <- list(christmas=c("Christmas", "Santa", "holiday"),
               opposition=c("Opposition", "reject", "notincorpus"),
               taxing="taxing",
               taxation="taxation",
               taxregex="tax*")
dictDfm <- dfm(corpus, dictionary=mydict)
dictDfm

## removing stopwords
testText <- "The quick brown fox named Séamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."#
testCorpus <- corpusCreate(testText)
dfm(testCorpus, stopwords=TRUE)
if (require(tm)) {
}

## adding one dfm to another
mydict2 <- list(partyref=c("Lenihan", "Fianna", "Sinn", "Gael"))
dictDfm2 <- dfm(corpus, dictionary=mydict2, addto=dictDfm)
dictDfm2



cleanEx()
nameEx("dfm2ldaformat")
### * dfm2ldaformat

flush(stderr()); flush(stdout())

### Name: dfm2ldaformat
### Title: Convert a quanteda 'dfm' (document feature matrix) into a the
###   data format needed by lda
### Aliases: dfm2ldaformat

### ** Examples

data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
# create document-feature matrix, remove stopwords
d <- dfm(iebudgets2010, stopwords=TRUE)
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

data(iebudgets)
iebudgets2010 <- subset(iebudgets, year==2010)
d <- dfmTrim(dfm(iebudgets2010), minCount=5, minDoc=3)
dim(d)
td <- dfm2tmformat(d)
length(td$v)
if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)



cleanEx()
nameEx("dfmSample")
### * dfmSample

flush(stderr()); flush(stdout())

### Name: dfmSample
### Title: Corpus sampling
### Aliases: dfmSample

### ** Examples

data(movies)
d <- dfm(movies)
samp <- dfmSample(d, 100, replace=TRUE)



cleanEx()
nameEx("dfmSort")
### * dfmSort

flush(stderr()); flush(stdout())

### Name: dfmSort
### Title: sort a dfm by one or more margins
### Aliases: dfmSort

### ** Examples

data(iebudgets)
dtm <- dfm(iebudgets)
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

data(iebudgets)
dtm <- dfm(iebudgets)
dim(dtm)  # 196 docs x 13343 words
dtmReduced <- dfmTrim(dtm, minCount=10, minDoc=3) # only words occuring at least 10 times and in at least 3 documents
dim(dtmReduced)  # 196 docs x 3006 words
dtmSampled <- dfmTrim(dtm, sample=200)  # top 200 words
dim(dtmSampled)  # 196 x 200 words



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
nameEx("getData")
### * getData

flush(stderr()); flush(stdout())

### Name: getData
### Title: extract the attributes (document-level meta-data) from a corpus
### Aliases: getData

### ** Examples

data(iebudgets)
getData(subset(iebudgets, year==2012))
getData(subset(iebudgets, year==2012), usenames=FALSE)



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
nameEx("getTexts")
### * getTexts

flush(stderr()); flush(stdout())

### Name: getTexts
### Title: extract the texts from a corpus
### Aliases: getTexts

### ** Examples

data(iebudgets)
cowenTexts <- getTexts(subset(iebudgets, speaker="Cowen"))
syllableCount(cowenTexts)



cleanEx()
nameEx("inaugCorpus")
### * inaugCorpus

flush(stderr()); flush(stdout())

### Name: inaugCorpus
### Title: A corpus of US inaugural addresses from 1789-2013
### Aliases: inaugCorpus

### ** Examples

data(inaugCorpus)
summary(inaugCorpus)



cleanEx()
nameEx("kwic")
### * kwic

flush(stderr()); flush(stdout())

### Name: kwic
### Title: List key words in context from a text or a corpus of texts.
### Aliases: kwic kwic.character kwic.corpus

### ** Examples

data(ieTexts)  # a text vector
kwic(ieTexts, "tax")
kwic(ieTexts, "tax", regex=FALSE)  # returns only whole word, without trailing punctuation
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
nameEx("lbg")
### * lbg

flush(stderr()); flush(stdout())

### Name: lbg
### Title: Example data from Laver Benoit and Garry (2003)
### Aliases: lbg

### ** Examples

data(inaugCorpus)
summary(inaugCorpus)



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
##D data(iebudgets)
##D iebudgets2010 <- subset(iebudgets, year==2010)
##D summary(iebudgets2010)
##D iebudgetsLenihan <- subset(iebudgets, speaker="Lenihan", select=c(speaker, year))
##D summary(iebudgetsLenihan)
## End(Not run)



cleanEx()
nameEx("summary.corpus")
### * summary.corpus

flush(stderr()); flush(stdout())

### Name: summary.corpus
### Title: Corpus summary
### Aliases: summary.corpus

### ** Examples

data(iebudgets)
summary(iebudgets, subset=(year==2010))
summary(iebudgets, nmax=10)



cleanEx()
nameEx("sylCounts")
### * sylCounts

flush(stderr()); flush(stdout())

### Name: sylCounts
### Title: A named list mapping words to counts of their syllables
### Aliases: sylCounts

### ** Examples

data(sylCounts)
syllableCounts["sixths"]
syllableCounts["onomatopeia"]



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
nameEx("tf")
### * tf

flush(stderr()); flush(stdout())

### Name: tf
### Title: normalizes the term frequencies a dfm
### Aliases: tf

### ** Examples

data(iebudgets)
dtm <- dfm(iebudgets)
dtm[1:10, 100:110]
tf(dtm)[1:10, 100:110]



cleanEx()
nameEx("tfidf")
### * tfidf

flush(stderr()); flush(stdout())

### Name: tfidf
### Title: compute the tf-idf weights of a dfm
### Aliases: tfidf

### ** Examples

data(iebudgets)
dtm <- dfm(iebudgets)
dtm[1:10, 100:110]
tfidf(dtm)[1:10, 100:110]
tfidf(dtm, normalize=FALSE)[1:10, 100:110]



cleanEx()
nameEx("tokenize")
### * tokenize

flush(stderr()); flush(stdout())

### Name: tokenize
### Title: Split a string into words The input text is split into words by
###   whitespace
### Aliases: tokenize

### ** Examples

testtxt <- "The quick brown fox named Séamus jumps over the lazy dog Rory, with Tom's newpaper in his mouth."
tokenize(testtxt)
tokenize(testtxt, lower=FALSE)



cleanEx()
nameEx("topFeatures")
### * topFeatures

flush(stderr()); flush(stdout())

### Name: topFeatures
### Title: list the top n features in a dfm
### Aliases: topFeatures

### ** Examples

data(iebudgets)
dtm <- dfm(iebudgets)
topFeatures(dtm)
topFeatures(dfm(iebudgets, stopwords=TRUE))
topFeatures(dtm, 50, normalize=TRUE)



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
