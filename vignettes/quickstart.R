## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "#>")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("quanteda")

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("kbenoit/quanteda")

## ----eval=FALSE----------------------------------------------------------
#  ## devtools required to install quanteda from Github
#  devtools::install_github("kbenoit/quantedaData")

## ----show=FALSE----------------------------------------------------------
require(quanteda)

## ------------------------------------------------------------------------
str(inaugTexts)  # this gives us some information about the object
myCorpus <- corpus(inaugTexts)  # build the corpus
summary(myCorpus, n = 5)

## ------------------------------------------------------------------------
docvars(myCorpus, "President") <- substring(names(inaugTexts), 6)
docvars(myCorpus, "Year") <- as.integer(substring(names(inaugTexts), 1, 4))
summary(myCorpus, n=5)

## ------------------------------------------------------------------------
metadoc(myCorpus, "language") <- "english"
metadoc(myCorpus, "docsource")  <- paste("inaugTexts", 1:ndoc(myCorpus), sep = "_")
summary(myCorpus, n = 5, showmeta = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  # Twitter json
#  mytf1 <- textfile("~/Dropbox/QUANTESS/social media/zombies/tweets.json")
#  myCorpusTwitter <- corpus(mytf1)
#  summary(myCorpusTwitter, 5)
#  # generic json - needs a textField specifier
#  mytf2 <- textfile("~/Dropbox/QUANTESS/Manuscripts/collocations/Corpora/sotu/sotu.json",
#                    textField = "text")
#  summary(corpus(mytf2), 5)
#  # text file
#  mytf3 <- textfile("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt", cache = FALSE)
#  summary(corpus(mytf3), 5)
#  # multiple text files
#  mytf4 <- textfile("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", cache = FALSE)
#  summary(corpus(mytf4), 5)
#  # multiple text files with docvars from filenames
#  mytf5 <- textfile("~/Dropbox/QUANTESS/corpora/inaugural/*.txt",
#                    docvarsfrom="filenames", sep="-", docvarnames=c("Year", "President"))
#  summary(corpus(mytf5), 5)
#  # XML data
#  mytf6 <- textfile("~/Dropbox/QUANTESS/quanteda_working_files/xmlData/plant_catalog.xml",
#                    textField = "COMMON")
#  summary(corpus(mytf6), 5)
#  # csv file
#  write.csv(data.frame(inaugSpeech = texts(inaugCorpus), docvars(inaugCorpus)),
#            file = "/tmp/inaugTexts.csv", row.names = FALSE)
#  mytf7 <- textfile("/tmp/inaugTexts.csv", textField = "inaugSpeech")
#  summary(corpus(mytf7), 5)

## ------------------------------------------------------------------------
texts(inaugCorpus)[2]

## ------------------------------------------------------------------------
summary(ie2010Corpus)

## ---- fig.width = 8------------------------------------------------------
tokenInfo <- summary(inaugCorpus)
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x=Year, y=Tokens, group=1)) + geom_line() + geom_point() +
        scale_x_discrete(labels=c(seq(1789,2012,12)), breaks=seq(1789,2012,12) ) 

# Longest inaugural address: William Henry Harrison
tokenInfo[which.max(tokenInfo$Tokens),] 

## ------------------------------------------------------------------------
library(quanteda)
mycorpus1 <- corpus(inaugTexts[1:5], note = "First five inaug speeches.")
mycorpus2 <- corpus(inaugTexts[53:57], note = "Last five inaug speeches.")
mycorpus3 <- mycorpus1 + mycorpus2
summary(mycorpus3)

## ------------------------------------------------------------------------
summary(subset(inaugCorpus, Year > 1990))
summary(subset(inaugCorpus, President == "Adams"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
kwic(inaugCorpus, "terror")
kwic(inaugCorpus, "terror", valuetype = "regex")
kwic(inaugCorpus, "communist*")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# inspect the document-level variables
head(docvars(inaugCorpus))

# inspect the corpus-level metadata
metacorpus(inaugCorpus)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokenize(txt)
tokenize(txt, removeNumbers = TRUE, removePunct = TRUE)
tokenize(txt, removeNumbers = FALSE, removePunct = TRUE)
tokenize(txt, removeNumbers = TRUE, removePunct = FALSE)
tokenize(txt, removeNumbers = FALSE, removePunct = FALSE)
tokenize(txt, removeNumbers = FALSE, removePunct = FALSE, removeSeparators = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tokenize("Great website: http://textasdata.com?page=123.", what = "character")
tokenize("Great website: http://textasdata.com?page=123.", what = "character", 
         removeSeparators = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# sentence level         
tokenize(c("Kurt Vongeut said; only assholes use semi-colons.", 
           "Today is Thursday in Canberra:  It is yesterday in London.", 
           "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"), 
          what = "sentence")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myCorpus <- subset(inaugCorpus, Year > 1990)

# make a dfm
myDfm <- dfm(myCorpus)
myDfm[, 1:5]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(myCorpus, ignoredFeatures = stopwords("english"), stem = TRUE)
myStemMat[, 1:5]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(stopwords("english"), 20)
head(stopwords("russian"), 10)
head(stopwords("arabic"), 10)

## ----warning=FALSE, fig.width = 8, fig.height = 8-----------------------------------------------------------------------------------------------------------------------------------------------------
mydfm <- dfm(ukimmigTexts, ignoredFeatures = c("will", stopwords("english")))
mydfm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
topfeatures(mydfm, 20)  # 20 top words

## ----warning = FALSE, fig.width = 8, fig.height = 8---------------------------------------------------------------------------------------------------------------------------------------------------
plot(mydfm)

## ----warning=FALSE, fig.width = 7, fig.height = 7-----------------------------------------------------------------------------------------------------------------------------------------------------
if (require(RColorBrewer))
    plot(mydfm, max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPartyDfm <- dfm(ie2010Corpus, groups = "party", ignoredFeatures = stopwords("english"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sort(byPartyDfm)[, 1:10]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
recentCorpus <- subset(inaugCorpus, Year > 1991)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myDict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
                          economy = c("jobs", "business", "grow", "work")))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPresMat <- dfm(recentCorpus, dictionary = myDict)
byPresMat

## ---- eval = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  liwcdict <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic",
#                         format = "LIWC")
#  liwcdfm <- dfm(inaugTexts[52:57], dictionary = liwcdict, verbose = FALSE)
#  liwcdfm[, 1:10]

## ----fig.width = 6------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
presDfm <- dfm(subset(inaugCorpus, Year>1980), 
               ignoredFeatures = stopwords("english"),
               stem=TRUE, verbose=FALSE)
obamaSimil <- similarity(presDfm, c("2009-Obama" , "2013-Obama"), n = NULL, 
                            margin = "documents", method = "cosine", normalize = FALSE)
dotchart(obamaSimil$`2009-Obama`, xlab = "Cosine similarity")

## ---- fig.width = 10, fig.height = 7, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
#  data(SOTUCorpus, package="quantedaData")
#  presDfm <- dfm(subset(SOTUCorpus, Date > as.Date("1960-01-01")), verbose = FALSE, stem = TRUE,
#                 ignoredFeatures = stopwords("english"))
#  presDfm <- trim(presDfm, minCount=5, minDoc=3)
#  # hierarchical clustering - get distances on normalized dfm
#  presDistMat <- dist(as.matrix(weight(presDfm, "relFreq")))
#  # hiarchical clustering the distance object
#  presCluster <- hclust(presDistMat)
#  # label with document names
#  presCluster$labels <- docnames(presDfm)
#  # plot as a dendrogram
#  plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
similarity(presDfm, c("fair", "health", "terror"), method = "cosine", margin = "features", n = 20)

## ----fig.width = 6, fig.height = 6--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make prettier document names
docnames(ie2010Corpus) <- 
    paste(docvars(ie2010Corpus, "name"), docvars(ie2010Corpus, "party"))
ieDfm <- dfm(ie2010Corpus, verbose = FALSE)
wf <- textmodel(ieDfm, model = "wordfish", dir=c(2,1))
wca <- textmodel(ieDfm, model = "ca")
# plot the results
plot(wf@theta, -1*wca$rowcoord[,1], 
     xlab="Wordfish theta-hat", ylab="CA dim 1 coordinate", pch=19)
text(wf@theta, -1*wca$rowcoord[,1], docnames(ieDfm), cex=.8, pos=1)
abline(lm(-1*wca$rowcoord[,1] ~ wf@theta), col="grey50", lty="dotted")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quantdfm <- dfm(ie2010Corpus, verbose = FALSE, 
                ignoredFeatures = c("will", stopwords("english")))

if (require(topicmodels)) {
    myLDAfit20 <- LDA(convert(quantdfm, to = "topicmodels"), k = 20)
    get_terms(myLDAfit20, 5)
    topics(myLDAfit20, 3)
}

