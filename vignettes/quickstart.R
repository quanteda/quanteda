## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "##")

## ----show=FALSE----------------------------------------------------------
require(quanteda)

## ------------------------------------------------------------------------
str(data_char_inaugural)  # this gives us some information about the object
myCorpus <- corpus(data_char_inaugural)  # build the corpus
summary(myCorpus, n = 5)

## ------------------------------------------------------------------------
docvars(myCorpus, "President") <- substring(names(data_char_inaugural), 6)
docvars(myCorpus, "Year") <- as.integer(substring(names(data_char_inaugural), 1, 4))
summary(myCorpus, n=5)

## ------------------------------------------------------------------------
metadoc(myCorpus, "language") <- "english"
metadoc(myCorpus, "docsource")  <- paste("data_char_inaugural", 1:ndoc(myCorpus), sep = "_")
summary(myCorpus, n = 5, showmeta = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  require(readtext)
#  
#  # Twitter json
#  mytf1 <- readtext("~/Dropbox/QUANTESS/social media/zombies/tweets.json")
#  myCorpusTwitter <- corpus(mytf1)
#  summary(myCorpusTwitter, 5)
#  # generic json - needs a textfield specifier
#  mytf2 <- readtext("~/Dropbox/QUANTESS/Manuscripts/collocations/Corpora/sotu/sotu.json",
#                    textfield = "text")
#  summary(corpus(mytf2), 5)
#  # text file
#  mytf3 <- readtext("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt", cache = FALSE)
#  summary(corpus(mytf3), 5)
#  # multiple text files
#  mytf4 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", cache = FALSE)
#  summary(corpus(mytf4), 5)
#  # multiple text files with docvars from filenames
#  mytf5 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt",
#                    docvarsfrom = "filenames", sep = "-", docvarnames = c("Year", "President"))
#  summary(corpus(mytf5), 5)
#  # XML data
#  mytf6 <- readtext("~/Dropbox/QUANTESS/quanteda_working_files/xmlData/plant_catalog.xml",
#                    textfield = "COMMON")
#  summary(corpus(mytf6), 5)
#  # csv file
#  write.csv(data.frame(inaugSpeech = texts(data_corpus_inaugural),
#                       docvars(data_corpus_inaugural)),
#            file = "/tmp/inaug_texts.csv", row.names = FALSE)b
#  mytf7 <- readtext("/tmp/inaug_texts.csv", textfield = "inaugSpeech")
#  summary(corpus(mytf7), 5)

## ------------------------------------------------------------------------
texts(data_corpus_inaugural)[2]

## ------------------------------------------------------------------------
summary(data_corpus_irishbudget2010)

## ---- fig.width = 8------------------------------------------------------
tokenInfo <- summary(data_corpus_inaugural)
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x=Year, y=Tokens, group=1)) + geom_line() + geom_point() +
        scale_x_discrete(labels=c(seq(1789,2012,12)), breaks=seq(1789,2012,12) ) 

# Longest inaugural address: William Henry Harrison
tokenInfo[which.max(tokenInfo$Tokens),] 

## ------------------------------------------------------------------------
library(quanteda)
mycorpus1 <- corpus(data_char_inaugural[1:5], note = "First five inaug speeches.")
mycorpus2 <- corpus(data_char_inaugural[53:57], note = "Last five inaug speeches.")
mycorpus3 <- mycorpus1 + mycorpus2
summary(mycorpus3)

## ------------------------------------------------------------------------
summary(corpus_subset(data_corpus_inaugural, Year > 1990))
summary(corpus_subset(data_corpus_inaugural, President == "Adams"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
kwic(data_corpus_inaugural, "terror")
kwic(data_corpus_inaugural, "terror", valuetype = "regex")
kwic(data_corpus_inaugural, "communist*")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# inspect the document-level variables
head(docvars(data_corpus_inaugural))

# inspect the corpus-level metadata
metacorpus(data_corpus_inaugural)

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
myCorpus <- corpus_subset(data_corpus_inaugural, Year > 1990)

# make a dfm
myDfm <- dfm(myCorpus)
myDfm[, 1:5]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
myStemMat[, 1:5]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(stopwords("english"), 20)
head(stopwords("russian"), 10)
head(stopwords("arabic"), 10)

## ----warning=FALSE, fig.width = 8, fig.height = 8-----------------------------------------------------------------------------------------------------------------------------------------------------
mydfm <- dfm(data_char_ukimmig2010, remove = c("will", stopwords("english")), 
             removePunct = TRUE)
mydfm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
topfeatures(mydfm, 20)  # 20 top words

## ----warning = FALSE, fig.width = 7, fig.height = 7---------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(20)
textplot_wordcloud(dfm_trim(mydfm, min_count = 6))

## ----warning=FALSE, fig.width = 7, fig.height = 7-----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
textplot_wordcloud(mydfm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPartyDfm <- dfm(data_corpus_irishbudget2010, groups = "party", remove = stopwords("english"), removePunct = TRUE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sort(byPartyDfm)[, 1:10]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
recentCorpus <- corpus_subset(data_corpus_inaugural, Year > 1991)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myDict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
                          economy = c("jobs", "business", "grow", "work")))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPresMat <- dfm(recentCorpus, dictionary = myDict)
byPresMat

## ---- eval = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  liwcdict <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic",
#                         format = "LIWC")
#  liwcdfm <- dfm(data_char_inaugural[52:57], dictionary = liwcdict, verbose = FALSE)
#  liwcdfm[, 1:10]

## ----fig.width = 6------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year>1980), 
               remove = stopwords("english"),
               stem = TRUE, removePunct = TRUE)
obamaSimil <- textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), n = NULL, 
                             margin = "documents", method = "cosine")
dotchart(as.list(obamaSimil)$"2009-Obama", xlab = "Cosine similarity")

## ---- fig.width = 10, fig.height = 7, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
#  data(SOTUCorpus, package="quantedaData")
#  presDfm <- dfm(corpus_subset(SOTUCorpus, Date > as.Date("1960-01-01")), verbose = FALSE, stem = TRUE,
#                 remove = stopwords("english"), removePunct = TRUE)
#  presDfm <- dfm_trim(presDfm, min_count=5, min_docfreq=3)
#  # hierarchical clustering - get distances on normalized dfm
#  presDistMat <- dist(as.matrix(weight(presDfm, "relFreq")))
#  # hiarchical clustering the distance object
#  presCluster <- hclust(presDistMat)
#  # label with document names
#  presCluster$labels <- docnames(presDfm)
#  # plot as a dendrogram
#  plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sim <- similarity(presDfm, c("fair", "health", "terror"), method = "cosine", margin = "features", n = 10)
print(sim, digits = 2)

## ----fig.width = 6, fig.height = 6--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make prettier document names
docnames(data_corpus_irishbudget2010) <- 
    paste(docvars(data_corpus_irishbudget2010, "name"), docvars(data_corpus_irishbudget2010, "party"))
ieDfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE)
wf <- textmodel(ieDfm, model = "wordfish", dir=c(2,1))
wca <- textmodel(ieDfm, model = "ca")
# plot the results
plot(wf@theta, -1*wca$rowcoord[,1], 
     xlab="Wordfish theta-hat", ylab="CA dim 1 coordinate", pch=19)
text(wf@theta, -1*wca$rowcoord[,1], docnames(ieDfm), cex=.8, pos=1)
abline(lm(-1*wca$rowcoord[,1] ~ wf@theta), col="grey50", lty="dotted")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quantdfm <- dfm(data_corpus_irishbudget2010, verbose = FALSE, 
                remove = c("will", stopwords("english")))

if (require(topicmodels)) {
    myLDAfit20 <- LDA(convert(quantdfm, to = "topicmodels"), k = 20)
    get_terms(myLDAfit20, 5)
    topics(myLDAfit20, 3)
}

