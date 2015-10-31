
# a demonstration of the capabilities of quanteda

str(ukimmigTexts)
# create a corpus from immigration texts
immigCorpus <- corpus(ukimmigTexts, notes="Created as part of a demo.")
docvars(immigCorpus) <- data.frame(party = docnames(immigCorpus), year = 2010)
summary(immigCorpus)

kwic(immigCorpus, "deport", 3)
kwic(immigCorpus, "deport", 3, valuetype = "regex")
kwic(immigCorpus, "illegal immig*", window = 3)

sort(lexdiv(dfm(immigCorpus, verbose=FALSE), "TTR"))

mydfm <- dfm(immigCorpus, stem = TRUE, ignoredFeatures = stopwords("english"))
docnames(mydfm)
topfeatures(mydfm, 20)

# open a nicer quartz device on a mac
if (Sys.info()[1]=="Darwin") quartz("BNP 2010 word cloud", 7, 7)
# this is necessary currently because no subset is yet implemented for dfm objects
plot(mydfm["BNP",])

# some examples of tokenization and string cleaning

(toks <- tokenize(inaugCorpus[2], removePunct = TRUE))
wordstem(toks)
removeFeatures(toks, stopwords("english"))


## more demonstrations


# extract a document-feature matrix
immigDfm <- dfm(subset(immigCorpus, party=="BNP"))
plot(immigDfm)
immigDfm <- dfm(subset(immigCorpus, party=="BNP"), ignoredFeatures = stopwords("english"))
plot(immigDfm, random.color = TRUE, rot.per = .25, colors = sample(colors()[2:128], 5))

# change units to sentences
immigCorpusSent <- changeunits(immigCorpus, to = "sentences")
summary(immigCorpusSent, 20)


## tokenize some texts
txt <- "#TextAnalysis is MY <3 4U @myhandle gr8 #stuff :-)"
tokenize(txt, removePunct=TRUE)
tokenize(txt, removePunct=TRUE, removeTwitter=TRUE)
(toks <- tokenize(toLower(txt), removePunct=TRUE, removeTwitter=TRUE))
str(toks)

# tokenize sentences
(sents <- tokenize(ukimmigTexts[1], what = "sentence", simplify = TRUE)[1:5])
# tokenize characters
tokenize(ukimmigTexts[1], what = "character", simplify = TRUE)[1:100]


## some descriptive statistics

## create a document-feature matrix from the inaugural corpus
summary(inaugCorpus)
presDfm <- dfm(inaugCorpus)
presDfm
docnames(presDfm)
# concatenate by president name                 
presDfm <- dfm(inaugCorpus, groups="President")
presDfm
docnames(presDfm)

# need first to install quantedaData, using
# devtools::install_github("kbenoit/quantedaData")
## show some selection capabilities on Irish budget corpus
data(iebudgetsCorpus, package = "quantedaData")
summary(iebudgetsCorpus, 10)
ieFinMin <- subset(iebudgetsCorpus, number=="01" & debate == "BUDGET")
summary(ieFinMin)
dfmFM <- dfm(ieFinMin)
plot(2008:2012, lexdiv(dfmFM, "C"), xlab="Year", ylab="Herndan's C", type="b",
     main = "World's Crudest Lexical Diversity Plot")


# plot some readability statistics
data(SOTUCorpus, package = "quantedaData")
fk <- readability(SOTUCorpus, "Flesch.Kincaid")
year <- lubridate::year(docvars(SOTUCorpus, "Date"))
require(ggplot2)
partyColours <- c("blue", "blue", "black", "black", "red", "red")
p <- ggplot(data = docvars(SOTUCorpus), aes(x = year, y = fk)) + #, group = delivery)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    geom_smooth(alpha=0.2, linetype=1, color="grey70", method = "loess", span = .34) +
    xlab("") +
    ylab("Flesch-Kincaid") +
    geom_point(aes(colour = party)) +
    scale_colour_manual(values = partyColours) +
    geom_line(aes(), alpha=0.3, size = 1) +
    ggtitle("Text Complexity in State of the Union Addresses") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
quartz(height=7, width=12)
print(p)


## Presidential Inaugural Address Corpus
presDfm <- dfm(inaugCorpus, ignoredFeatures = stopwords("english"))
# compute some document similarities
similarity(presDfm, "1985-Reagan", n=5, margin="documents")
similarity(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents", method = "cosine")
similarity(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents", method = "Hellinger")
similarity(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents", method = "eJaccard")

# compute some term similarities
similarity(presDfm, c("fair", "health", "terror"), method="cosine")


## low-level NLP tasks

# form ngrams
txt <- "Hey @kenbenoit #textasdata: The quick, brown fox jumped over the lazy dog!"
(toks1 <- tokenize(toLower(txt), removePunct = TRUE))
tokenize(toLower(txt), removePunct = TRUE, ngrams = 2)
tokenize(toLower(txt), removePunct = TRUE, ngrams = c(1,3))

# low-level options exist too
ngrams(tokens, c(1, 3, 5))

# form "skip-grams"
tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."),
                   removePunct = TRUE, simplify = TRUE)
ngrams(tokens, n = 2, skip = 1, concatenator = " ")
ngrams(tokens, n = 3, skip = 0:1, concatenator = " ")

# mine bigrams
collocs2 <- collocations(inaugTexts, size = 2, method = "all")
head(collocs2, 20)

# mine trigrams
collocs3 <- collocations(inaugTexts, size = 3, method = "all")
head(collocs3, 20)

# remove parts of speech and inspect
head(removeFeatures(collocs2, stopwords("english")), 20)
head(removeFeatures(collocs3, stopwords("english")), 20)
