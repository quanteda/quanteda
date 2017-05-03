
# a demonstration of the capabilities of quanteda 

str(data_char_ukimmig2010)
# create a corpus from immigration texts
immigCorpus <- corpus(data_char_ukimmig2010, metacorpus = list(notes = "Created as part of a demo."))
docvars(immigCorpus) <- data.frame(party = docnames(immigCorpus), year = 2010)
summary(immigCorpus)

kwic(immigCorpus, "deport", 3)
kwic(immigCorpus, "deport", 3, valuetype = "regex")
kwic(immigCorpus, "illegal immig*", window = 3)

sort(textstat_lexdiv(dfm(immigCorpus, verbose=FALSE), "TTR"))

mydfm <- dfm(immigCorpus, stem = TRUE, remove = stopwords("english"))
docnames(mydfm)
topfeatures(mydfm, 20)

# open a nicer quartz device on a mac
if (Sys.info()[1]=="Darwin") quartz("BNP 2010 word cloud", 7, 7)
# this is necessary currently because no subset is yet implemented for dfm objects
textplot_wordcloud(mydfm["BNP",])

# some examples of tokenization and string cleaning

(toks <- tokens(inaugCorpus[2], remove_punct = TRUE))
tokens_wordstem(toks)
tokens_remove(toks, stopwords("english"))


## more demonstrations


# extract a document-feature matrix
immigDfm <- dfm(subset(immigCorpus, party=="BNP"))
textplot_wordcloud(immigDfm)
immigDfm <- dfm(subset(immigCorpus, party=="BNP"), remove = stopwords("english"))
textplot_wordcloud(immigDfm, random.color = TRUE, rot.per = .25, colors = sample(colors()[2:128], 5))

# change units to sentences
immigCorpusSent <- changeunits(immigCorpus, to = "sentences")
summary(immigCorpusSent, 20)


## tokenize some texts
txt <- "#TextAnalysis is MY <3 4U @myhandle gr8 #stuff :-)"
tokens(txt, remove_punct=TRUE)
tokens(txt, remove_punct=TRUE, remove_twitter=TRUE)
(toks <- tokens(char_tolower(txt), remove_punct=TRUE, remove_twitter=TRUE))
str(toks)

# tokenize sentences
(sents <- tokens(data_char_ukimmig2010[1], what = "sentence", simplify = TRUE)[1:5])
# tokenize characters
tokens(data_char_ukimmig2010[1], what = "character", simplify = TRUE)[1:100]


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
plot(2008:2012, textstat_lexdiv(dfmFM, "C"), xlab="Year", ylab="Herndan's C", type="b",
     main = "World's Crudest Lexical Diversity Plot")


# plot some readability statistics
data(SOTUCorpus, package = "quantedaData")
fk <- textstat_readability(SOTUCorpus, "Flesch.Kincaid")
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
presDfm <- dfm(inaugCorpus, remove = stopwords("english"))
# compute some document similarities
as.list(textstat_simil(presDfm, "1985-Reagan", n=5, margin="documents"))
as.list(textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents", method = "cosine"))
as.list(textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents", method = "Hellinger"))
as.list(textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents", method = "eJaccard"))

# compute some term similarities
as.list(textstat_dist(presDfm, c("fair", "health", "terror"), margin = "features", method="cosine"))


## low-level NLP tasks

# form ngrams
txt <- "Hey @kenbenoit #textasdata: The quick, brown fox jumped over the lazy dog!"
tokens(char_tolower(txt), remove_punct = TRUE, ngrams = 2)
tokens(char_tolower(txt), remove_punct = TRUE, ngrams = c(1,3))

# low-level options exist too
(toks1 <- tokens(char_tolower(txt), remove_punct = TRUE))
tokens_ngrams(toks1, n = c(1, 3, 5))

# form "skip-grams"
toks2 <- tokens(char_tolower("Insurgents killed in ongoing fighting"))
tokens_ngrams(toks2, n = 2, skip = 1, concatenator = " ")
tokens_ngrams(toks2, n = 3, skip = 0:1, concatenator = " ")

# mine bigrams
collocs2 <- collocations(data_corpus_inaugural, size = 2, method = "all")
head(collocs2, 20)

# mine trigrams
collocs3 <- collocations(data_corpus_inaugural, size = 3, method = "all")
head(collocs3, 20)

# remove parts of speech and inspect
head(removeFeatures(collocs2, stopwords("english")), 20)
head(removeFeatures(collocs3, stopwords("english")), 20)
