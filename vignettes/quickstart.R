## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, 
                      comment = "##")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("quanteda")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("quanteda/quanteda.corpora")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("kbenoit/quanteda.dictionaries")

## ---- message = FALSE----------------------------------------------------
library(quanteda)

## ------------------------------------------------------------------------
my_corpus <- corpus(data_char_ukimmig2010)  # build a new corpus from the texts
summary(my_corpus)

## ------------------------------------------------------------------------
docvars(my_corpus, "Party") <- names(data_char_ukimmig2010)
docvars(my_corpus, "Year") <- 2010
summary(my_corpus)

## ------------------------------------------------------------------------
metadoc(my_corpus, "language") <- "english"
metadoc(my_corpus, "docsource")  <- paste("data_char_ukimmig2010", 1:ndoc(my_corpus), sep = "_")
summary(my_corpus, showmeta = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  require(readtext)
#  
#  # Twitter json
#  mytf1 <- readtext("~/Dropbox/QUANTESS/social media/zombies/tweets.json")
#  my_corpusTwitter <- corpus(mytf1)
#  summary(my_corpusTwitter, 5)
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
#            file = "/tmp/inaug_texts.csv", row.names = FALSE)
#  mytf7 <- readtext("/tmp/inaug_texts.csv", textfield = "inaugSpeech")
#  summary(corpus(mytf7), 5)

## ------------------------------------------------------------------------
texts(data_corpus_inaugural)[2]

## ------------------------------------------------------------------------
summary(data_corpus_irishbudget2010)

## ---- fig.width = 8------------------------------------------------------
tokenInfo <- summary(data_corpus_inaugural)
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x = Year, y = Tokens, group = 1)) + geom_line() + geom_point() +
        scale_x_continuous(labels = c(seq(1789, 2017, 12)), breaks = seq(1789, 2017, 12)) +
    theme_bw()

# Longest inaugural address: William Henry Harrison
tokenInfo[which.max(tokenInfo$Tokens), ] 

## ------------------------------------------------------------------------
library(quanteda)
my_corpus1 <- corpus(data_corpus_inaugural[1:5])
my_corpus2 <- corpus(data_corpus_inaugural[53:58])
my_corpus3 <- my_corpus1 + my_corpus2
summary(my_corpus3)

## ------------------------------------------------------------------------
summary(corpus_subset(data_corpus_inaugural, Year > 1990))
summary(corpus_subset(data_corpus_inaugural, President == "Adams"))

## ---- tidy=TRUE----------------------------------------------------------
kwic(data_corpus_inaugural, "terror")

## ------------------------------------------------------------------------
kwic(data_corpus_inaugural, "terror", valuetype = "regex")

## ------------------------------------------------------------------------
kwic(data_corpus_inaugural, "communist*")

## ------------------------------------------------------------------------
# inspect the document-level variables
head(docvars(data_corpus_inaugural))

# inspect the corpus-level metadata
metacorpus(data_corpus_inaugural)

## ------------------------------------------------------------------------
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt)
tokens(txt, remove_numbers = TRUE,  remove_punct = TRUE)
tokens(txt, remove_numbers = FALSE, remove_punct = TRUE)
tokens(txt, remove_numbers = TRUE,  remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)

## ------------------------------------------------------------------------
tokens("Great website: http://textasdata.com?page=123.", what = "character")
tokens("Great website: http://textasdata.com?page=123.", what = "character", 
         remove_separators = FALSE)

## ------------------------------------------------------------------------
# sentence level         
tokens(c("Kurt Vongeut said; only assholes use semi-colons.", 
           "Today is Thursday in Canberra:  It is yesterday in London.", 
           "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"), 
          what = "sentence")

## ------------------------------------------------------------------------
my_corpus <- corpus_subset(data_corpus_inaugural, Year > 1990)

# make a dfm
my_dfm <- dfm(my_corpus)
my_dfm[, 1:5]

## ------------------------------------------------------------------------
# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(my_corpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
myStemMat[, 1:5]

## ------------------------------------------------------------------------
head(stopwords("english"), 20)
head(stopwords("russian"), 10)
head(stopwords("arabic"), 10)

## ----warning=FALSE, fig.width = 8, fig.height = 8------------------------
my_dfm <- dfm(data_char_ukimmig2010, remove = stopwords("english"), remove_punct = TRUE)
my_dfm

## ------------------------------------------------------------------------
topfeatures(my_dfm, 20)  # 20 top words

## ----warning=FALSE, fig.width = 7, fig.height = 7------------------------
set.seed(100)
textplot_wordcloud(my_dfm, min_count = 6, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

## ------------------------------------------------------------------------
by_party_dfm <- dfm(data_corpus_irishbudget2010, groups = "party", 
                  remove = stopwords("english"), remove_punct = TRUE)

## ------------------------------------------------------------------------
dfm_sort(by_party_dfm)[, 1:10]

## ------------------------------------------------------------------------
recent_corpus <- corpus_subset(data_corpus_inaugural, Year > 1991)

## ------------------------------------------------------------------------
my_dict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
                          economy = c("jobs", "business", "grow", "work")))

## ------------------------------------------------------------------------
by_pres_mat <- dfm(recent_corpus, dictionary = my_dict)
by_pres_mat

## ---- eval = FALSE-------------------------------------------------------
#  liwcdict <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic",
#                         format = "LIWC")
#  liwcdfm <- dfm(data_corpus_inaugural[52:58], dictionary = liwcdict)
#  liwcdfm[, 1:10]

## ----fig.width = 6-------------------------------------------------------
pres_dfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
               remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
obama_simil <- textstat_simil(pres_dfm, c("2009-Obama" , "2013-Obama"), 
                             margin = "documents", method = "cosine")
obama_simil
# dotchart(as.list(obamaSimil)$"2009-Obama", xlab = "Cosine similarity")

## ---- fig.width = 10, fig.height = 7, eval = FALSE-----------------------
#  data(data_corpus_sotu, package = "quanteda.corpora")
#  pres_dfm <- dfm(corpus_subset(data_corpus_sotu, Date > as.Date("1980-01-01")),
#                 stem = TRUE, remove_punct = TRUE,
#                 remove = stopwords("english"))
#  pres_dfm <- dfm_trim(pres_dfm, min_termfreq = 5, min_docfreq = 3)
#  
#  # hierarchical clustering - get distances on normalized dfm
#  pres_dist_mat <- textstat_dist(dfm_weight(pres_dfm, "prop"))
#  # hiarchical clustering the distance object
#  pres_cluster <- hclust(pres_dist_mat)
#  # label with document names
#  pres_cluster$labels <- docnames(pres_dfm)
#  # plot as a dendrogram
#  plot(pres_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

## ------------------------------------------------------------------------
sim <- textstat_simil(pres_dfm, c("fair", "health", "terror"), method = "cosine", margin = "features")
lapply(as.list(sim), head, 10)

## ------------------------------------------------------------------------
# make prettier document names
ie_dfm <- dfm(data_corpus_irishbudget2010)
textmodel_wordfish(ie_dfm, dir = c(2, 1))

## ------------------------------------------------------------------------
quant_dfm <- dfm(data_corpus_irishbudget2010, 
                remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("english"))
quant_dfm <- dfm_trim(quant_dfm, min_termfreq = 4, max_docfreq = 10)
quant_dfm

set.seed(100)
if (require(topicmodels)) {
    my_lda_fit20 <- LDA(convert(quant_dfm, to = "topicmodels"), k = 20)
    get_terms(my_lda_fit20, 5)
}

