library(quanteda)
#### Analyze Bollinger texts from Evans et al JELS 2007
# load in Amicus texts from a zipped web archive

# download and unzip texts
amicusFile <- "http://www.kenbenoit.net/courses/tcd2014qta/exercises/amicus_curiae.zip"
download.file(amicusFile, basename(amicusFile))
unzip(basename(amicusFile))

# load in the texts to a vector of texts using quanteda's getTextDir()
amicusTexts <- c(getTextDir("./amicus/training"), getTextDir("./amicus/testing"))
# change the encoding (because texts contain special symbols such as §)
amicusTexts <- iconv(amicusTexts, from="latin1", to="UTF-8")

# set training class
trainclass <- factor(c("P", "R", rep(NA, length(amicusTexts)-2)))

# set test class
testclass  <- rep(NA, length(amicusTexts))
testclass[grep("AP", names(amicusTexts))] <- "AP"
testclass[grep("AR", names(amicusTexts))] <- "AR"

# make a corpus object with texts and training and test labels
amicusCorpus <- 
  corpusCreate(amicusTexts, attribs=list(trainclass=trainclass, testclass=testclass))
summary(amicusCorpus)
# extract a word by document matrix
amicusDfm <- dfm(amicusCorpus, bigram=TRUE)
# train the NB classifier 
amicusNb <- naiveBayesText(as.matrix(amicusDfm), amicusCorpus$attribs$trainclass, smooth=1, prior="uniform")
# predict test class
amicusNbp <- predict(amicusNb)
# compare the predicted class (rows) versus the actual class (columns)
table(amicusNbp$docs$nb.predicted, amicusCorpus$attribs$testclass)