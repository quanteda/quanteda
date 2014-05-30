test_that("corpusCreate makes a corpus with a list of texts", {
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
  
  is_time <- function(x) equals(as.POSIXct(x, tz = "UTC"))
  floor_base <- function(unit) floor_date(base, unit)
  
  expect_that(length(amicusCorpus$attribs$texts), equals(100))
  expect_that(length(amicusTexts), equals(100))

})