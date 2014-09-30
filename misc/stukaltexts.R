## trial on Denis Stukal's Russian texts
##
## Ken Benoit 26-Sept-2014


rm(list=ls())
gc()

library(quanteda)

# read the texts into a corpus
stukalCorp <- corpus(directory("~/Dropbox/QUANTESS/corpora/pozhdata"), 
                     docvars="filenames")
size1 <- object.size(texts(stukalCorp))
    
# convert from Windows Cyrillic (aka Windows-1251)
texts(stukalCorp) <- iconv(texts(stukalCorp), from="WINDOWS-1251", to="UTF-8")
size2 <- object.size(texts(stukalCorp))


stukalCorp <- corpus(directory("~/Dropbox/QUANTESS/corpora/pozhdata"), 
                     docvars="filenames",
                     enc="WINDOWS-1251")
size3 <- object.size(texts(stukalCorp))

identical(size2, size3)
cat(as.numeric(size2 / size1), "times larger in UTF-8.\n")
## 1.821928 times larger in UTF-8.
# probably not 2x larger only because of spaces/punctuation which still need just 1 byte

# this is VERY slow (relatively)
system.time(tst <- tokenize(texts(stukalCorp)))
# compare to
system.time(tst <- tokenize(inaugTexts))

summary(stukalCorp, 20)
# view the first text to see if it looks ok - yes
texts(stukalCorp)[1]

system.time(mydfm <- dfm(stukalCorp)) # stopwords=stopwordsGet("russian"), stem=))
# most of that is from tokenize()

topfeatures(mydfm, 30)


s <- paste(texts(stukalCorp), collapse=" ")
nchar(s)

system.time(tokSS <- strsplit(s, " "), gcFirst = TRUE)
system.time(tokSc <- scan(what="char", text=s, quiet=TRUE, quote="", sep=" "), gcFirst = TRUE)
## scan remains faster (approx 2x)


system.time(sgs1 <- gsub("[[:punct:]]", "", s))


cleanSingleNew <- function(s, removeDigits=TRUE, removePunct=TRUE, lower=TRUE) {
    if (removePunct) {
        s <- gsub("[[:punct:]]", "", s)
    }
    # must remove "punctuation" first
    if (removeDigits) {
        s <- gsub("[[:digit:]]", "", s)
    } 
    if (lower) {
        s <- tolower(s)
    }
    # convert multiple whitespace (up to 100 in a row) into one
    ## s <- gsub(" {2,}", " ", s)
    # remove leading and trailing whitespace and return
    gsub("^ +| +$", "", s)
}


system.time(test <- cleanSingleNew(s, removeDigits=FALSE, lower=FALSE), gcFirst = TRUE)
system.time(test <- sapply(texts(stukalCorp), cleanSingleNew, removeDigits=FALSE, lower=FALSE), gcFirst = TRUE)

system.time(test <- cleanSingleNew(s, removePunct=FALSE, removeDigits=TRUE, lower=FALSE), gcFirst = TRUE)
system.time(test <- sapply(texts(stukalCorp), cleanSingleNew, removePunct=FALSE, removeDigits=TRUE, lower=FALSE), gcFirst = TRUE)

## the top two tests show that a) no bottleneck here and b) sapply is faster

system.time(test1 <- cleanSingleNew(s, removePunct=FALSE, removeDigits=FALSE, lower=TRUE), gcFirst = TRUE)
##    user  system elapsed 
## 106.511   0.115 106.876 
system.time(test2 <- sapply(texts(stukalCorp), cleanSingleNew, removePunct=FALSE, removeDigits=FALSE, lower=TRUE), gcFirst = TRUE)
##    user  system elapsed 
##   3.941   0.006   3.949  
system.time(test2a <- sapply(texts(stukalCorp), tolower), gcFirst = TRUE)
##    user  system elapsed 
##   3.912   0.002   3.917 




tolowerRussian <- function(s) {
    chartr("АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", 
           "абвгдеёжзийклмнопрстуфхцчшщъыьэюя", 
           s)
}


## COMPARE RUN TIMES
system.time(test3 <- tolower(s), gcFirst = TRUE)
##    user  system elapsed 
## 106.511   0.115 106.876
## MUCH SLOWER with the longer text set
##
## if tokenized completely
tkns <- tokenize(s, lower=FALSE)
system.time(test3a <- tolower(tkns), gcFirst = TRUE)
##    user  system elapsed 
##   2.850   0.005   2.861 
##############
## FASTEST ###
##############

system.time(test4 <- tolowerRussian(s), gcFirst = TRUE)
##    user  system elapsed 
##   3.941   0.006   3.949 
system.time(test5 <- sapply(texts(stukalCorp), tolowerRussian), gcFirst = TRUE)
##    user  system elapsed 
##   3.941   0.006   3.949 


## fuggedaboudit
require(Unicode)
system.time(test6 <- u_to_lower_case(s),  gcFirst = TRUE)
## user  system elapsed 
## 21.805   0.235  22.301 
