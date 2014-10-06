## Line-by-line profiling for general quanteda workflow, with most 
## common operations 
## using English and Cyrillic texts
##
## Paul Nulty 29th Sept 2014

library(lineprof)


# profiling of general workflow 
source('./misc/profilingTests.R')
res <- lineprof(profTests())
shine(res)



#of the clean function on English texts
iac <- corpus(directory("~/Dropbox/QUANTESS/corpora/inaugural"), 
              docvars="filenames", sep="-")
res1 <- lineprof( cleanTest1(texts(iac)))
shine(res1)

#of the clean function on Russian texts
stukalCorp <- corpus(directory("~/Dropbox/QUANTESS/corpora/pozhdata"), 
                     docvars="filenames")
texts(stukalCorp) <- iconv(texts(stukalCorp), from="WINDOWS-1251", to="UTF-8")

# does it actually do anything?
s <- texts(stukalCorp)
temp <- gsub("[[:punct:]]", '', s, useBytes=FALSE) #no
temp <- tolower(s) #yes

res1 <- lineprof( cleanTest1(texts(stukalCorp)))
shine(res1)
