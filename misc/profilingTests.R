## Line-by-line profiling for general quanteda workflow, with most 
## common operations 
## using English and Cyrillic texts
##
## Paul Nulty 29th Sept 2014
profTests <- function(){
    library(quanteda)
    
    # opening and conversion
    stukalCorp <- corpus(directory("~/Dropbox/QUANTESS/corpora/pozhdata"), 
                         docvars="filenames")
    texts(stukalCorp) <- iconv(texts(stukalCorp), from="WINDOWS-1251", to="UTF-8")
    # opening and conversion
    iac <- corpus(directory("~/Dropbox/QUANTESS/corpora/inaugural"), 
                         docvars="filenames", sep="-")
    
    # cleaning alone
    stcl <- clean(texts(stukalCorp))
    incl <- clean(texts(iac))
    
    # tokenization (includes cleaning)
    stoks <- tokenize(texts(stukalCorp))
    intoks <- tokenize(texts(iac))
    
    #dfm
    stdfm <- dfm(stukalCorp)
    indfm <-dfm(iac)
}


cleanTest1 <- function(s){
    temp <- gsub("[[:punct:]]", '', s)
    temp <- gsub("[[:punct:]]", '', s, perl=TRUE)
    
    # fast but doesn't work for non-ascii
    temp <- gsub("[[:punct:]]", '', s, useBytes=TRUE)
    
    temp <- tolower(s)

}




