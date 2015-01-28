library(quanteda)
tte <- inaugCorpus

df1 <- dfm(inaugTexts)
df2 <- dfms(inaugTexts)

df2m <- as.matrix(df2)

class(df2)

df2 <- as.dfm()


class(df2) <- c("dfm", class(df2))

trimdfm(df2)
textmodel(df2, df1)


# possibilities for url detection:
ur <- 'http://www.ttet.com'
nchar(gsub("[^.]", "", ur))

grep('http://' , ur)

grep("http://[^s]+",ur)


################################################
# html imports
################################################
## this way just stripts html tags
htm <- readLines('~/Desktop/1.html')
tmp <- (gsub("<.*?>", "", htm))
tmp <- tmp[tmp!=""]



# this way extracts based on the list tag and emdash separator

library(rvest)
testDoc <- html('/home/paul/Dropbox/QUANTESS/corpora/EP_2014_social_media/twitter/deu/Andreas_Schwab/1.html')
twts <- html_nodes(testDoc, "li")[42:133]
html_text(j)
atts <- c()
txts <- c()
for(tw in twts){
    this_txt <- (html_text(tw))
    parts <- unlist(strsplit(this_txt, "—"))
    print(parts[2])
    atts <- c(atts, parts[1])
    txts <- c(txts, parts[2])
}
testCorp <- corpus(txts, docvars=atts)

