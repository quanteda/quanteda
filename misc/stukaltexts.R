## trial on Denis Stukal's Russian texts
##
## Ken Benoit 26-Sept-2014

library(quanteda)

# read the texts into a corpus
stukalCorp <- corpus(directory("~/Dropbox/QUANTESS/corpora/pozhdata"), docvars="filenames")
size1 <- object.size(texts(stukalCorp))
    
# convert from Windows Cyrillic (aka Windows-1251)
texts(stukalCorp) <- iconv(texts(stukalCorp), from="WINDOWS-1251", to="UTF-8")
size2 <- object.size(texts(stukalCorp))

cat(as.numeric(size2 / size1), "times larger in UTF-8.\n")
## 1.821928 times larger in UTF-8.
# probably not 2x larger only because of spaces/punctuation which still need just 1 byte

# this is VERY slow (relatively)
tst <- tokenize(texts(stukalCorp))
# compare to
tst <- tokenize(inaugTexts)

summary(stukalCorp, 20)
# view the first text to see if it looks ok - yes
texts(stukalCorp)[1]

mydfm <- dfm(stukalCorp, stopwords=stopwordsGet("russian"), stem=)

topfeatures(mydfm, 30)

