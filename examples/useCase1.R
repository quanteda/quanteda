library(quanteda)


# from a path
d <- getDirectory("~/Dropbox/QUANTESS/corpora/ukManRenamed")
newCorp <- corpus(d)

# from a path with the attribs in the filenames
d <- getDirectory("~/Dropbox/QUANTESS/corpora/ukManRenamed" )
newCorp1 <- corpus(d, attribs="filenames", attNames=c('country','type','year','language', 'party'))
texts(newCorp1)<- iconv(texts(newCorp1), from="latin1", to="UTF-8")

# from a character vector
ukTexts <- getTextDir("~/Dropbox/QUANTESS/corpora/ukManRenamed")
newCorp2 <- corpus(ukTexts)
texts(newCorp2)<- iconv(texts(newCorp2), from="latin1", to="UTF-8")

# from GUI chooser
newCorp3 <- corpus()

# length
length(newCorp2)

testTexts <- rep("testing",length(ukTexts))
# accessor
y <- texts(newCorp)

# replacement
texts(newCorp) <- testTexts

#cleaning a string
single <- clean('This is a test. Testing! Punctuation. £100, €0.83, "for example". ')

#character vector
vec <- clean(texts(newCorp1))

# and a corpus (returns a corpus)
tempCorpus <- clean(newCorp1)

# dfms
tempd <- dfm(newCorp1)

# tokenize
single <- tokenize('This is a text.')
vec <- tokenize(texts(newCorp1))

#returns a corpus with tokens
newCorp1 <- tokenize(newCorp1)

#index
tempi <- index(newCorp1)

