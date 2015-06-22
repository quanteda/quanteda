## ------------------------------------------------------------------------
require(quanteda)
koheiCorpus <- corpus(textfile("~/Dropbox/QUANTESS/Testing/Kohei tokenizer/1996-1997.txt"))
koheiCorpus
nchar(texts(koheiCorpus))

## ------------------------------------------------------------------------
# defaults, including lowercase and normal cleaning
system.time(tokenize(texts(koheiCorpus)))
# just tokenization
system.time(tokenize(texts(koheiCorpus), toLower=FALSE, removeNumbers=FALSE, removePunct=FALSE, removeSeparators=FALSE))

## ------------------------------------------------------------------------
# fixed on a space
system.time(stringi::stri_split_fixed(toLower(texts(koheiCorpus)), " ", simplify=FALSE))
# smarter method with whitespace class
system.time(stringi::stri_split_regex(toLower(texts(koheiCorpus)), "\\s", simplify=FALSE))

## ------------------------------------------------------------------------
system.time(tm::scan_tokenizer(toLower(texts(koheiCorpus))))

