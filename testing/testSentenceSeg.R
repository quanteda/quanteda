# temp testing code for sentence seg
string <- paste(readLines('~/Dropbox/QUANTESS/corpora/iebudgets/budget_2011/2011_BUDGET_02_Michael_Noonan_FG.txt'), collapse="\n")
res <- sentenceSeg(string)
corpus.create(res)
