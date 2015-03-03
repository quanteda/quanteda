library(quanteda)

tmp <- read.csv("/home/paul/Desktop/Tweets2.csv", header = TRUE, stringsAsFactors=FALSE)
corp <- corpus(tmp$text, docvars = data.frame(uni=tmp$University))

mat <- dfm(corp, groups="uni")
mat <- trim(mat, minCount=8, minDoc=8)
mat <- weight(mat, type="relFreq")

sentDict <- list(good=c( 'rich', 'smart', 'best'), bad=c('poor', 'dumb', 'worst'))
dictMat <- dfm(corp, groups="uni", dictionary=sentDict)