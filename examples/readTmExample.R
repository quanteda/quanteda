# work-in-progress to read convert quanteda corpus format to and from tm document-term-matrix format
library(quanteda)
library(tm)
data(iebudgets)

out <- Corpus(VectorSource(iebudgets$attribs$texts))

tdm <- TermDocumentMatrix(out)

tdm$