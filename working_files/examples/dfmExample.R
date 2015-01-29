# testing dfm and its arguments
library(quanteda)
data(iebudgets)

cust <- c('able', 'accident')


default <- dfm(iebudgets)
defStops <- dfm(iebudgets, stopwords=TRUE)
custStops <-dfm(iebudgets, stopwords=cust)
