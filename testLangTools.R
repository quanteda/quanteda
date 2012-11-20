source("~/Dropbox/code/quanteda/R/quanteda.R")

path <- "~/Dropbox/QUANTESS/quanteda/data/syllableCounts.RData"
testSentence <- "In a statement, the Department of Public Expenditure and Reform said: The Government considers if the public service pay and pensions bill is to make the necessary contribution to the consolidation in expenditure that will be necessary up to 2015, a new deal, setting out a new agenda for change and productivity, is necessary"
print(countSyllables(testSentence, path))













#sum the syllables in the words

# words <- c()
# counts <-c()
# 
# for(j in 1:length(pd)){
#   count <- 0
# 
#   for(w in 1:length(pd[[j]])){
#     c <- grep("[[:digit:]]", pd[[j]][w])
#     if(!length(c)==0){
#       count <- count+1
#     }
#   }
#   words <- c(words, pd[[j]][1])
#   counts <- c(counts, count)
#   if(j%%1000==0) print(pd[[j]][1])
# }
# names(counts) <- words
# print(counts)


