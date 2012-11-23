source("~/Dropbox/code/quanteda/R/quanteda.R")

path <- "~/Dropbox/QUANTESS/quanteda/data/syllableCounts.RData"
testSentence <- "In a statement, the Department of Public Expenditure and Reform said: The Government considers if the public service pay and pensions bill is to make the necessary contribution to the consolidation in expenditure that will be necessary up to 2015, a new deal, setting out a new agenda for change and productivity, is necessary"
print(countSyllables(testSentence))
