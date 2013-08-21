rm(list = ls(all = TRUE))

library(quanteda)

setwd("~/Dropbox/budget_speeches/analysis/")
#setwd("~/Dropbox/Shared/Ken/budget_speeches/analysis/")


# read first budget and generate list containing the WFM
d <- read.delim("~/Dropbox/budget_speeches/data/speeches/budget_1983.tab")
budget.corpus <- corpus.create(d$speech, attribs=d[,-17])

budget.wfm <- list()
budget.wfm[[1982]] <- create.fvm.corpus(budget.corpus,groups = c("member_name"))

# add all other budgets to corpus and WFM list

for (i in 1983:1985) {
  d <- read.delim(paste("~/Dropbox/budget_speeches/data/speeches/budget_",i,".tab",sep=""))
  budget.corpus <- corpus.append(budget.corpus, d$speech, d[,-17])
  budget.wfm[[i]] <- create.fvm.corpus(budget.corpus,groups = c("member_name"),select(year==i))
}
