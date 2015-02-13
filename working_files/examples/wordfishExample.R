# Validation against Will Lowe's wordfish code on 2009 irish budget data

library(austin)

wordfish.out <- wordfish(iebudget2009)
wordfishcpp.out <- textmodel_wordfish(t(iebudget2009),c(1,1,5,5),c(0.1,1e-3))

cbind(wordfish.out$theta,wordfishcpp.out$theta)


# To do list:
# Recode C++ to take prior precisions rather than prior variances, set prior defaults to match Wordfish defaults
# Change tolerances to match default tolerances from Wordfish
# Add direction option





