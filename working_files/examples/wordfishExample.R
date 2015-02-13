# Validation and speed comparison against Will Lowe's wordfish code on 2009 irish budget data

library(quanteda)
library(austin)
data(iebudget2009)

wordfishcpp.out <- textmodel_wordfish(t(iebudget2009))

wordfish.out <- wordfish(iebudget2009)

comparison <- cbind(wordfish.out$theta,wordfishcpp.out$theta)
print(comparison)
  cor(comparison)

# To do list:
# Add standard error code




