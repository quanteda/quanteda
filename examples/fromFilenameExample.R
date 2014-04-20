
dirname <- "~/Dropbox/QUANTESS/corpora/iebudgets/budget_2010/"
attNames <- c("year", "debate", "number", "firstname", "surname", "party")
cr <- corpusFromFilenames(dirname, c("country", "electionType", "year", "language", "party"))
