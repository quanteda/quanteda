library(quanteda)
data(iebudgets)

# using the existing method
(timeold <- system.time(wfmOld <- create.fvm.corpus(iebudgets)))

# new method
(timenew <- system.time(wfmNew <- dfm.corpus(iebudgets)))

# hoooooowww much faster???
cat("Time difference: the new method is", round(timeold[3]/timenew[3], 1), "times faster!")

# show that the results are identical
sum(t(wfmOld) - wfmNew)  # zero if all cells are the same
class(wfmOld)
class(wfmNew)
