.onAttach <- function(...) {
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))
    threads <- RcppParallel::defaultNumThreads()
    packageStartupMessage("Using ", threads - 1, " of ", threads, " cores for parallel computing")
    RcppParallel::setThreadOptions(threads - 1)
}
