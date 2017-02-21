.onAttach <- function(...) {
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))
    threads <- RcppParallel::defaultNumThreads()
    if (threads == 1) {
        packageStartupMessage("Disabling parallel computing")
        RcppParallel::setThreadOptions(1)
    } else {
        packageStartupMessage("Using ", threads - 1, " of ", threads, " cores for parallel computing")
        RcppParallel::setThreadOptions(threads - 1)
    }
}
