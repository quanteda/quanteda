.onAttach <- function(...) {
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))
    threads <- RcppParallel::defaultNumThreads()
    packageStartupMessage("Use ", threads - 1, " of ", threads, " cores in parallel computing")
    RcppParallel::setThreadOptions(threads - 1)
}
