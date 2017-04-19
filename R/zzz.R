.onAttach <- function(...) {
    # startup message
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))

    # initialize quanteda options
    quanteda_options(reset = TRUE)
    
    # set verbose
    quanteda_options(verbose = FALSE)

    # set threads
    threads <- RcppParallel::defaultNumThreads()
    if (threads == 1) {
        packageStartupMessage("Disabling parallel computing")
        quanteda_options(threads = 1)
    } else {
        packageStartupMessage("Using ", threads - 1, " of ", threads, " cores for parallel computing")
        quanteda_options(threads = threads - 1)
    }

}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
