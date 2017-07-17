.onAttach <- function(...) {
    # startup message
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))
    
    # initialize options
    quanteda_options(initialize = TRUE)
    
    # threads message
    if (qatd_cpp_tbb_enabled()) {
        packageStartupMessage("Using ", quanteda_options("threads"), " of ", 
                              RcppParallel::defaultNumThreads(), " threads for parallel computing")
    } else {
        packageStartupMessage("Parallel computing is disabled") 
    }
}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
