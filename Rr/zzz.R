.onAttach <- function(...) {
    # startup message
    #packageStartupMessage("Initializing package: ", sQuote('quanteda'))
    packageStartupMessage("Package version: ", as.character(utils::packageVersion("quanteda")))
    
    # initialize options
    quanteda_options(initialize = TRUE)
    
    # threads message
    if (qatd_cpp_tbb_enabled()) {
        packageStartupMessage("Parallel computing: ", quanteda_options("threads"), " of ", 
                              RcppParallel::defaultNumThreads(), " threads used.")
    } else {
        packageStartupMessage("Parallel computing: disabled") 
    }
    
    # wesite link
    packageStartupMessage("See https://quanteda.io for tutorials and examples.")
}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
