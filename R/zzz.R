.onAttach <- function(...) {
    # startup message
    packageStartupMessage("Initializing package: ‘quanteda‘")
    packageStartupMessage("Package version: ", as.character(utils::packageVersion("quanteda")))
    
    # initialize options
    quanteda_options(initialize = TRUE)
    
    # threads message
    if (qatd_cpp_tbb_enabled()) {
        packageStartupMessage("Parallel computing: ", quanteda_options("threads"), " of ", 
                              RcppParallel::defaultNumThreads(), " threads")
    } else {
        packageStartupMessage("Parallel computing: disabled") 
    }
    
    # wesite link
    packageStartupMessage('To learn how to use ‘quanteda‘ through examples and tutorials, go to https://quanteda.io')
}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
