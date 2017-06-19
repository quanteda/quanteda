.onAttach <- function(...) {
    # startup message
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))
    
    # initialize options
    quanteda_options(initialize = TRUE)
    
    # threads message
    packageStartupMessage("Using ", quanteda_options("threads"), " of ", 
                          RcppParallel::defaultNumThreads(), " threads for parallel computing")
}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
