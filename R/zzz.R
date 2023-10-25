.onAttach <- function(...) {
    # startup message
    #packageStartupMessage("Initializing package: ", sQuote('quanteda'))
    packageStartupMessage("Package version: ", as.character(utils::packageVersion("quanteda")), "\n",
                          "Unicode version: ", stringi::stri_info()[["Unicode.version"]], "\n",
                          "ICU version: ", stringi::stri_info()[["ICU.version"]])
    
    # initialize options
    quanteda_options(initialize = TRUE)
    
    # threads message
    if (cpp_tbb_enabled()) {
        packageStartupMessage("Parallel computing: ", quanteda_options("threads"), " of ", 
                              cpp_get_max_thread(), " threads used.")
    } else {
        packageStartupMessage("Parallel computing: disabled") 
    }
    
    # wesite link
    packageStartupMessage("See https://quanteda.io for tutorials and examples.")
}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
