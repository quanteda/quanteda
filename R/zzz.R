.onAttach <- function(...) {
    # startup message
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")))

    # get options pre-set in .Rprofile
    #pre_options <- list(getOption("quanteda_verbose"),
    #                    getOption("quanteda_print_dfm_max_ndoc"),
    #                    getOption("quanteda_print_dfm_max_nfeature"),
    #                    getOption("quanteda_threads"))
    
    # initialize quanteda options
    quanteda_options(reset = TRUE)
    
    # recover the setting in .Rprofile
    #if ((length(pre_options)>=1) & !is.null(pre_options[[1]])) options(quanteda_verbose = pre_options[[1]])
    #if ((length(pre_options)>=2) & !is.null(pre_options[[2]])) options(quanteda_print_dfm_max_ndoc = pre_options[[2]])
    #if ((length(pre_options)>=3) & !is.null(pre_options[[3]])) options(quanteda_print_dfm_max_nfeature = pre_options[[3]])
    #if ((length(pre_options)>=4) & !is.null(pre_options[[4]])) options(quanteda_threads = pre_options[[4]])
    
    if (file.exists("~/.Rprofile")){
        pre_options<-grep("quanteda_", readLines("~/.Rprofile"), value=TRUE)
        eval(parse(text = pre_options))
    }
}

.onUnload <- function (libpath) {
    library.dynam.unload("quanteda", libpath)
}
