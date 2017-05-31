# implement default options
QUANTEDA_OPTION_LIST <- list(quanteda_threads = 1L,
                             quanteda_verbose = FALSE,
                             quanteda_print_dfm_ndoc = 20L,
                             quanteda_print_dfm_nfeature = 20L)

#' get or set package options for quanteda
#' 
#' Get or set global options affecting functions across \pkg{quatneda}.
#' @param ... options to be set, as key-value pair, same as \code{\link{options}}
#' @param reset logical; if \code{TRUE}, reset all \pkg{quanteda} options to their 
#'   default values
#' @details
#' Currently available options are:
#' \describe{
#' \item{\code{verbose}}{logical; if \code{TRUE} then use this as the default
#'    for all functions with a \code{verbose} argument}
#' \item{\code{threads}}{integer; specifies the number of threads to use in
#'    use this as the setting in all functions that uee parallelization}
#' }
#' @return 
#'   When called using a \code{key = value} pair (where \code{key} can be a label or 
#'   quoted character name)), the option is set and \code{TRUE} is returned invisibly.
#' 
#'   When called with no arguments, a named list of the package options is returned.
#'   
#'   When called with \code{reset = TRUE} as an argument, all arguments are options 
#'   are reset to their default values, and \code{TRUE} is returned invisibly.
#' @export
#' @importFrom RcppParallel setThreadOptions
#' @examples
#' quanteda_options()
#' quanteda_options(verbose = FALSE)
#' quanteda_options("verbose" = FALSE)
#' quanteda_options("threads")
#' quanteda_options(print_dfm_ndoc = 50L)
#' \dontrun{
#' quanteda_options(reset = TRUE) 
#' }
quanteda_options <- function(..., reset = FALSE) {
    args <- list(...)
    
    if ("reset" %in% names(args)) 
        reset <- args$reset
    if (reset) {
        
        #########################
        ## HARD-CODED DEFAULTS ##
        #########################
        options(quanteda_threads = 1L)
        options(quanteda_verbose = FALSE)
        options(quanteda_print_dfm_ndoc = 20L)
        options(quanteda_print_dfm_nfeature = 20L)
        
        return(invisible(TRUE))
    }
        
    # if no options are specified
    if (!length(args)) {
        retlist <- options()[names(QUANTEDA_OPTION_LIST)]
        names(retlist) <- stringi::stri_replace_all_fixed(names(retlist), "quanteda_", "")
        return(retlist)
    }
    
    for (i in seq_along(args)) {
        key <- names(args)[i]
        value <- args[[i]]
        
        # if the name of the argument was specified, without assigning a value
        if (is.null(key)) {
            key <- value
            value <- NA
        }
        
        if (key == "threads") {
            if (is.na(value)) {
                return(getOption("quanteda_threads"))
            } else {
                options(quanteda_threads = value)
                RcppParallel::setThreadOptions(value)
            }
        
        } else if (key == "verbose") {
            if (is.na(value)) {
                return(getOption("quanteda_verbose"))
            } else {
                options(quanteda_verbose = value)
            }
        
        } else if (key == "print_dfm_ndoc"){
            if (is.na(value)) {
                return(getOption("quanteda_print_dfm_ndoc"))
            } else {
                options(quanteda_print_dfm_ndoc = value)
            }
        
        } else if (key == "print_dfm_nfeature"){
            if (is.na(value)) {
                return(getOption("quanteda_print_dfm_nfeature"))
            } else {
                options(quanteda_print_dfm_nfeature = value)
            }
        
        } else {
            stop(key, " is not a valid quanteda option")
        }
    }
    return(invisible(TRUE))
}


