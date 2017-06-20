# implement default options
QUANTEDA_OPTION_LIST <- list(quanteda_threads = max(1L, RcppParallel::defaultNumThreads()/2),
                             quanteda_verbose = FALSE,
                             quanteda_print_dfm_max_ndoc = 20L,
                             quanteda_print_dfm_max_nfeature = 20L)


#' get or set package options for quanteda
#' 
#' Get or set global options affecting functions across \pkg{quatneda}.
#' @param ... options to be set, as key-value pair, same as \code{\link{options}}. 
#'   This may be a list of valid key-value pairs, useful for setting a group of
#'   options at once (see examples).
#' @param reset logical; if \code{TRUE}, reset all \pkg{quanteda} options to their 
#'   default values
#' @param initialize logical; if \code{TRUE}, reset only the \pkg{quanteda} options 
#'   that are not already defined.  Used for setting initial values when some have 
#'   been defined previously, such as in `.Rprofile`.
#' @details
#' Currently available options are:
#' \describe{
#' \item{\code{verbose}}{logical; if \code{TRUE} then use this as the default
#'    for all functions with a \code{verbose} argument}
#' \item{\code{threads}}{integer; specifies the number of threads to use in
#'    use this as the setting in all functions that uee parallelization}
#' \item{\code{print_dfm_max_ndoc}}{integer; specifies the number of documents
#'    to display when using the defaults for printing a dfm}
#' \item{\code{print_dfm_max_nfeature}}{integer; specifies the number of features
#'    to display when using the defaults for printing a dfm}
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
#' (qopts <- quanteda_options())
#' \donttest{
#' quanteda_options(verbose = TRUE)
#' quanteda_options("verbose" = FALSE)
#' quanteda_options("threads")
#' quanteda_options(print_dfm_max_ndoc = 50L)
#' # reset to defaults
#' quanteda_options(reset = TRUE)
#' # reset to saved values 
#' quanteda_options(qopts)
#' }
quanteda_options <- function(..., reset = FALSE, initialize = FALSE) {
    args <- list(...)
    
    # if the ... is a list already, use that
    if (length(args)==1 && is.list(args[[1]])) args <- args[[1]]
    
    if (initialize)
        return(quanteda_initialize())
    
    if (reset)
        return(quanteda_reset())

    # if no options are specified
    if (!length(args)) {
        retlist <- options()[names(QUANTEDA_OPTION_LIST)]
        names(retlist) <- stringi::stri_replace_all_fixed(names(retlist), "quanteda_", "")
        return(retlist)
    }
    
    # initialize if needed - in case a call is direct without attaching package
    if (!"package:quanteda" %in% search()) {
        quanteda_initialize()
    }
    
    # return or set specified options
    for (i in seq_along(args)) {
        key <- names(args)[i]
        value <- args[[i]]
        
        # if the name of the argument was specified, without assigning a value
        if (is.null(key)) {
            key <- value
            value <- NA
        }
        
        # check for key validity
        if (!key %in% stri_replace_all_fixed(names(QUANTEDA_OPTION_LIST), "quanteda_", ""))
            stop(key, " is not a valid quanteda option")
        
        # if the name of the key only is supplied, return the value
        if (is.na(value))
            return(getOption(paste0("quanteda_", key)))
        
        # special setting for threads
        if (key == "threads") {
            if (value > (available_threads <- RcppParallel::defaultNumThreads())) {
                warning("setting threads instead to maximum available ", available_threads)
                value <- available_threads
            }
        }
        
        # assign the key-value
        opt_list <- list(value)
        names(opt_list) <- paste0("quanteda_", key)
        options(opt_list)
    }

    return(invisible(TRUE))
}


quanteda_initialize <- function() {
    apply_list <- QUANTEDA_OPTION_LIST
    for (opt in names(QUANTEDA_OPTION_LIST)) {
        if (!is.null(getOption(opt)))
            apply_list[opt] <- NULL
    }
    options(apply_list)
    return(invisible(TRUE))
}

quanteda_reset <- function() {
    options(QUANTEDA_OPTION_LIST)
    return(invisible(TRUE))
}
