# implement default options
QUANTEDA_OPTIONS <- list(threads = max(1L, floor(RcppParallel::defaultNumThreads() / 2)),
                         verbose = FALSE,
                         print_dfm_max_ndoc = 20L,
                         print_dfm_max_nfeature = 20L,
                         base_docname = "text",
                         base_featname = "feat",
                         language_stemmer = "english",
                         language_stopwords = "english")


#' get or set package options for quanteda
#' 
#' Get or set global options affecting functions across \pkg{quanteda}.
#' @param ... options to be set, as key-value pair, same as
#'   \code{\link{options}}. This may be a list of valid key-value pairs, useful
#'   for setting a group of options at once (see examples).
#' @param reset logical; if \code{TRUE}, reset all \pkg{quanteda} options to
#'   their default values
#' @param initialize logical; if \code{TRUE}, reset only the \pkg{quanteda}
#'   options that are not already defined.  Used for setting initial values when
#'   some have been defined previously, such as in `.Rprofile`.
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
#' \item{\code{base_docname}}{character; stem name for documents that are
#' unnamed when a corpus, tokens, or dfm are created or when a dfm is converted
#' from another object}
#' \item{\code{base_featname}}{character; stem name for features that are
#' unnamed when they are added, for whatever reason, to a dfm through an operation
#' that adds features}
#' \item{\code{base_featname}}{character; stem name for features that are
#' unnamed when they are added, for whatever reason, to a dfm through an operation
#' that adds features}
#' \item{\code{base_featname}}{character; stem name for features that are
#' unnamed when they are added, for whatever reason, to a dfm through an operation
#' that adds features}
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
#' (opt <- quanteda_options())
#' \donttest{
#' quanteda_options(verbose = TRUE)
#' quanteda_options("verbose" = FALSE)
#' quanteda_options("threads")
#' quanteda_options(print_dfm_max_ndoc = 50L)
#' # reset to defaults
#' quanteda_options(reset = TRUE)
#' # reset to saved options
#' quanteda_options(opt)
#' }
quanteda_options <- function(..., reset = FALSE, initialize = FALSE) {
    args <- list(...)
    
    # if the ... is a list already, use that
    if (length(args) == 1 && is.list(args[[1]])) args <- args[[1]]
    
    if (initialize) 
        return(quanteda_initialize())

    if (reset)
        return(quanteda_reset())

    # if no options are specified
    if (!length(args)) {
        opts <- options()[paste0("quanteda_", names(QUANTEDA_OPTIONS))]
        names(opts) <- stri_replace_first_fixed(names(opts), "quanteda_", "") # remove prefix
        return(opts)
    }
    
    # initialize if needed - in case a call is direct without attaching package
    if (!"package:quanteda" %in% search()) {
        quanteda_initialize()
    }
    
    # if the name of the key only is supplied, return the value
    if (is.null(names(args)))
        return(getOption(paste0("quanteda_", args[[1]])))
    
    # return or set specified options
    for (key in names(args)) {
        value <- args[[key]]
        
        # check for key validity
        if (!key %in% names(QUANTEDA_OPTIONS))
            stop(key, " is not a valid quanteda option")
        
        # special setting for threads
        if (key == "threads") {
            available_threads <- RcppParallel::defaultNumThreads()
            if (value > available_threads) {
                warning("setting threads instead to maximum available ", available_threads)
                value <- available_threads
            }
            RcppParallel::setThreadOptions(value)
        }
        
        # assign the key-value
        options <- list(value)
        names(options) <- paste0("quanteda_", key)
        options(options)
    }
    
    return(invisible(TRUE))
}


quanteda_initialize <- function() {
    opts <- QUANTEDA_OPTIONS
    for (key in names(opts)) {
        if (!is.null(getOption(paste0("quanteda_", key))))
            opts[key] <- NULL # do not overwrite saved options
    }
    quanteda_options(opts)
    return(invisible(TRUE))
}

quanteda_reset <- function() {
    quanteda_options(QUANTEDA_OPTIONS)
    return(invisible(TRUE))
}
