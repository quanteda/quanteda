#' Get or set package options for quanteda
#'
#' Get or set global options affecting functions across \pkg{quanteda}.
#' @param ... options to be set, as key-value pair, same as
#'   [options()]. This may be a list of valid key-value pairs, useful
#'   for setting a group of options at once (see examples).
#' @param reset logical; if `TRUE`, reset all \pkg{quanteda} options to
#'   their default values
#' @param initialize logical; if `TRUE`, reset only the \pkg{quanteda}
#'   options that are not already defined.  Used for setting initial values when
#'   some have been defined previously, such as in `.Rprofile`.
#' @details Currently available options are: \describe{
#' \item{`verbose`}{logical; if `TRUE` then use this as the default
#' for all functions with a `verbose` argument}
#' \item{`threads`}{integer; specifies the number of threads to use in
#' parallelized functions}
#' \item{`print_dfm_max_ndoc`}{integer; specifies the number of documents
#' to display when using the defaults for printing a dfm}
#' \item{`print_dfm_max_nfeat`}{integer; specifies the number of
#' features to display when using the defaults for printing a dfm}
#' \item{`base_docname`}{character; stem name for documents that are
#' unnamed when a corpus, tokens, or dfm are created or when a dfm is converted
#' from another object} \item{`base_featname`}{character; stem name for
#' features that are unnamed when they are added, for whatever reason, to a dfm
#' through an operation that adds features}
#' \item{`base_compname`}{character; stem name for components that are
#' created by matrix factorization} 
#' \item{`language_stemmer`}{character; language option for [char_wordstem()], 
#' [tokens_wordstem()], and [dfm_wordstem()]} 
#' \item{`pattern_hashtag`, `pattern_username`}{character; regex patterns for
#' (social media) hashtags and usernames respectively, used to avoid segmenting
#' these in the default internal "word" tokenizer}
#' \item{`tokens_block_size`}{integer; specifies the
#'   number of documents to be tokenized at a time in blocked tokenization.
#'   When the number is large, tokenization becomes faster but also memory-intensive.}
#' \item{`tokens_locale`}{character; specify locale in stringi boundary detection in 
#'   tokenization and corpus reshaping. See [stringi::stri_opts_brkiter()].}}
#' @return When called using a `key = value` pair (where `key` can be
#' a label or quoted character name)), the option is set and `TRUE` is
#' returned invisibly.
#'
#' When called with no arguments, a named list of the package options is
#' returned.
#'
#' When called with `reset = TRUE` as an argument, all arguments are
#' options are reset to their default values, and `TRUE` is returned
#' invisibly.
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
    if (length(args) == 1 && is.list(args[[1]])) 
        args <- args[[1]]
    
    # initialize automatically it not yet done so
    if (is.null(getOption('quanteda_initialized')) || !"package:quanteda" %in% search())
        quanteda_initialize()
        
    if (initialize) {
        return(invisible(quanteda_initialize()))
    } else if (reset) {
        return(invisible(quanteda_reset()))
    } else if (!length(args)) {
        # return all option values with names
        opts_names <- names(get_options_default())
        opts <- options()[paste0("quanteda_", opts_names)]
        names(opts) <- stri_sub(names(opts), 10, -1) # remove prefix
        return(opts)
    } else if (is.null(names(args))) {
        # return a option value
        return(getOption(paste0("quanteda_", args[[1]])))
    } else {
        # set value
        for (key in names(args)) {
            set_option_value(key, args[[key]])
        }
        return(invisible(args))
    }
}

quanteda_initialize <- function() {
    opts <- get_options_default()
    for (key in names(opts)) {
        if (is.null(getOption(paste0("quanteda_", key))))
            set_option_value(key, opts[[key]])
    }
    unlist(options('quanteda_initialized' = TRUE), use.names = FALSE)
}

quanteda_reset <- function() {
    opts <- get_options_default()
    for (key in names(opts)) {
        set_option_value(key, opts[[key]])
    }
    unlist(options('quanteda_initialized' = TRUE), use.names = FALSE)
}

set_option_value <- function(key, value) {
    
    opts <- get_options_default()
    # check for key validity
    if (!key %in% names(opts))
        stop(key, " is not a valid quanteda option")
    
    # special setting for threads
    if (key == "threads") {
        value <- as.integer(value)
        value_default <- RcppParallel::defaultNumThreads()
        if (value < 1)
            stop("Number of threads must be greater or equal to 1")
        if (value > value_default) {
            warning("Setting threads instead to maximum available ", value_default, call. = FALSE)
            value <- value_default
        }
        RcppParallel::setThreadOptions(value)
        Sys.setenv("OMP_THREAD_LIMIT" = value)
    }
    
    # assign the key-value
    opts <- list(value)
    names(opts) <- paste0("quanteda_", key)
    options(opts)
    
}

# returns default options
get_options_default <- function(){
    opts <- list(threads = min(RcppParallel::defaultNumThreads(), 2),
                 verbose = FALSE,
                 print_dfm_max_ndoc = 6L,
                 print_dfm_max_nfeat = 10L,
                 print_dfm_summary = TRUE,
                 print_corpus_max_ndoc = 6L,
                 print_corpus_max_nchar = 60L,
                 print_corpus_summary = TRUE,
                 print_tokens_max_ndoc = 6L,
                 print_tokens_max_ntoken = 12L,
                 print_tokens_summary = TRUE,
                 print_dictionary_max_nkey = 6L,
                 print_dictionary_max_nval = 20L,
                 print_dictionary_summary = TRUE,
                 base_docname = "text",
                 base_featname = "feat",
                 base_compname = "comp",
                 language_stemmer = "english",
                 pattern_hashtag = "#\\w+#?",
                 pattern_username = "@[a-zA-Z0-9_]+",
                 tokens_block_size = 10000L,
                 tokens_locale = "en_US@ss=standard")
    return(opts)
}
