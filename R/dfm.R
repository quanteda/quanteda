#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix from a [tokens] or [dfm] object.
#' @param x a [tokens] or [dfm] object.
#' @param tolower convert all features to lowercase.
#' @param remove_padding logical; if `TRUE`, remove the "pads" left as empty tokens after
#' calling [tokens()] or [tokens_remove()] with `padding = TRUE`.
#' @param verbose display messages if `TRUE`.
#' @param ... not used.
#' @section Changes in version 3:
#' In \pkg{quanteda} v4, many convenience functions formerly available in
#' `dfm()` were removed.
#' @return a [dfm-class] object
#' @import Matrix
#' @export
#' @rdname dfm
#' @keywords dfm
#' @seealso  [as.dfm()], [dfm_select()], [dfm-class]
#' @examples
#' ## for a corpus
#' toks <- data_corpus_inaugural |>
#'   corpus_subset(Year > 1980) |>
#'   tokens()
#' dfm(toks)
#'
#' # removal options
#' toks <- tokens(c("a b c", "A B C D")) |>
#'     tokens_remove("b", padding = TRUE)
#' toks
#' dfm(toks)
#' dfm(toks) |>
#'  dfm_remove(pattern = "") # remove "pads"
#'
#' # preserving case
#' dfm(toks, tolower = FALSE)
dfm <- function(x,
                tolower = TRUE,
                remove_padding = FALSE,
                verbose = quanteda_options("verbose"),
                ...) {
    
    # to catch expansion of defunct "remove" to "remove_padding"
    check_defunct_dfm_args(names(as.list(sys.call())[-1]))
                           
    tolower <- check_logical(tolower)
    remove_padding <- check_logical(remove_padding)
    verbose <- check_logical(verbose)

    UseMethod("dfm")
}

#' @export
dfm.default <- function(x, ...) {
    check_class(class(x), "dfm", defunct_methods = c("corpus", "character"))
}

#' @export
dfm.tokens <- function(x,
                       tolower = TRUE,
                       remove_padding = FALSE,
                       verbose = quanteda_options("verbose"),
                       ...) {
    
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    dfm(as.tokens_xptr(x), tolower = tolower,
        remove_padding = remove_padding, verbose = verbose, ...)

}

#' @method dfm tokens_xptr
#' @export
dfm.tokens_xptr <- function(x,
                            tolower = TRUE,
                            remove_padding = FALSE,
                            verbose = quanteda_options("verbose"),
                            ...) {
    
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    check_dots(...)
    if (verbose)
        catm("Creating a dfm from a", global$object_class, "object...\n")

    x <- as.tokens_xptr(x) # avoid modifying the original tokens
    if (tolower)
        x <- tokens_tolower(x)
    if (remove_padding)
        x <- tokens_remove(x, "", valuetype = "fixed")
    attrs <- attributes(x)
    temp <- t(cpp_dfm(x, attrs$meta$object$what == "dictionary"))
    result <- build_dfm(temp, colnames(temp),
                        docvars = get_docvars(x, user = TRUE, system = TRUE),
                        meta = attrs[["meta"]])

    if (verbose) {
        catm(" ...complete, elapsed time:",
             format((proc.time() - global$proc_time)[3], digits = 3), "seconds.\n")
        catm("Finished constructing a", paste(format(dim(result), big.mark = ",", trim = TRUE), collapse = " x "),
             "sparse dfm.\n")
    }
    global$object_class <- NULL
    return(result)
}



#' @importFrom stringi stri_trans_totitle
#' @export
dfm.dfm <- function(x,
                    tolower = TRUE,
                    remove_padding = FALSE,
                    verbose = quanteda_options("verbose"),
                    ...) {
    
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    check_dots(...)
    x <- as.dfm(x)

    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- dfm_tolower(x)
    }

    remove_padding <- check_logical(remove_padding)
    if (remove_padding)
        x <- dfm_remove(x, "", valuetype = "fixed")

    # remove any NA named columns
    is_na <- is.na(featnames(x))
    if (any(is_na))
        x <- x[, !is_na, drop = FALSE]
    
    global$object_class <- NULL
    return(x)
}

# utility functions -----------

# create an empty dfm for given features and documents
make_null_dfm <- function(feature = NULL, document = NULL) {
    if (is.null(feature)) feature <- character()
    if (is.null(document)) document <- character()
    temp <- as(as(as(sparseMatrix(
        i = NULL,
        j = NULL,
        dims = c(length(document), length(feature))
    ), "CsparseMatrix"), "generalMatrix"), "dMatrix")

    build_dfm(temp, feature,
              docvars = make_docvars(length(document), document))
}

# pad dfm with zero-count features
pad_dfm <- function(x, feature) {
    feat_pad <- setdiff(feature, featnames(x))
    if (length(feat_pad)) {
        suppressWarnings(
            x <- cbind(x, make_null_dfm(feat_pad, docnames(x)))
        )
    }
    x <- x[, match(feature, featnames(x))]
    return(x)
}

# defunct methods -------------

#' @export
dfm.character <- function(x, ...) {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "dfm.character()",
        with = I('`dfm(tokens(x))`')
    )
}

#' @export
dfm.corpus <- function(x, ...) {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "dfm.corpus()",
        with = I('`dfm(tokens(x))`')
    )
}

check_defunct_dfm_args <- function(arg_names) {
    if ("stem" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0", 
            what = "quanteda::dfm(stem)",
            with = "dfm_stem()")
    }
    if ("select" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(select)",
            with = "dfm_select()")
    }
    if ("remove" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(remove)",
            with = "dfm_remove()")
    }
    if ("dictionary" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(dictionary)",
            with = "dfm_lookup()")
    }
    if ("thesaurus" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(thesaurus)",
            with = I("`dfm_lookup(..., exclusive = FALSE)`"))
    }
    if ("valuetype" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(valuetype)",
            with = "dfm_select()")
    }
    if ("case_insensitive" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(case_insensitive)",
            with = "dfm_select()")
    }
    if ("groups" %in% arg_names) {
        lifecycle::deprecate_stop(
            when = "3.0",
            what = "quanteda::dfm(groups)",
            with = "dfm_group()")
    }
}
