#' Convert the case of tokens
#'
#' `tokens_tolower()` and `tokens_toupper()` convert the features of a
#' [tokens] object and re-index the types.
#' @inheritParams char_tolower
#' @inheritParams messages
#' @importFrom stringi stri_trans_tolower
#' @export
#' @examples
#' # for a document-feature matrix
#' toks <- tokens(c(txt1 = "b A A", txt2 = "C C a b B"))
#' tokens_tolower(toks)
#' tokens_toupper(toks)
tokens_tolower <- function(x, keep_acronyms = FALSE) {
    UseMethod("tokens_tolower")
}

#' @export
tokens_tolower.default <- function(x, keep_acronyms = FALSE) {
    check_class(class(x), "tokens_tolower")
}

#' @export
tokens_tolower.tokens <- function(x, keep_acronyms = FALSE) {
    as.tokens(tokens_tolower(as.tokens_xptr(x), keep_acronyms))
}

lowercase_types <- function(type, keep_acronyms) {
    if (keep_acronyms) {
        is_acronyms <- stri_detect_regex(type, "^\\p{Uppercase_Letter}(\\p{Uppercase_Letter}|\\d)+$")
    } else {
        is_acronyms <- rep(FALSE, length(type))
    }
    type[!is_acronyms] <- stri_trans_tolower(type[!is_acronyms])
    return(type)
}

#' @rdname tokens_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
tokens_toupper <- function(x) {
    UseMethod("tokens_toupper")
}
    
#' @export
tokens_toupper.default <- function(x) {
    check_class(class(x), "tokens_toupper")
}

#' @noRd
#' @export
tokens_toupper.tokens <- function(x) {
    as.tokens(tokens_toupper(as.tokens_xptr(x)))
}


#' Convert the case of character objects
#'
#' `char_tolower` and `char_toupper` are replacements for
#' \link[base:chartr]{base::tolower()} and \link[base:chartr]{base::tolower()}
#' based on the \pkg{stringi} package.  The \pkg{stringi} functions for case
#' conversion are superior to the \pkg{base} functions because they correctly
#' handle case conversion for Unicode.  In addition, the `*_tolower()` functions
#' provide an option for preserving acronyms.
#' @param x the input object whose character/tokens/feature elements will be
#'   case-converted
#' @param keep_acronyms logical; if `TRUE`, do not lowercase any
#'   all-uppercase words (applies only to `*_tolower()` functions)
#' @export
#' @examples
#' txt1 <- c(txt1 = "b A A", txt2 = "C C a b B")
#' char_tolower(txt1)
#' char_toupper(txt1)
#'
#' # with acronym preservation
#' txt2 <- c(text1 = "England and France are members of NATO and UNESCO",
#'           text2 = "NASA sent a rocket into space.")
#' char_tolower(txt2)
#' char_tolower(txt2, keep_acronyms = TRUE)
#' char_toupper(txt2)
char_tolower <- function(x, keep_acronyms = FALSE) {
    UseMethod("char_tolower")
}

#' @export
char_tolower.default <- function(x, keep_acronyms = FALSE) {
    check_class(class(x), "char_tolower")
}

#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex stri_trans_tolower
#' @export
char_tolower.character <- function(x, keep_acronyms = FALSE) {
    keep_acronyms <- check_logical(keep_acronyms)
    name <- names(x)
    if (keep_acronyms) {
        match <- stri_extract_all_regex(x, "\\b(\\p{Uppercase_Letter}(\\p{Uppercase_Letter}|\\d)+)\\b")
        for (i in which(lengths(match) > 0)) {
            m <- unique(match[[i]])
            x[i] <- stri_replace_all_regex(x[i], paste0("\\b", m, "\\b"),
                                                 paste0("\uE000", m, "\uE001"),
                                           vectorize_all = FALSE)
            x[i] <- stri_trans_tolower(x[i])
            x[i] <- stri_replace_all_regex(x[i], paste0("\uE000", stri_trans_tolower(m), "\uE001"),
                                                 m, vectorize_all = FALSE)
        }
    } else {
        x <- stri_trans_tolower(x)
    }
    names(x) <- name
    return(x)
}

#' @rdname char_tolower
#' @export
char_toupper <- function(x) {
    UseMethod("char_toupper")
}

#' @export
char_toupper.default <- function(x) {
    check_class(class(x), "char_toupper")
}

#' @importFrom stringi stri_trans_toupper
#' @export
char_toupper.character <- function(x) {
    name <- names(x)
    x <- stri_trans_toupper(x)
    names(x) <- name
    return(x)
}

#' Convert the case of the features of a dfm and combine
#'
#' `dfm_tolower()` and `dfm_toupper()` convert the features of the dfm or
#' fcm to lower and upper case, respectively, and then recombine the counts.
#' @inheritParams char_tolower
#' @inheritParams messages
#' @importFrom stringi stri_trans_tolower
#' @export
#' @examples
#' # for a document-feature matrix
#' dfmat <- dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE)
#' dfmat
#' dfm_tolower(dfmat)
#' dfm_toupper(dfmat)
#'
dfm_tolower <- function(x, keep_acronyms = FALSE,
                        verbose = quanteda_options("verbose")) {
    UseMethod("dfm_tolower")
}

#' @export
dfm_tolower.default <- function(x, keep_acronyms = FALSE,
                                verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_tolower")
}

#' @export
dfm_tolower.dfm <- function(x, keep_acronyms = FALSE,
                            verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    keep_acronyms <- check_logical(keep_acronyms)
    if (!nfeat(x)) return(x)
    set_dfm_featnames(x) <- lowercase_types(featnames(x), keep_acronyms)
    if (verbose)
        before <- stats_dfm(x)
    x <- dfm_compress(x, margin = "features")
    if (verbose)
        message_dfm("dfm_tolower()", before, stats_dfm(x))    
    return(x)
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
dfm_toupper <- function(x,
                        verbose = quanteda_options("verbose")) {
    UseMethod("dfm_toupper")
}

#' @export
dfm_toupper.default <- function(x,
                                verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_toupper")
}

#' @export
dfm_toupper.dfm <- function(x,
                            verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    if (!nfeat(x)) return(x)
    set_dfm_featnames(x) <- char_toupper(featnames(x))
    if (verbose)
        before <- stats_dfm(x)
    x <- dfm_compress(x, margin = "features")
    if (verbose)
        message_dfm("dfm_toupper()", before, stats_dfm(x))    
    return(x)
}

#' @rdname dfm_tolower
#' @details `fcm_tolower()` and `fcm_toupper()` convert both dimensions of
#'   the [fcm] to lower and upper case, respectively, and then recombine
#'   the counts. This works only on fcm objects created with `context =
#'   "document"`.
#' @export
#' @examples
#' # for a feature co-occurrence matrix
#' fcmat <- fcm(tokens(c("b A A d", "C C a b B e")),
#'              context = "document")
#' fcmat
#' fcm_tolower(fcmat)
#' fcm_toupper(fcmat)
fcm_tolower <- function(x, keep_acronyms = FALSE,
                        verbose = quanteda_options("verbose")) {
    UseMethod("fcm_tolower")
}

#' @export
fcm_tolower.default <- function(x, keep_acronyms = FALSE,
                                verbose = quanteda_options("verbose")) {
    check_class(class(x), "fcm_tolower")
}

#' @export
fcm_tolower.fcm <- function(x, keep_acronyms = FALSE,
                            verbose = quanteda_options("verbose")) {
    x <- as.fcm(x)
    keep_acronyms <- check_logical(keep_acronyms)
    set_fcm_featnames(x) <- lowercase_types(featnames(x), keep_acronyms)
    if (verbose)
        before <- stats_dfm(x)
    x <- fcm_compress(x)
    if (verbose)
        message_dfm("fcm_tolower()", before, stats_dfm(x))    
    return(x)
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
fcm_toupper <- function(x,
                        verbose = quanteda_options("verbose")) {
    UseMethod("fcm_toupper")
}

#' @export
fcm_toupper.default <- function(x,
                                verbose = quanteda_options("verbose")) {
    check_class(class(x), "fcm_toupper")
}

#' @export
fcm_toupper.fcm <- function(x,
                            verbose = quanteda_options("verbose")) {
    x <- as.fcm(x)
    set_fcm_featnames(x) <- char_toupper(colnames(x))
    if (verbose)
        before <- stats_dfm(x)
    x <- fcm_compress(x)
    if (verbose)
        message_dfm("fcm_toupper()", before, stats_dfm(x))    
    return(x)
}
