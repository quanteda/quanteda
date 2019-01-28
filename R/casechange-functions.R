#' Convert the case of tokens
#' 
#' \code{tokens_tolower} and \code{tokens_toupper} convert the features of a
#' \link{tokens} object and re-index the types.
#' @inheritParams char_tolower
#' @importFrom stringi stri_trans_tolower
#' @export
#' @examples
#' # for a document-feature matrix
#' toks <- tokens(c(txt1 = "b A A", txt2 = "C C a b B"))
#' tokens_tolower(toks) 
#' tokens_toupper(toks)
tokens_tolower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("tokens_tolower")
}

#' @export
tokens_tolower.default <- function(x, keep_acronyms = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "tokens_tolower"))
}

#' @export
tokens_tolower.tokens <- function(x, keep_acronyms = FALSE, ...) {
    types(x) <- lowercase_types(types(x), keep_acronyms)
    tokens_recompile(x, gap = FALSE, dup = TRUE)
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
tokens_toupper <- function(x, ...) {
    UseMethod("tokens_toupper")
}
    
#' @export
tokens_toupper.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "tokens_toupper"))
}

#' @noRd
#' @export
tokens_toupper.tokens <- function(x, ...) {
    types(x) <- char_toupper(types(x), ...)
    tokens_recompile(x, gap = FALSE, dup = TRUE)
}


#' Convert the case of character objects
#' 
#' \code{char_tolower} and \code{char_toupper} are replacements for 
#' \link[base]{tolower} and \link[base]{toupper} based on the \pkg{stringi} 
#' package.  The \pkg{stringi} functions for case conversion are superior to the
#' \pkg{base} functions because they correctly handle case conversion for
#' Unicode.  In addition, the \code{*_tolower} functions provide an option for
#' preserving acronyms.
#' @param x the input object whose character/tokens/feature elements will be 
#'   case-converted
#' @param keep_acronyms logical; if \code{TRUE}, do not lowercase any 
#'   all-uppercase words (applies only to \code{*_tolower} functions)
#' @param ... additional arguments passed to \pkg{stringi} functions, (e.g. 
#'   \code{\link{stri_trans_tolower}}), such as \code{locale}
#' @import stringi
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
char_tolower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("char_tolower")
}

#' @export
char_tolower.default <- function(x, keep_acronyms = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "char_tolower"))
}

#' @export
char_tolower.character <- function(x, keep_acronyms = FALSE, ...) {
    name <- names(x)
    if (keep_acronyms) {
        match <- stri_extract_all_regex(x, "\\b(\\p{Uppercase_Letter}(\\p{Uppercase_Letter}|\\d)+)\\b")
        for (i in which(lengths(match) > 0)) {
            m <- unique(match[[i]])
            x[i] <- stri_replace_all_regex(x[i], paste0('\\b', m, '\\b'), 
                                                 paste0('\uE000', m, '\uE001'), 
                                           vectorize_all = FALSE)
            x[i] <- stri_trans_tolower(x[i])
            x[i] <- stri_replace_all_regex(x[i], paste0('\uE000', stri_trans_tolower(m), '\uE001'), 
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
char_toupper <- function(x, ...) {
    UseMethod("char_toupper")
}

#' @export
char_toupper.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "char_toupper"))
}

#' @export 
char_toupper.character <- function(x, ...) {
    name <- names(x)
    x <- stri_trans_toupper(x, ...)
    names(x) <- name
    return(x)
}

#' Convert the case of the features of a dfm and combine
#' 
#' \code{dfm_tolower} and \code{dfm_toupper} convert the features of the dfm or
#' fcm to lower and upper case, respectively, and then recombine the counts.
#' @inheritParams char_tolower
#' @importFrom stringi stri_trans_tolower
#' @export
#' @examples
#' # for a document-feature matrix
#' dfmat <- dfm(c("b A A", "C C a b B"), tolower = FALSE)
#' dfmat
#' dfm_tolower(dfmat) 
#' dfm_toupper(dfmat)
#'    
dfm_tolower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("dfm_tolower")
}

#' @export
dfm_tolower.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_tolower"))
}

#' @export
dfm_tolower.dfm <- function(x, keep_acronyms = FALSE, ...) {
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    set_dfm_featnames(x) <- lowercase_types(featnames(x), keep_acronyms)
    dfm_compress(x, margin = "features")
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
dfm_toupper <- function(x, ...) {
    UseMethod("dfm_toupper")
}

#' @export
dfm_toupper.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_toupper"))
}

#' @export
dfm_toupper.dfm <- function(x, ...) {
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    set_dfm_featnames(x) <- char_toupper(featnames(x), ...)
    dfm_compress(x, margin = "features")
}

#' @rdname dfm_tolower
#' @details \code{fcm_tolower} and \code{fcm_toupper} convert both dimensions of
#'   the \link{fcm} to lower and upper case, respectively, and then recombine
#'   the counts. This works only on fcm objects created with \code{context = 
#'   "document"}.
#' @export
#' @examples
#' # for a feature co-occurrence matrix
#' fcmat <- fcm(tokens(c("b A A d", "C C a b B e")), 
#'              context = "document")
#' fcmat
#' fcm_tolower(fcmat) 
#' fcm_toupper(fcmat)   
fcm_tolower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("fcm_tolower")   
}

#' @export
fcm_tolower.default <- function(x, keep_acronyms = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "fcm_tolower"))
}

#' @export
fcm_tolower.fcm <- function(x, keep_acronyms = FALSE, ...) {
    set_fcm_featnames(x) <- lowercase_types(featnames(x), keep_acronyms)
    fcm_compress(x)
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
fcm_toupper <- function(x, ...) {
    UseMethod("fcm_toupper")   
}

#' @export
fcm_toupper.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "fcm_toupper"))
}

#' @export
fcm_toupper.fcm <- function(x, ...) {
    set_fcm_featnames(x) <- char_toupper(colnames(x), ...)
    fcm_compress(x)
}

