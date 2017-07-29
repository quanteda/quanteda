#' convert the case of tokens
#' 
#' \code{tokens_tolower} and \code{tokens_toupper} convert the features of a
#' \link{tokens} object and reindex the types.
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

#' @noRd
#' @export
tokens_tolower.tokenizedTexts <- function(x, keep_acronyms = FALSE, ...) {
    as.tokenizedTexts(tokens_tolower(as.tokens(x), keep_acronyms = keep_acronyms, ...))
}

#' @noRd
#' @export
tokens_tolower.tokens <- function(x, keep_acronyms = FALSE, ...) {
    types(x) <- char_tolower(types(x), keep_acronyms = keep_acronyms, ...)
    tokens_recompile(x)
}


#' @rdname tokens_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
tokens_toupper <- function(x, ...) {
    UseMethod("tokens_toupper")
}

#' @noRd
#' @export
tokens_toupper.tokenizedTexts <- function(x, ...) {
    as.tokenizedTexts(tokens_toupper(as.tokens(x), ...))
}
    
#' @noRd
#' @export
tokens_toupper.tokens <- function(x, ...) {
    types(x) <- char_toupper(types(x), ...)
    tokens_recompile(x)
}


#' convert the case of character objects
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
#' txt <- c(txt1 = "b A A", txt2 = "C C a b B")
#' char_tolower(txt) 
#' char_toupper(txt)
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

#' @noRd
#' @export
char_tolower.character <- function(x, keep_acronyms = FALSE, ...) {
    savedNames <- names(x)
    if (keep_acronyms)
        x <- stri_replace_all_regex(x, "\\b(\\p{Uppercase_Letter}{2,})\\b",  "_$1_", ...)
    x <- stri_trans_tolower(x, ...)
    if (keep_acronyms) {
        m1 <- unique(unlist(stri_extract_all_regex(x, "\\b_\\p{Lowercase_Letter}+_\\b", omit_no_match = TRUE, ...)))
        if (length(m1) > 0) {
            m2 <- stri_replace_all_fixed(stri_trans_toupper(m1, ...), "_", "", ...)
            x <- vapply(x, function(s) stri_replace_all_regex(s, m1,  m2, vectorize_all = FALSE, ...), character(1))
        }
    }
    names(x) <- savedNames
    return(x)
}

#' @rdname char_tolower
#' @export 
char_toupper <- function(x, ...) {
    UseMethod("char_toupper")
}

#' @noRd
#' @export 
char_toupper.character <- function(x, ...) {
    savedNames <- names(x)
    x <- stri_trans_toupper(x, ...)
    names(x) <- savedNames
    return(x)
}

#' convert the case of the features of a dfm and combine
#' 
#' \code{dfm_tolower} and \code{dfm_toupper} convert the features of the dfm or
#' fcm to lower and upper case, respectively, and then recombine the counts.
#' @inheritParams char_tolower
#' @importFrom stringi stri_trans_tolower
#' @export
#' @examples
#' # for a document-feature matrix
#' mydfm <- dfm(c("b A A", "C C a b B"), 
#'              toLower = FALSE, verbose = FALSE)
#' mydfm
#' dfm_tolower(mydfm) 
#' dfm_toupper(mydfm)
#'    
dfm_tolower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("dfm_tolower")
}

#' @noRd
#' @export
dfm_tolower.dfm <- function(x, keep_acronyms = FALSE, ...) {
    colnames(x) <- char_tolower(featnames(x), keep_acronyms = keep_acronyms, ...)
    dfm_compress(x, margin = "features")
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
dfm_toupper <- function(x, ...) {
    UseMethod("dfm_toupper")
}

#' @noRd
#' @export
dfm_toupper.dfm <- function(x, ...) {
    colnames(x) <- char_toupper(featnames(x), ...)
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
#' myfcm <- fcm(tokens(c("b A A d", "C C a b B e")), 
#'              context = "document")
#' myfcm
#' fcm_tolower(myfcm) 
#' fcm_toupper(myfcm)   
fcm_tolower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("fcm_tolower")   
}

#' @noRd
#' @export
fcm_tolower.fcm <- function(x, keep_acronyms = FALSE, ...) {
    colnames(x) <- rownames(x) <- 
        char_tolower(colnames(x), keep_acronyms = keep_acronyms, ...)
    fcm_compress(x)
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
fcm_toupper <- function(x, ...) {
    UseMethod("fcm_toupper")   
}

#' @noRd
#' @export
fcm_toupper.fcm <- function(x, ...) {
    colnames(x) <- rownames(x) <-
        char_toupper(colnames(x), ...)
    fcm_compress(x)
}

