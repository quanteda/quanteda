#' convert the case of tokens
#' 
#' \code{tokens_tolower} and \code{tokens_toupper} convert the features of a
#' \link{tokens} object and reindex the types.
#' @param x a \link{tokens} object
#' @param ... additional arguments passed to \code{\link{char_tolower}}
#'   (currently this is limited to \code{keep_acronyms})
#' @importFrom stringi stri_trans_tolower
#' @seealso \code{\link{char_tolower}}, \code{\link{char_toupper}}
#' @export
#' @examples
#' # for a document-feature matrix
#' toks <- tokens(c(txt1 = "b A A", txt2 = "C C a b B"))
#' tokens_tolower(toks) 
#' tokens_toupper(toks)
tokens_tolower <- function(x, ...) {
    UseMethod("tokens_tolower")
}

#' @noRd
#' @export
tokens_tolower.tokenizedTexts <- function(x, ...) {
    as.tokenizedTexts(tokens_tolower(as.tokens(x), ...))
}

#' @noRd
#' @export
tokens_tolower.tokens <- function(x, ...) {
    types(x) <- char_tolower(types(x), ...)
    tokens_hashed_recompile(x)
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
    tokens_hashed_recompile(x)
}


#' convert the case of character objects
#' 
#' \code{char_tolower} and \code{char_toupper} are replacements for 
#' \link[base]{tolower} and \link[base]{toupper} based on the \pkg{stringi} 
#' package.
#' @param x a \link{tokens} object
#' @param keep_acronyms if \code{TRUE}, do not lowercase any all-uppercase words.
#'   Only applies to \code{char_tolower}.
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
