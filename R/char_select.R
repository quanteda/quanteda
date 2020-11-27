#' Select or remove elements from a character vector
#' 
#' @description
#' These function select or discard elements from a [character] object.  For
#' convenience, the functions `char_remove` and `char_keep` are defined as
#' shortcuts for `char_select(x, pattern, selection = "remove")` and
#' `char_select(x, pattern, selection = "keep")`, respectively.  
#' 
#' @description These functions make it easy to change, for instance, stopwords
#'   based on pattern matching.
#' @param x an input [character] vector
#' @inheritParams tokens_select
#' @inheritParams pattern
#' @inheritParams valuetype
#' @return a modified [character] vector
#' @export
#' @examples 
#' # character selection
#' mykeywords <- c("natural", "national", "denatured", "other")
#' char_select(mykeywords, "nat*", valuetype = "glob")
#' char_select(mykeywords, "nat", valuetype = "regex")
#' char_select(mykeywords, c("natur*", "other"))
#' char_select(mykeywords, c("natur*", "other"), selection = "remove")
#' 
char_select <- function(x, pattern, selection = c("keep", "remove"),
                        valuetype = c("glob", "fixed", "regex"),
                        case_insensitive = TRUE) {
    UseMethod("char_select")
}

#' @noRd
#' @export
char_select.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "char_select"))
}

#' @noRd
#' @export
char_select.character <- function(x, pattern, selection = c("keep", "remove"),
                                  valuetype = c("glob", "fixed", "regex"),
                                  case_insensitive = TRUE) {
    valuetype <- match.arg(valuetype)
    selection <- match.arg(selection)
    
    if (is.dictionary(pattern))
        pattern <- unique(unlist(as.list(pattern), use.names = FALSE))
    if (is.collocations(pattern))
        pattern <- pattern[["collocation"]]
    
    result <- 
        pattern2fixed(pattern = pattern, types = x, 
                      valuetype = valuetype, 
                      case_insensitive = case_insensitive) %>%
        unlist()

    if (selection == "remove") {
        result <- x[!(x %in% result)]
        if (!length(result)) 
            result <- character(0) # avoids named character(0)
    }
    
    result
}

#' @rdname char_select
#' @param ... additional arguments passed by `char_remove` and `char_keep` to
#'   `char_select`. Cannot include `selection`.
#' @export
#' @examples
#' # character removal
#' char_remove(letters[1:5], c("a", "c", "x"))
#' words <- c("any", "and", "Anna", "as", "announce", "but")
#' char_remove(words, "an*")
#' char_remove(words, "an*", case_insensitive = FALSE)
#' char_remove(words, "^.n.+$", valuetype = "regex")
#'
#' # remove some of the system stopwords
#' stopwords("en", source = "snowball")[1:6]
#' stopwords("en", source = "snowball")[1:6] %>%
#'   char_remove(c("me", "my*"))
#'   
char_remove <- function(x, ...) {
    if ("selection" %in% names(list(...))) {
        stop("char_remove cannot include selection argument")
    }
    char_select(x, ..., selection = "remove")
}

#' @rdname char_select
#' @export
#' @examples 
#' # character keep
#' char_keep(letters[1:5], c("a", "c", "x"))
char_keep <- function(x, ...) {
    if ("selection" %in% names(list(...))) {
        stop("char_keep cannot include selection argument")
    }
    char_select(x, ..., selection = "keep")
}
