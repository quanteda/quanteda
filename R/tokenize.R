#' quanteda default tokenizer
#' 
#' Default \pkg{quanteda} tokenizer to implement full set of options, for a
#' character or [corpus] input.
#' @param x a character or [corpus] object
#' @inheritParams tokens
#' @param ... not used
#' @return a (uniquely) named list of characters
#' @keywords tokens
#' @importFrom stringi stri_split_regex stri_detect_regex stri_detect_charclass
#'   stri_split_boundaries stri_subset_regex stri_split_fixed
#' @export
#' @examples
#' txt <- c(doc1 = "Tweet https://quanteda.io using @quantedainit and #rstats.",
#'          doc2 = "The £1,000,000 question.",
#'          doc3 = "毎日#quantedaを使用してください！")
#' tokenize(txt)
#' tokenize(txt, remove_symbols = TRUE, remove_punct = TRUE)
#' tokenize(txt, remove_symbols = TRUE, remove_punct = TRUE, remove_url = TRUE)
#' tokenize(txt, remove_symbols = TRUE, remove_punct = TRUE, preserve_tags = TRUE)
#' tokenize(txt, remove_symbols = FALSE, remove_punct = TRUE, remove_numbers = TRUE)
tokenize <- function(x,
                     remove_punct = FALSE,
                     remove_symbols = FALSE,
                     remove_numbers = FALSE,
                     remove_url = FALSE,
                     preserve_tags = !remove_punct,
                     split_infix_hyphens = FALSE, ...) {
    UseMethod("tokenize")
}

#' @rdname tokenize
#' @noRd
#' @export
tokenize.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "tokenize"))
}

#' @rdname tokenize
#' @noRd
#' @export
tokenize.corpus <- function(x, ...) {
    tokenize(texts(x), ...)
}

#' @rdname tokenize
#' @noRd
#' @export
tokenize.character <- function(x, 
                               remove_punct = FALSE,
                               remove_symbols = FALSE,
                               remove_numbers = FALSE,
                               remove_url = FALSE,
                               preserve_tags = !remove_punct,
                               split_infix_hyphens = FALSE,
                               ...) {
    
    check_dots(list(...), names(formals("tokenize")))
    check_input(x)
    named <- names(x)

    # initial split on white space
    out <- stri_split_regex(x, "[\\p{Z}\\p{C}]+")

    # get document indexes to vectorize tokens
    doc_lengths <- cumsum(lengths(out))
    docindex <- c(0, doc_lengths)
    # convert the list into a vector - avoids all those mapplys
    out <- unlist(out)

    # intialize index vectors
    index_symbols <- index_tag <- index_urls <- index_infixhyphens <- 
        rep(FALSE, length(out))
    # initialize regex for removals
    regex_to_remove <- character()

    if (!split_infix_hyphens) {
        # anything with an interior hyphen
        index_infixhyphens <- stri_detect_regex(out, ".+\\p{Pd}.+")
    }
    
    if (preserve_tags) {
        # get the index of social media #hashtags and @usernames
        index_tag <- stri_detect_regex(out, "^#[A-Za-z]+\\w*|^@\\w+")
    }

    if (!remove_symbols) {
        # anything with a symbol not also a tag
        index_symbols <- stri_detect_charclass(out, "\\p{S}") && !index_tag
    }

    # handle URLs and email addresses
    index_url <- stri_detect_regex(out, "^((https{0,1}|s{0,1}ftp)://)|(\\w+@\\w+)")
    if (remove_url) {
        out[index_url] <- ""
        regex_to_remove = c(regex_to_remove, "^$") # remove the blanks later
    }

    # apply ICU word tokenizer to everything not indexed for preservation
    index_to_split <- !(index_url | index_tag | index_symbols | index_infixhyphens)
    out[index_to_split] <- stri_split_boundaries(out[index_to_split], type = "word")
    
    # convert the vector back to a list
    out <- split(out,
                 cut(
                   seq_along(out),
                   docindex,
                   include.lowest = FALSE,
                   labels = named
                 ))
    # handle nested lists from 2nd stage stri_split_boundaries() call
    out <- lapply(out, unlist)

    # final removals
    if (remove_punct)
        regex_to_remove <- c(regex_to_remove, "^\\p{P}$")
    if (remove_symbols)
        regex_to_remove <- c(regex_to_remove, "^\\p{S}$")
    if (remove_numbers)
        regex_to_remove <- c(regex_to_remove, "^\\p{Sc}{0,1}(\\p{N}+[,.]{0,1}\\p{N}+)+\\p{Sc}{0,1}$")
    if (length(regex_to_remove) > 0)
        out <- lapply(out, function(toks) 
            stri_subset_regex(toks, paste(regex_to_remove, collapse = "|"), negate = TRUE))

    # reattach names
    names(out) <- named

    out
}

# internal tokenizers for legacy handling of what argument ----------

#' @rdname tokenize
#' @importFrom tokenizers tokenize_characters
tokenize_character <- function(x) {
    tokenizers::tokenize_characters(x, strip_non_alphanum = FALSE, simplify = FALSE)
}

#' @rdname tokenize
#' @importFrom tokenizers tokenize_sentences
tokenize_sentence <- function(x) {
    tokenizers::tokenize_sentences(x)
}

#' @rdname tokenize
tokenize_fasterword <- function(x) {
    stringi::stri_split_regex(x, "[\\p{Z}\\p{C}]+")
}

tokenize_fastestword <- function(x) {
    stringi::stri_split_fixed(x, " ")
}


# utility functions ----------

check_input <- function(x) {
  check_character <- is.character(x) |
  if (is.list(x)) {
       check_list <- all(vapply(x, is.character, logical(1))) &
         all(vapply(x, length, integer(1)) == 1L)
  } else {
    check_list <- FALSE
  }
  if (!(check_character | check_list))
    stop("Input must be a character vector of any length or a list of character\n",
         "  vectors, each of which has a length of 1.")
}
