# man page ----------

#' quanteda tokenizers
#' 
#' Internal methods for tokenization providing default and legacy methods for  
#' text segmentation.
#' @name tokenize_internal
#' @rdname tokenize_internal
#' @aliases tokenize
#' @param x (named) character; input texts
#' @return a list of characters corresponding to the (most conservative) tokens,
#'   including whitespace where applicable.
#' @keywords tokens internal
#' @importFrom stringi stri_split_boundaries
#' @examples
#' \dontrun{
#' txt <- c(doc1 = "Tweet https://quanteda.io using @quantedainit and #rstats.",
#'          doc2 = "The £1,000,000 question.",
#'          doc3 = "毎日 #quanteda を使用してください！",
#'          doc4 = "Line 1.\nLine2\n\nLine3.",
#'          doc5 = "?",
#'          doc6 = "Self-aware machines! \U0001f600")
#' tokenize_default(txt)
#' tokenize_default(txt, remove_punct = TRUE, split_currency = TRUE)
#' tokenize_fasterword(txt)
#' tokenize_fastestword(txt)
#' tokenize_sentence(txt)
#' tokenize_character(txt[2])
#' }
NULL

# default tokenizer ----------

#' @rdname tokenize_internal
#' @inheritParams tokens
#' @importFrom stringi stri_split_regex stri_detect_regex stri_detect_charclass
#' @export
tokenize_default <- function(x, 
                             remove_punct = FALSE,
                             remove_symbols = FALSE,
                             remove_numbers = FALSE,
                             remove_url = FALSE,
                             remove_separators = FALSE,
                             split_tags = FALSE,
                             split_hyphens = FALSE,
                             split_currency = FALSE,
                             verbose = quanteda_options("verbose"),
                             ...) {
    
    if (verbose) catm("Starting tokenization...\n")
    time_start <- proc.time()
    
    check_dots(list(...), names(formals("tokenize_default")))
    named <- names(x)

    # initial split on white space
    if (verbose) catm("...segmenting tokens\n")
    out <- stri_split_regex(x, "[\\p{Z}\\p{C}]+")

    # get document indexes to vectorize tokens
    if (verbose) catm("...vectorizing tokens\n")
    doc_lengths <- cumsum(lengths(out))
    docindex <- c(0, doc_lengths)
    # convert the list into a vector - avoids all those mapplys
    out <- unlist(out)

    # set index vectors for tokens to skip in 2nd stage
    index_tag <- index_hyphen <- index_currency <- rep(FALSE, length(out))
    if (verbose) catm("...preserving tokens not to split\n")
    index_url <- stri_detect_regex(out, "^((https{0,1}|s{0,1}ftp)://)|(\\w+@\\w+)")
    if (!split_tags)
        index_tag <- stri_detect_regex(out, "^#[A-Za-z]+\\w*|^@\\w+")
    if (!split_hyphens)
        index_hyphen <- stri_detect_regex(out, ".+\\p{Pd}.+")
    if (!split_currency)    
        index_currency <- stri_detect_regex(out, "^\\p{Sc}\\p{N}+|\\p{N}+\\p{Sc}$")
    
    # apply ICU word tokenizer to everything not indexed for preservation
    if (verbose) catm("...applying second stage smart tokenization\n")
    index_to_split <- !(index_tag | index_hyphen | index_url | index_currency)
    out[index_to_split] <- stri_split_boundaries(out[index_to_split], type = "word")
    
    if (remove_url) {
        if (verbose) catm("...removing URLs\n")  
        if (sum(index_url)) out[index_url] <- ""
    }

    # removals
    regex_to_remove <- character()
    removing_msg <- character()
    if (remove_punct) {
        regex_to_remove <- c(regex_to_remove, "^\\p{P}$")
        removing_msg <- c(removing_msg, "punctuation")
    }
    if (remove_symbols) {
        regex_to_remove <- c(regex_to_remove, "^\\p{S}$")
        removing_msg <- c(removing_msg, "symbols")
    }
    if (remove_numbers) {
        regex_to_remove <- c(regex_to_remove, "(^\\p{N}{1,3}([.,]\\p{N}{3})*([.,]\\p{N}+)?$)")
        removing_msg <- c(removing_msg, "numbers")
    }
    if (length(regex_to_remove)) {
        if (verbose) catm("...removing", paste(removing_msg, collapse = ", "), "\n")
        out[index_to_split] <- lapply(out[index_to_split], stri_subset_regex, 
                                      paste(regex_to_remove, collapse = "|"), negate = TRUE)
    }
      
    # convert the vector back to a list
    if (verbose) catm("...tidying up\n")
    out <- split(out, cut(seq_along(out), docindex, include.lowest = FALSE, labels = named))
    # handle nested lists from 2nd stage stri_split_boundaries() call
    out <- lapply(out, unlist)

    # remove any blanks (from removing URLs)
    out <- lapply(out, function(toks) toks[toks != ""])
    
    # reattach names
    names(out) <- named
    
    if (verbose) {
      catm("...total elapsed: ",
           format((proc.time() - time_start)[3], digits = 3), "seconds.\n")
      catm("Finished tokenizing ", format(length(x), big.mark = ","), " text",
           if (length(x) > 1) "s", ".\n", sep = "")
    }
    
    out
}

# legacy tokenizers ----------

#' @rdname tokenize_internal
#' @export
tokenize_word <- function(x) {
    stri_split_boundaries(x, type = "word")
}

#' @rdname tokenize_internal
#' @importFrom tokenizers tokenize_characters
#' @export
tokenize_character <- function(x) {
    tokenizers::tokenize_characters(x, strip_non_alphanum = FALSE, simplify = FALSE)
}

#' @rdname tokenize_internal
#' @importFrom tokenizers tokenize_sentences
#' @export
tokenize_sentence <- function(x) {
    tokenizers::tokenize_sentences(x)
}

#' @rdname tokenize_internal
#' @importFrom stringi stri_split_regex
#' @export
tokenize_fasterword <- function(x) {
    stri_split_regex(x, "[\\p{Z}\\p{C}]+")
}

#' @rdname tokenize_internal
#' @importFrom stringi stri_split_regex
#' @export
tokenize_fastestword <- function(x) {
    stri_split_regex(x, " ")
}
