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
#' txt <- c(doc1 = "Tweet https://quanteda.io using @quantedainit and #rstats.",
#'          doc2 = "The £1,000,000 question.",
#'          doc3 = "毎日 #quanteda を使用してください！",
#'          doc4 = "Line 1.\nLine2\n\nLine3.")
#' tokenize_default(txt)
#' tokenize_fasterword(txt)
#' tokenize_fastest_word(txt)
#' tokenize_sentence(txt)
#' tokenize_character(txt[2])
NULL

# default tokenizer ----------

#' @rdname tokenize_internal
#' @export
tokenize_default <- function(x) {
    stri_split_boundaries(x, type = "word")
}

# legacy tokenizers ----------

#' @rdname tokenize_internal
#' @export
tokenize_word <- tokenize_default  # change later to legacy method

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
