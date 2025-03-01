#' Segment a corpus into chunks of a given size
#'
#' Segment a corpus into new documents of roughly equal sized text chunks, with
#' the possibility of overlapping the chunks.
#' @inheritParams tokens_chunk
#' @param inflation_factor numeric; the number of single default quanteda tokens
#'   required to produce a single chunked tokens. This is designed for chunking
#'   for input to LLMs, since LLM tokenizers (such as LLaMA's SentencePiece
#'   Byte-Pair Encoding tokenizer) require more tokens than the ;linguistically
#'   defined grammatically-based tokenizer that is the \pkg{quanteda} default.
#'   The recommended setting for this is 0.8, meaning that if size is 100, then
#'   the actual chunk size will be 100 / 0.8 = 125. Defaults to 1.0.
#' @seealso [tokens_chunk()]
#' @keywords corpus
#' @export
#' @examples
#' data_corpus_inaugural[1] |>
#'   corpus_chunk(size = 10)
#'
corpus_chunk <- function(x, size, overlap = 0, inflation_factor = 1.0,
                         use_docvars = TRUE,
                         verbose = quanteda_options("verbose")) {
    UseMethod("corpus_chunk")
}

#' @export
corpus_chunk.default <- function(x, size, overlap = 0, inflation_factor = 1.0,
                                 use_docvars = TRUE,
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "corpus_chunk")
}

#' @export
corpus_chunk.corpus <- function(x, size, overlap = 0, inflation_factor = 1.0,
                                use_docvars = TRUE,
                                verbose = quanteda_options("verbose")) {
    toks <- tokens(x)
    toks <- tokens_chunk(toks, size = ceiling(size / inflation_factor),
                         overlap = overlap,
                         use_docvars = use_docvars, verbose = verbose)
    corpus(
      tokens_to_char(toks),
      docvars = docvars(toks)
    )
}



# utility functions ----

tokens_to_char <- function(x) {
    sapply(as.list(x), collapse_char_vector)
}

collapse_char_vector <- function(x) {
    stringi::stri_c(x, collapse = " ") |>
        # remove space before punctuation (comma, period, etc.)
        stringi::stri_replace_all_regex(" ([,\\.?!;:])", "$1")
}
