#' Segment a corpus into chunks of a given size
#'
#' Segment a corpus into new documents of roughly equal sized text chunks, with
#' the possibility of overlapping the chunks.
#' @inheritParams tokens_chunk
#' @param size integer; the (approximate) token length of the chunks. See
#'   Details.
#' @param truncate logical; if `TRUE`, truncate the text after `size`
#' @param inflation_factor numeric; the number of single default quanteda tokens
#'   required to produce a single chunked token. This is designed for chunking
#'   for input to LLMs, since LLM tokenizers (such as LLaMA's SentencePiece
#'   Byte-Pair Encoding tokenizer) require more tokens than the ;linguistically
#'   defined grammatically-based tokenizer that is the \pkg{quanteda} default.
#'   The recommended setting for this is 0.8, meaning that if size is 100, then
#'   the actual chunk size will be 100 / 0.8 = 125. Defaults to 1.0.
#' @details
#' The token length is estimated using `stringi::stri_length(txt) /
#' stringi::stri_count_boundaries(txt)` to avoid needing to tokenize and rejoin
#' the corpus from the tokens. Combined with `inflation_factor`, this means
#' that the exact token length for chunking will be approximate.
#' @seealso [tokens_chunk()]
#' @keywords corpus
#' @importFrom lifecycle signal_stage
#' @export
#' @examples
#' data_corpus_inaugural[1] |>
#'   corpus_chunk(size = 10)
#'
corpus_chunk <- function(x, size, overlap = 0, inflation_factor = 1.0,
                         truncate = FALSE,
                         use_docvars = TRUE,
                         verbose = quanteda_options("verbose")) {
  signal_stage("experimental", "corpus_chunk()")
  UseMethod("corpus_chunk")
}

#' @export
corpus_chunk.default <- function(x, size, overlap = 0, inflation_factor = 1.0,
                                 truncate = FALSE,
                                 use_docvars = TRUE,
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "corpus_chunk")
}

#' @export
#' @importFrom stringi stri_length stri_count_boundaries stri_wrap
corpus_chunk.corpus <- function(x, size, overlap = 0, inflation_factor = 1.0,
                                truncate = FALSE,
                                use_docvars = TRUE,
                                verbose = quanteda_options("verbose")) {
  if (!use_docvars) {
    docvars(x) <- NULL
  }
  attrs <- attributes(x)

  mean_tok_nchar <- stri_length(x) / stri_count_boundaries(x)

  lis <- lapply(seq_along(x), function(i) {
    if (truncate) {
      head(stri_wrap(x[[i]], width = size / inflation_factor * mean_tok_nchar[i]), 1)
    } else {
      stri_wrap(x[[i]], width = size / inflation_factor * mean_tok_nchar[i])
    }
  })

  attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]],
                                        rep(seq_along(lis), lengths(lis)),
                                        drop_docid = FALSE)
  result <- rebuild_corpus(unlist(lis, use.names = FALSE), attrs)

  return(result)
}
