#' Split corpus text into chunks
#'
#' Wrap each document's text into chunks of roughly equal length and return
#' the chunks as documents in a corpus.
#' @param x a [corpus] object
#' @param size integer giving the approximate number of tokens per chunk.
#'   See Details.
#' @param truncate if `TRUE`, keep only the first chunk of each document
#' @inheritParams tokens_chunk
#' @return A [corpus] with one document per chunk.
#' @details
#' Each document is wrapped with `stringi::stri_wrap()`. The wrap width is
#' `size` multiplied by the average number of characters per boundary segment
#' in the document, estimated using `stringi::stri_length()` and
#' `stringi::stri_count_boundaries()`.
#'
#' Note that when used for chunking texts prior to sending to large language
#' models (LLMs) with limited input token lengths, size should typically be set
#' to approximately 0.75-0.80 of the LLM's token limit.  This is because
#' tokenizers (such as LLaMA's SentencePiece Byte-Pair Encoding tokenizer)
#' require more tokens than the linguistically defined grammatically-based
#' tokenizer that is the \pkg{quanteda} default.
#' @seealso [tokens_chunk()] for chunking by token count with optional overlap.
#' @keywords corpus
#' @importFrom lifecycle signal_stage
#' @export
#' @examples
#' data_corpus_inaugural[1] |>
#'   corpus_chunk(size = 10)
#'
corpus_chunk <- function(x, size,
                         truncate = FALSE,
                         use_docvars = TRUE,
                         verbose = quanteda_options("verbose")) {
  signal_stage("experimental", "corpus_chunk()")
  UseMethod("corpus_chunk")
}

#' @export
corpus_chunk.default <- function(x, size,
                                 truncate = FALSE,
                                 use_docvars = TRUE,
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "corpus_chunk")
}

#' @export
#' @importFrom stringi stri_length stri_count_boundaries stri_wrap
corpus_chunk.corpus <- function(x, size,
                                truncate = FALSE,
                                use_docvars = TRUE,
                                verbose = quanteda_options("verbose")) {

  size <- check_integer(size, min = 0)
  truncate <- check_logical(truncate)
  verbose <- check_logical(verbose)

  if (!use_docvars)
    docvars(x) <- NULL

  attrs <- attributes(x)
  n <- stri_count_boundaries(x)
  n[n == 0] <- 1L
  avg <- stri_length(x) / n # average token length

  lis <- lapply(seq_along(x), function(i) {
    if (truncate) {
      head(stri_wrap(x[[i]], width = size * avg[i]), 1)
    } else {
      stri_wrap(x[[i]], width = size * avg[i])
    }
  })

  attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]],
                                        rep(seq_along(lis), lengths(lis)),
                                        drop_docid = FALSE)
  result <- rebuild_corpus(unlist(lis, use.names = FALSE), attrs)
  if (verbose)
      message_corpus("corpus_chunk()", stats_corpus(result))
  return(result)
}
