#' Segment a corpus into chunks of a given size
#'
#' Segment a corpus into new documents of roughly equal sized text chunks, with
#' the possibility of overlapping the chunks.
#' @param size integer; the (approximate) token length of the chunks. See
#'   Details.
#' @param truncate logical; if `TRUE`, truncate the text after `size`
#' @inheritParams tokens_chunk
#' @details
#' The token length is estimated using `stringi::stri_length(txt) /
#' stringi::stri_count_boundaries(txt)` to avoid needing to tokenize and rejoin
#' the corpus from the tokens.
#'
#' Note that when used for chunking texts prior to sending to large language
#' models (LLMs) with limited input token lengths, size should typically be set
#' to approximately 0.75-0.80 of the LLM's token limit.  This is because
#' tokenizers (such as LLaMA's SentencePiece Byte-Pair Encoding tokenizer)
#' require more tokens than the linguistically defined grammatically-based
#' tokenizer that is the \pkg{quanteda} default. Note also that because of the
#' use of `stringi::stri_count_boundaries(txt)` to approximate token length
#' (efficiently), the exact token length for chunking will be approximate.
#' @seealso [tokens_chunk()]
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
  if (verbose)
      before <- stats_corpus(x)
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
      message_corpus("corpus_chunk()", before, stats_corpus(result))
  return(result)
}
