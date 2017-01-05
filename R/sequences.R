

#' find sequences of tokens
#' 
#' This function automatically identify sequences of tokens. This algorithm is 
#' based on Blaheta and Johnson's “Unsupervised Learning of Multi-Word Verbs”.
#' @param x tokenizedTexts objects
#' @param tokens types of token in sequuences
#' @param count_min minimum frequency of sequences
#' @param nested collect nested sub-sequence
#' @examples 
#' sents <- as.character(tokens(data_corpus_inaugural[1:10], what = "sentence"))
#' tokens <- tokens(sents, removePunct = TRUE)
#' tokens <- tokens_select(tokens, stopwords("english"), "remove", padding = TRUE)
#' types <- unique(as.character(tokens))
#' 
#' # extracting multi-part nouns
#' types_upper <- types[stringi::stri_detect_regex(types, "^([A-Z][a-z\\-]{2,})")]
#' seqs <- sequences(tokens, types_upper, count_min = 2)
#' head(seqs, 10)
#' 
#' # types can be any words
#' types_lower <- types[stringi::stri_detect_regex(types, "^([a-z]+)$") & !types %in%stopwords()]
#' seqs2 <- sequences(tokens, types_lower, count_min = 3)
#' head(seqs2, 10)
#' 
#' @keywords internal collocations
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). Unsupervised learning of
#'   multi-word verbs. Presented at the ACLEACL Workshop on the Computational
#'   Extraction, Analysis and Exploitation of Collocations.
#' @export
sequences <- function(x, tokens, features, valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE, count_min = 2, nested=TRUE, ...) {
    
    valuetype <- match.arg(valuetype)
    
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    types <- attr(x, 'types')
    features <- unlist(features, use.names = FALSE)
    features_fixed <- regex2fixed5(features, types, valuetype, case_insensitive, FALSE) # convert glob or regex to fixed
    features_id <- lapply(features_fixed, function(x) fmatch(x, types))
    
    seqs <- qutd_cpp_sequences(toks, features_id, count_min, nested)
    seqs$z <- seqs$lambda / seqs$sigma
    seqs$p <- 1 - stats::pnorm(seqs$z)
    seqs$seqences <- stringi::stri_c_list(out2$sequence, ' ')
    
    df <- as.data.frame(seqs)
    class(df) <- "sequences"
    
    return(df)
}
