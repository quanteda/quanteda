#' Recombine a dfm or fcm by combining identical dimension elements
#'
#' "Compresses" or groups a [dfm] or [fcm] whose dimension names are
#' the same, for either documents or features.  This may happen, for instance,
#' if features are made equivalent through application of a thesaurus.  It could also be needed after a
#' [cbind.dfm()] or [rbind.dfm()] operation.  In most cases, you will not
#' need to call `dfm_compress`, since it is called automatically by functions that change the
#' dimensions of the dfm, e.g. [dfm_tolower()].
#'
#' @param x input object, a [dfm] or [fcm]
#' @param margin character indicating on which margin to compress a dfm, either
#'   `"documents"`, `"features"`, or `"both"` (default).  For fcm
#'   objects, `"documents"` has no effect.
#' @inheritParams messages
#' @return `dfm_compress` returns a [dfm] whose dimensions have been
#'   recombined by summing the cells across identical dimension names
#'   ([docnames] or [featnames]).  The [docvars] will be
#'   preserved for combining by features but not when documents are combined.
#' @export
#' @examples
#' # dfm_compress examples
#' dfmat <- rbind(dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE),
#'                dfm(tokens("A C C C C C"), tolower = FALSE))
#' colnames(dfmat) <- char_tolower(featnames(dfmat))
#' dfmat
#' dfm_compress(dfmat, margin = "documents")
#' dfm_compress(dfmat, margin = "features")
#' dfm_compress(dfmat)
#'
#' # no effect if no compression needed
#' dfmatsubset <- dfm(tokens(data_corpus_inaugural[1:5]))
#' dim(dfmatsubset)
#' dim(dfm_compress(dfmatsubset))
#'
dfm_compress <- function(x, margin = c("both", "documents", "features"),
                         verbose = quanteda_options("verbose")) {
    UseMethod("dfm_compress")
}

#' @export
dfm_compress.default <- function(x,
                                 margin = c("both", "documents", "features"),
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_compress")
}

#' @export
dfm_compress.dfm <- function(x, margin = c("both", "documents", "features"),
                             verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    margin <- match.arg(margin)
    
    if (!nfeat(x) || !ndoc(x)) return(x)
    
    attrs <- attributes(x)
    documents <- features <- NULL
    if (margin %in% c("both", "documents"))
        documents <- factor(docnames(x), levels = unique(docnames(x)))
    if (margin %in% c("both", "features"))
        features <- factor(featnames(x), levels = unique(featnames(x)))
    
    if (verbose)
        before <- stats_dfm(x)
    x <- group_matrix(x, documents, features)
    result <- build_dfm(x, colnames(x),
                        unit = "documents",
                        docvars = group_docvars(attrs[["docvars"]], documents),
                        meta = attrs[["meta"]]
    )
    if (verbose)
        message_dfm("dfm_compress()", before, stats_dfm(result))
    return(result)
}
