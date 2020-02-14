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
#' @return `dfm_compress` returns a [dfm] whose dimensions have been
#'   recombined by summing the cells across identical dimension names
#'   ([docnames] or [featnames]).  The [docvars] will be
#'   preserved for combining by features but not when documents are combined.
#' @export
#' @examples
#' # dfm_compress examples
#' dfmat <- rbind(dfm(c("b A A", "C C a b B"), tolower = FALSE),
#'              dfm("A C C C C C", tolower = FALSE))
#' colnames(dfmat) <- char_tolower(featnames(dfmat))
#' dfmat
#' dfm_compress(dfmat, margin = "documents")
#' dfm_compress(dfmat, margin = "features")
#' dfm_compress(dfmat)
#'
#' # no effect if no compression needed
#' dfmatsubset <- dfm(data_corpus_inaugural[1:5])
#' dim(dfmatsubset)
#' dim(dfm_compress(dfmatsubset))
#'
dfm_compress <- function(x, margin = c("both", "documents", "features")) {
    UseMethod("dfm_compress")
}

#' @export
dfm_compress.default <- function(x,
                                 margin = c("both", "documents", "features")) {
    stop(friendly_class_undefined_message(class(x), "dfm_compress"))
}

#' @export
dfm_compress.dfm <- function(x, margin = c("both", "documents", "features")) {

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(x)
    margin <- match.arg(margin)
    documents <- features <- NULL
    if (margin %in% c("both", "documents"))
        documents <- factor(docnames(x), levels = unique(docnames(x)))
    if (margin %in% c("both", "features"))
        features <- factor(featnames(x), levels = unique(featnames(x)))
    group_dfm(x, documents, features)
}
