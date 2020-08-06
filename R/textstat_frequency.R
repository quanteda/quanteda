#' Tabulate feature frequencies
#'
#' Produces counts and document frequencies summaries of the features in a
#' [dfm], optionally grouped by a [docvars] variable or other supplied
#' grouping variable.
#' @param x a [dfm] object
#' @param n (optional) integer specifying the top `n` features to be returned,
#' within group if `groups` is specified
#' @param ties_method character string specifying how ties are treated.  See
#'   [data.table::frank()] for details.  Unlike that function,
#'   however, the default is `"min"`, so that frequencies of 10, 10, 11
#'   would be ranked 1, 1, 3.
#' @param ... additional arguments passed to [dfm_group()].  This can
#'   be useful in passing `force = TRUE`, for instance, if you are grouping a
#'   dfm that has been weighted.
#' @inheritParams groups
#' @return a data.frame containing the following variables:
#' \describe{
#' \item{`feature`}{(character) the feature}
#' \item{`frequency`}{count of the feature}
#' \item{`rank`}{rank of the feature, where 1 indicates the greatest
#' frequency}
#' \item{`docfreq`}{document frequency of the feature, as a count (the
#' number of documents in which this feature occurred at least once)}
#' \item{`docfreq`}{document frequency of the feature, as a count}
#' \item{`group`}{(only if `groups` is specified) the label of the group.
#' If the features have been grouped, then all counts, ranks, and document
#' frequencies are within group.  If groups is not specified, the `group`
#' column is omitted from the returned data.frame.}
#' }
#' @examples
#' set.seed(20)
#' dfmat1 <- dfm(c("a a b b c d", "a d d d", "a a a"))
#' textstat_frequency(dfmat1)
#' textstat_frequency(dfmat1, groups = c("one", "two", "one"), ties_method = "first")
#' textstat_frequency(dfmat1, groups = c("one", "two", "one"), ties_method = "dense")
#'
#' dfmat2 <- corpus_subset(data_corpus_inaugural, President == "Obama") %>%
#'    dfm(remove_punct = TRUE, remove = stopwords("english"))
#' tstat1 <- textstat_frequency(dfmat2)
#' head(tstat1, 10)
#'
#' \donttest{
#' # plot 20 most frequent words
#' library("ggplot2")
#' ggplot(tstat1[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
#'     geom_point() +
#'     coord_flip() +
#'     labs(x = NULL, y = "Frequency")
#'
#' # plot relative frequencies by group
#' dfmat3 <- data_corpus_inaugural %>%
#'     corpus_subset(Year > 2000) %>%
#'     dfm(remove = stopwords("english"), remove_punct = TRUE) %>%
#'     dfm_group(groups = "President") %>%
#'     dfm_weight(scheme = "prop")
#'
#' # calculate relative frequency by president
#' tstat2 <- textstat_frequency(dfmat3, n = 10, groups = "President")
#'
#' # plot frequencies
#' ggplot(data = tstat2, aes(x = factor(nrow(tstat2):1), y = frequency)) +
#'     geom_point() +
#'     facet_wrap(~ group, scales = "free") +
#'     coord_flip() +
#'     scale_x_discrete(breaks = nrow(tstat2):1,
#'                        labels = tstat2$feature) +
#'     labs(x = NULL, y = "Relative frequency")
#' }
#' @return `textstat_frequency` returns a data.frame of features and
#'   their term and document frequencies within groups.
#' @export
#' @keywords plot
textstat_frequency <- function(x, n = NULL, groups = NULL,
                               ties_method = c("min", "average", "first", "random", "max", "dense"),
                               ...) {
    UseMethod("textstat_frequency")
}

#' @export
textstat_frequency.default <- function(x, n = NULL, groups = NULL,
                               ties_method = c("min", "average", "first", "random", "max", "dense"),
                               ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_frequency"))
}

#' @importFrom data.table data.table setcolorder setorder frank := .SD setDF
#' @export
textstat_frequency.dfm <- function(x, n = NULL, groups = NULL,
                               ties_method = c("min", "average", "first", "random", "max", "dense"),
                               ...) {
    group <- frequency <- NULL
    ties_method <- match.arg(ties_method)
    x <- as.dfm(x)

    if (!sum(x))
        stop(message_error("dfm_empty"))

    if (is.null(groups))
        groups <- rep("all", ndoc(x))

    tf <- x
    tf <- dfm_group(tf, groups, ...)
    tf <- as(tf, "dgTMatrix")

    df <- dfm_weight(x, "boolean", force = TRUE)
    df <- dfm_group(df, groups, ...)
    df <- as(df, "dgTMatrix")

    result <- data.table(
        feature = colnames(tf)[tf@j + 1L],
        frequency = tf@x,
        docfreq = df@x,
        group = rownames(tf)[tf@i + 1L]
    )

    # get the frequency rank
    result[, rank := frank(-frequency, ties.method = ties_method), by = group]
    setorder(result, group, rank)

    # keep only first n items by group, if n is specified
    if (!is.null(n)) {
        stopifnot(is.numeric(n))
        result <- result[, head(.SD, n), by = group]
    }

    setcolorder(result, c("feature", "frequency", "rank", "docfreq", "group"))

    # make into data.frame, class it, and return
    result <- setDF(result)
    class(result) <- c("frequency", "textstat", "data.frame")
    rownames(result) <- as.character(seq_len(nrow(result)))
    return(result)
}
