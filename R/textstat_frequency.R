#' Tabulate feature frequencies
#' 
#' Produces counts and document frequencies summaries of the features in a
#' \link{dfm}, optionally grouped by a \link{docvars} variable or other supplied
#' grouping variable.
#' @param x a \link{dfm} object
#' @param n (optional) integer specifying the top \code{n} features to be returned,
#' within group if \code{groups} is specified
#' @inheritParams groups
#' @return a data.frame containing the following variables:
#' \describe{
#' \item{\code{feature}}{(character) the feature}
#' \item{\code{frequency}}{count of the feature}
#' \item{\code{rank}}{rank of the feature, where 1 indicates the greatest
#' frequency}
#' \item{\code{docfreq}}{document frequency of the feature, as a count (the
#' number of documents in which this feature occurred at least once)}
#' \item{\code{docfreq}}{document frequency of the feature, as a count}
#' \item{\code{group}}{(only if \code{groups} is specified) the label of the group.
#' If the features have been grouped, then all counts, ranks, and document
#' frequencies are within group.  If groups is not specified, the \code{group}
#' column is omitted from the returned data.frame.}
#' }
#' @examples 
#' dfmat1 <- dfm(c("a a b b c d", "a d d d", "a a a"))
#' textstat_frequency(dfmat1)
#' textstat_frequency(dfmat1, groups = c("one", "two", "one"))
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
#' tstat2 <- textstat_frequency(dfmat3, n = 10,
#'                                   groups = "President")
#' 
#' # plot frequencies
#' ggplot(data = tstat2, aes(x = nrow(tstat2):1, y = frequency)) +
#'     geom_point() +
#'     facet_wrap(~ group, scales = "free") +
#'     coord_flip() +
#'     scale_x_continuous(breaks = nrow(tstat2):1,
#'                        labels = tstat2$feature) +
#'     labs(x = NULL, y = "Relative frequency")
#' }
#' @return \code{textstat_frequency} returns a data.frame of features and
#'   their term and document frequencies within groups.
#' @export
#' @keywords plot
textstat_frequency <- function(x, n = NULL, groups = NULL) {
    UseMethod("textstat_frequency")
}
    
#' @export
textstat_frequency.default <- function(x, n = NULL, groups = NULL) { 
    stop(friendly_class_undefined_message(class(x), "textstat_frequency"))
}

#' @importFrom data.table data.table setorder setcolorder
#' @export
textstat_frequency.dfm <- function(x, n = NULL, groups = NULL) { 
    
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    
    if (is.null(groups)) groups <- rep("all", ndoc(x))
    x@weightTf[["scheme"]] <- "count" # reset for docfreq
    docfreq <- dfm_group(dfm_weight(x, "boolean"), groups)@x
    x <- as(dfm_group(x, groups), "dgTMatrix")
    temp <- data.table(feature = colnames(x)[x@j + 1],
                       frequency = x@x,
                       docfreq = docfreq,
                       group = rownames(x)[x@i + 1])
    
    group <- frequency <- NULL
    setorder(temp, group, -frequency)
    temp[, rank := 1:.N, by = group]
    if (!is.null(n)) temp <- temp[rank <= n]
    
    result <- data.frame(feature = temp$feature,
                         frequency = temp$frequency,
                         rank = temp$rank,
                         docfreq = temp$docfreq,
                         group = temp$group,
                         stringsAsFactors = FALSE)
    class(result) <- c('frequency', 'textstat', 'data.frame')
    rownames(result) <- as.character(seq_len(nrow(result)))
    return(result)
}
