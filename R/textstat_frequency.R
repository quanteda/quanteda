#' tabulate feature frequencies
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
#' dfm1 <- dfm(c("a a b b c d", "a d d d", "a a a"))
#' textstat_frequency(dfm1)
#' textstat_frequency(dfm1, groups = c("one", "two", "one"))
#' 
#' obamadfm <- 
#'     corpus_subset(data_corpus_inaugural, President == "Obama") %>%
#'     dfm(remove_punct = TRUE, remove = stopwords("english"))
#' freq <- textstat_frequency(obamadfm)
#' head(freq, 10)
#'
#' \donttest{
#' # plot 20 most frequent words
#' library("ggplot2")
#' ggplot(freq[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
#'     geom_point() + 
#'     coord_flip() +
#'     labs(x = NULL, y = "Frequency")
#' 
#' # plot relative frequencies by group
#' dfm_weight_pres <- data_corpus_inaugural %>% 
#'     corpus_subset(Year > 2000) %>% 
#'     dfm(remove = stopwords("english"), remove_punct = TRUE) %>% 
#'     dfm_weight(type = "relfreq")
#' 
#' # calculate relative frequency by president
#' freq_weight <- textstat_frequency(dfm_weight_pres, n = 10,
#'                                   groups = "President")
#' 
#' # plot frequencies
#' ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
#'     geom_point() +
#'     facet_wrap(~ group, scales = "free") +
#'     coord_flip() +
#'     scale_x_continuous(breaks = nrow(freq_weight):1,
#'                        labels = freq_weight$feature) +
#'     labs(x = NULL, y = "Relative frequency")
#' }
#' @export
#' @keywords plot
textstat_frequency <- function(x, n = NULL, groups = NULL) {
    UseMethod("textstat_frequency")
}
    
#' @rdname textplot_frequency
#' @noRd
#' @importFrom data.table data.table setorder setcolorder
#' @export
textstat_frequency.dfm <- function(x, n = NULL, groups = NULL) { 
    
    x <- as.dfm(x)
    group <- frequency <- NULL
  
    groupsadded <- FALSE
    if (is.null(groups)) {
        groupsadded <- TRUE
        groups <- rep("all", ndoc(x))
    }
    
    # get document frequency, override weight check
    x_docfreq <- x
    x_docfreq@weightTf[["scheme"]] <- "count"
    x_docfreq <- dfm_group(tf(x_docfreq, "boolean"), groups)
    
    x <- dfm_group(x, groups)
    ngroups <- ndoc(x)
    
    
    x_dgt <- as(x, "dgTMatrix")
    result <- data.table(feature = featnames(x)[x_dgt@j+1],
                         frequency = x_dgt@x,
                         docfreq = x_docfreq@x,
                         group = docnames(x)[x_dgt@i+1])
    setorder(result, group, -frequency)
    # result[, ngrp := ifelse(is.null(n), .N, min(c(n, .N))), by = group]
    result[, rank := 1:.N, by = group]
    # result[, ngrp := NULL]
    setcolorder(result, c("feature", "frequency", "rank", "docfreq", "group"))
    if (groupsadded) result[, group := NULL]
    if (!is.null(n)) result <- result[rank <= n]
    as.data.frame(result)
}
