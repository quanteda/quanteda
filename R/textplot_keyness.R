#' Plot word keyness
#'
#' Plot the results of a "keyword" of features comparing their differential
#' associations with a target and a reference group, after calculating keyness
#' using [textstat_keyness()].
#' @param x a return object from [textstat_keyness()]
#' @param show_reference logical; if `TRUE`, show key reference features in
#'   addition to key target features
#' @param show_legend logical; if `TRUE`, show legend
#' @param n integer; number of features to plot
#' @param min_count numeric; minimum total count of feature across the target
#'   and reference categories, for a feature to be included in the plot
#' @param margin numeric; size of margin where feature labels are shown
#' @param color character or integer; colors of bars for target and reference documents.
#'   `color` must have two elements when `show_reference = TRUE`.  See
#'   \link[ggplot2:aes_colour_fill_alpha]{ggplot2::color}.
#' @param labelcolor character; color of feature labels.
#' @param labelsize numeric; size of feature labels and bars.  See
#'   \link[ggplot2:aes_linetype_size_shape]{ggplot2::size}.
#' @param font character; font-family of texts. Use default font if `NULL`.
#' @return a \pkg{ggplot2} object
#' @export
#' @author Haiyan Wang and Kohei Watanabe
#' @seealso [textstat_keyness()]
#' @keywords textplot
#' @examples
#' # compare Trump speeches to other Presidents by chi^2
#' dfmat1 <- data_corpus_inaugural %>%
#'      corpus_subset(Year > 1980) %>%
#'      dfm(groups = "President", remove = stopwords("english"), remove_punct = TRUE)
#' tstat1 <- textstat_keyness(dfmat1, target = "Trump")
#' textplot_keyness(tstat1, margin = 0.2, n = 10)
#'
#' # compare contemporary Democrats v. Republicans
#' corp <- data_corpus_inaugural %>%
#'     corpus_subset(Year > 1960)
#' docvars(corp, "party") <-
#'     ifelse(docvars(corp, "President") %in% c("Nixon", "Reagan", "Bush", "Trump"),
#'            "Republican", "Democrat")
#' dfmat2 <- dfm(corp, groups = "party", remove = stopwords("english"),
#'                 remove_punct = TRUE)
#' tstat2 <- textstat_keyness(dfmat2, target = "Democrat", measure = "lr")
#' textplot_keyness(tstat2, color = c("blue", "red"), n = 10)
#'
textplot_keyness <-  function(x, show_reference = TRUE, show_legend = TRUE,
                              n = 20L, min_count = 2L, margin = 0.05,
                              color = c("darkblue", "gray"), labelcolor = "gray30",
                              labelsize = 4, font = NULL) {
    UseMethod("textplot_keyness")
}

#' @export
textplot_keyness.default <- function(x, show_reference = TRUE, show_legend = TRUE,
                                     n = 20L, min_count = 2L, margin = 0.05,
                                     color = c("darkblue", "gray"), labelcolor = "gray30",
                                     labelsize = 4, font = NULL) {
    stop(friendly_class_undefined_message(class(x), "textplot_keyness"))
}

#' @import ggplot2
#' @export
textplot_keyness.keyness <- function(x, show_reference = TRUE, show_legend = TRUE,
                                     n = 20L, min_count = 2L, margin = 0.05,
                                     color = c("darkblue", "gray"), labelcolor = "gray30",
                                     labelsize = 4, font = NULL) {

    font <- check_font(font)
    color <- as.factor(color)
    if (show_reference) {
        if (length(color) > 2) {
            color <- color[1:2]
        } else if (length(color) == 1) {
            color <- rep(color, 2)
        }
    }

    # extract attribute befor subsetting
    measure <- colnames(x)[2]

    # drop infrequent words
    data <- x[(x$n_target + x$n_reference) >= min_count, , drop = FALSE]

    if (nrow(data) < 1)
        stop("Too few words in the documents.")

    data$keyness <- data[[2]]
    data$right <- data$keyness >= 0
    if (show_reference) {
        t <- intersect(which(data$right), head(seq(nrow(data)), n))
        r <- intersect(which(!data$right), tail(seq(nrow(data)), n))
        i <- union(t, r)
    } else {
        i <- intersect(which(data$right), head(seq(nrow(data)), n))
    }
    data <- data[i, , drop = FALSE]
    if (show_reference) {
        group <- attr(x, "groups")
        data$color <- color[2 - data$right]
    } else {
        data$color <- color[1]
        color <- group <- NULL
    }

    data$x1 <- ifelse(data$right, abs(data$keyness), abs(data$keyness) * -1)
    data$y1 <- rank(data$keyness, ties.method = "first")
    data$x2 <- 0
    data$y2 <- data$y
    margin <- margin * max(abs(data$x1)) * 2

    x1 <- y1 <- x2 <- y2 <- feature <- NULL
    ggplot(data) +
         xlim(if (show_reference) min(data$x1) - margin else 0, max(data$x1) + margin) +
         geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = color),
                      size = labelsize) +
         scale_colour_identity(NULL, labels = group, breaks = color,
                               guide = if (show_legend) "legend" else FALSE) +
         xlab(measure) +
         geom_label(aes(x = x1, y = y1, label = feature), label.size = NA, fill = NA,
                    vjust = "center", hjust = ifelse(data$right, "left", "right"),
                    color = labelcolor, size = labelsize, family = font) +
         theme_bw(base_family = font) +
         theme(axis.line = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               plot.background = element_blank(),
               panel.grid.major.y = element_line(linetype = "dotted"))
}
