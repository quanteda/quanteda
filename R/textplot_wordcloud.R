#' Plot features as a wordcloud
#'
#' Plot a [dfm] or [textstat_keyness] object as a wordcloud, where the feature
#' labels are plotted with their sizes proportional to their numerical values in
#' the dfm.  When `comparison = TRUE`, it plots comparison word clouds by
#' document (or by target and reference categories in the case of a keyness
#' object).
#' @details The default is to plot the word cloud of all features, summed across
#'   documents.  To produce word cloud plots for specific document or set of
#'   documents, you need to slice out the document(s) from the dfm object.
#'   
#'   Comparison wordcloud plots may be plotted by setting `comparison =
#'   TRUE`, which plots a separate grouping for *each document* in the dfm.
#'   This means that you will need to slice out just a few documents from the
#'   dfm, or to create a dfm where the "documents" represent a subset or a
#'   grouping of documents by some document variable.
#'   
#' @param x a [dfm] or [textstat_keyness] object
#' @param min_size size of the smallest word
#' @param max_size size of the largest word
#' @param min_count words with frequency below min_count will not be plotted
#' @param max_words maximum number of words to be plotted. The least frequent
#'   terms dropped.  The maximum frequency will be split evenly across
#'   categories when `comparison = TRUE`.
#' @param color color of words from least to most frequent
#' @param font font-family of words and labels. Use default font if `NULL`.
#' @param adjust adjust sizes of words by a constant. Useful for non-English
#'   words for which R fails to obtain correct sizes.
#' @param rotation proportion of words with 90 degree rotation
#' @param random_order plot words in random order. If `FALSE`, they will be
#'   plotted in decreasing frequency.
#' @param random_color choose colors randomly from the colors. If `FALSE`,
#'   the color is chosen based on the frequency
#' @param ordered_color if `TRUE`, then colors are assigned to words in
#'   order.
#' @param labelcolor color of group labels. Only used when `comparison = TRUE`.
#' @param labelsize size of group labels. Only used when `comparison = TRUE`.
#' @param labeloffset  position of group labels. Only used when
#'   `comparison = TRUE`.
#' @param fixed_aspect logical; if `TRUE`, the aspect ratio is fixed. Variable
#'   aspect ratio only supported if rotation = 0.
#' @param comparison logical; if `TRUE`, plot a wordcloud that compares
#'   documents in the same way as [wordcloud::comparison.cloud()].  If `x` is a 
#'   [textstat_keyness] object, then only the target category's key terms are
#'   plotted when `comparison = FALSE`, otherwise the top `max_words / 2` terms
#'   are plotted from the target and reference categories. 
#' @param ... additional parameters. Only used to make it compatible with
#'   \pkg{wordcloud}
#' @examples
#' # plot the features (without stopwords) from Obama's inaugural addresses
#' set.seed(10)
#' dfmat1 <- dfm(corpus_subset(data_corpus_inaugural, President == "Obama"),
#'               remove = stopwords("english"), remove_punct = TRUE) %>%
#'    dfm_trim(min_termfreq = 3)
#'     
#' # basic wordcloud
#' textplot_wordcloud(dfmat1)
#' 
#' # plot in colors with some additional options
#' textplot_wordcloud(dfmat1, rotation = 0.25, 
#'                    color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
#'   
#' # other display options
#' col <- sapply(seq(0.1, 1, 0.1), function(x) adjustcolor("#1F78B4", x))
#' textplot_wordcloud(dfmat1, adjust = 0.5, random_order = FALSE, 
#'                    color = col, rotation = FALSE)
#'   
#' # comparison plot of Obama v. Trump
#' dfmat2 <- dfm(corpus_subset(data_corpus_inaugural, President %in% c("Obama", "Trump")),
#'               remove = stopwords("english"), remove_punct = TRUE, groups = "President") %>%
#'     dfm_trim(min_termfreq = 3)
#' 
#' textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 300,
#'                    color = c("blue", "red"))
#'                    
#' # for keyness
#' tstat <- tail(data_corpus_inaugural, 2) %>%
#'     dfm(remove_punct = TRUE, remove = stopwords("en")) %>%
#'     textstat_keyness(target = 2)
#' textplot_wordcloud(tstat, max_words = 100)
#' textplot_wordcloud(tstat, comparison = FALSE, max_words = 100)
#' @export
#' @keywords textplot
#' @author Kohei Watanabe, building on code from Ian Fellows's \pkg{wordcloud}
#'   package.
#' @import ggplot2
textplot_wordcloud <- function(x, 
                               min_size = 0.5, 
                               max_size = 4,
                               min_count = 3,
                               max_words = 500,
                               color = "darkblue",
                               font = NULL,
                               adjust = 0,
                               rotation = 0.1,
                               random_order = FALSE,
                               random_color = FALSE,
                               ordered_color = FALSE,
                               labelcolor = "gray20",
                               labelsize = 1.5,
                               labeloffset = 0,
                               fixed_aspect = TRUE,
                               ...,
                               comparison = FALSE) {
    UseMethod("textplot_wordcloud")
}

#' @export
textplot_wordcloud.default <- function(x, ..., comparison = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textplot_wordcloud"))
}

#' @export
textplot_wordcloud.dfm <- function(x, 
                                   min_size = 0.5, 
                                   max_size = 4,
                                   min_count = 3,
                                   max_words = 500,
                                   color = "darkblue",
                                   font = NULL,
                                   adjust = 0,
                                   rotation = 0.1,
                                   random_order = FALSE,
                                   random_color = FALSE,
                                   ordered_color = FALSE,
                                   labelcolor = "gray30",
                                   labelsize = 1.5,
                                   labeloffset = 0.05,
                                   fixed_aspect = TRUE,
                                   ...,
                                   comparison = FALSE) {

    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    if (comparison) {
        if (ndoc(x) > 8) 
            stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud_comparison(x, min_size , max_size, min_count, max_words,
                             color, font, adjust, rotation,
                             random_order, random_color, ordered_color,
                             labelcolor, labelsize, labeloffset, fixed_aspect, ...)
    } else {
        wordcloud(x, min_size, max_size, min_count, max_words,
                  color, font, adjust, rotation,
                  random_order, random_color, ordered_color,
                  labelcolor, labelsize, labeloffset, fixed_aspect, ...)
    }
}

#' @export
textplot_wordcloud.keyness <- function(x, ..., max_words = 100, 
                                       comparison = TRUE) {
    n <- if (!comparison) max_words else floor(max_words / 2)
    # transform into a dfm
    mat <- as.matrix(x[, c("n_target", "n_reference")])
    dimnames(mat) <- list(x$feature, attr(x, "groups"))
    mat <- utils::head(mat, n)
    if (comparison) mat <- rbind(utils::tail(mat, n))
    mat <- as.dfm(t(mat))
    
    # slice out the target only if comparison is not wanted
    if (!comparison) {
        mat <- dfm_trim(mat[1, ], min_termfreq = 1)
    }
    
    textplot_wordcloud(mat, ..., max_words = max_words, comparison = comparison)
}

# internal ----------

#' Internal function for textplot_wordcloud
#'
#' This function implements wordcloud without dependencies. Code is adopted from 
#' [wordcloud::wordcloud()].
#' @inheritParams textplot_wordcloud
#' @param scale deprecated argument
#' @param min.freq deprecated argument
#' @param max.words deprecated argument
#' @param random.order deprecated argument
#' @param random.color deprecated argument
#' @param rot.per deprecated argument
#' @param ordered.colors deprecated argument
#' @param use.r.layout deprecated argument
#' @param fixed.asp deprecated argument
#' @importFrom graphics text
#' @keywords internal
#' @author Kohei Watanabe, building on code from Ian Fellows's \pkg{wordcloud} package.
wordcloud <- function(x, min_size, max_size, min_count, max_words, 
                      color, font, adjust, rotation,
                      random_order, random_color, ordered_color,
                      labelcolor, labelsize, labeloffset, fixed_aspect,
                      # deprecated arguments
                      colors, scale, min.freq, max.words, random.order, 
                      random.color, rot.per, ordered.colors, use.r.layout, fixed.asp, ...) {
    
    arg_dep <- character()
    if (!missing(min.freq)) {
        min_count <- min.freq
        arg_dep <- c(arg_dep, 'min_count' = 'min.freq')
    }
    if (!missing(colors)) {
        color <- colors
        arg_dep <- c(arg_dep, 'color' = 'colors')
    }
    if (!missing(scale)) {
        max_size <- scale[1]
        min_size <- scale[2]
        arg_dep <- c(arg_dep, 'min_size and max_size' = 'scale')
    }
    if (!missing(max.words)) {
        max_words <- max.words
        arg_dep <- c(arg_dep, 'max_words' = 'max.words')
    }
    if (!missing(random.order)) {
        random_order <- random.order
        arg_dep <- c(arg_dep, 'random_order' = 'random.order')
    }
    if (!missing(random.color)) {
        random_color <- random.color
        arg_dep <- c(arg_dep, 'random_color' = 'random.color')
    }
    if (!missing(rot.per)) {
        rotation <- rot.per
        arg_dep <- c(arg_dep, 'rotation' = 'rot.per')
    }
    if (!missing(ordered.colors)) {
        ordered_color <- ordered.colors
        arg_dep <- c(arg_dep, 'ordered_color' = 'ordered.colors')
    }
    if (!missing(use.r.layout)) {
        warning('use.r.layout is no longer used', call. = FALSE)
    }
    if (!missing(fixed.asp)) {
        fixed_aspect <- fixed.asp
        arg_dep <- c(arg_dep, 'fixed_aspect' = 'fixed.asp')
    }
    if (length(arg_dep))
        warning(paste(arg_dep), " is deprecated; use ", paste(names(arg_dep)), " instead", call. = FALSE)
    
    if (!fixed_aspect && rotation > 0)
        stop("Variable aspect ratio not supported for rotated words. Set rotation=0.")
    
    tails <- "g|j|p|q|y"
    nc <- length(color)
    
    font <- check_font(font)
    x <- dfm_trim(x, min_termfreq = min_count)
    # check to see that dfm is not empty
    if (!sum(x)) stop("No features left after trimming with min_count = ", min_count)
    freq <- Matrix::colSums(x)
    word <- names(freq)
    freq <- unname(freq)
    
    if (ordered_color) {
        if (length(color) != 1 && length(color) != length(word)) {
            stop("Length of color does not match length of word vector")
        }
    }
    
    ord <- rank(-freq, ties.method = "random")
    word <- word[ord <= max_words]
    freq <- freq[ord <= max_words]
    if (ordered_color) {
        color <- color[ord <= max_words]
    }
    
    if (random_order) {
        ord <- sample.int(length(word))
    } else {
        ord <- order(freq, decreasing = TRUE)
    }
    word <- word[ord]
    freq <- freq[ord]

    theta_step <- 0.1
    r_step <- 0.05
    
    op <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(0, 0, 0, 0), usr = c(-1, 1, -1, 1), family = font)
    graphics::plot.new()
    
    if (fixed_aspect) {
        graphics::plot.window(c(0, 1), c(0, 1), asp = 1)
    } else {
        graphics::plot.window(c(0, 1), c(0, 1))
    }
    freq <- freq / max(freq)
    size <- (max_size - min_size) * freq + min_size
    size <- size * (min(grDevices::dev.size("in")) / 7) # default window size is 7 in
    boxes <- list()
    for (i in seq_along(word)) {
        rot <- stats::runif(1) < rotation
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5

        wd <- graphics::strwidth(word[i], cex = size[i])
        ht <- graphics::strheight(word[i], cex = size[i])
        
        if (grepl(tails, word[i]))
            ht <- ht * 1.2 # extra height for g, j, p, q, y
        if (rot) {
            tp <- ht
            ht <- wd
            wd <- tp
        }
        
        is_overlapped <- TRUE
        while (is_overlapped) {
            if (!qatd_cpp_is_overlap(x1 - 0.5 * wd, y1 - 0.5 * ht, wd, ht, boxes) &&
                x1 - 0.5 * wd > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wd < 1 && y1 + 0.5 * ht < 1) {
                if (!random_color) {
                    if (ordered_color) {
                        cc <- color[i]
                    } else {
                        cc <- ceiling(nc * freq[i])
                        cc <- color[cc]
                    }
                } else {
                    cc <- color[sample(seq(nc), 1)]
                }
                text(x1, y1, word[i], cex = (1 + adjust) * size[i], offset = 0, srt = rot * 90, col = cc,  ...)
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wd, y1 - 0.5 * ht, wd, ht)
                is_overlapped <- FALSE
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(word[i], "could not be fit on page. It will not be plotted."))
                    is_overlapped <- FALSE
                }
                theta <- theta + theta_step
                r <- r + r_step * theta_step / (2 * pi)
                x1 <- 0.5 + r * cos(theta)
                y1 <- 0.5 + r * sin(theta)
            }
        }
    }
    #abline(v=c(0, 0.25, 0.75, 1), h=c(0, 0.25, 0.75, 1))
    graphics::par(op)
}

#' Internal function for textplot_wordcloud
#'
#' This function implements wordcloud that compares documents. Code is adopted
#' from [wordcloud::comparison.cloud()].
#' @inheritParams textplot_wordcloud
#' @param scale deprecated argument
#' @param min.freq deprecated argument
#' @param max.words deprecated argument
#' @param random.order deprecated argument
#' @param random.color  deprecated argument
#' @param rot.per deprecated argument
#' @param ordered.colors deprecated argument
#' @param use.r.layout deprecated argument
#' @param title.size deprecated argument
#' @keywords internal
#' @keywords textplot_internal
#' @author Kohei Watanabe, build on code from Ian Fellows's \pkg{wordcloud} package.
wordcloud_comparison <- function(x, min_size, max_size, min_count, max_words,
                                 color, font, adjust, rotation,
                                 random_order, random_color, ordered_color,
                                 labelcolor, labelsize, labeloffset, fixed_aspect,
                                 # deprecated arguments
                                 colors, scale, min.freq, max.words, 
                                 random.order, rot.per, use.r.layout, title.size, ...) {
    
    arg_dep <- character()
    if (!missing(min.freq)) {
        min_count <- min.freq
        arg_dep <- c(arg_dep, 'min_count' = 'min.freq')
    }
    if (!missing(colors)) {
        color <- colors
        arg_dep <- c(arg_dep, 'color' = 'colors')
    }
    if (!missing(scale)) {
        max_size <- scale[1]
        min_size <- scale[2]
        arg_dep <- c(arg_dep, 'min_size and max_size' = 'scale')
    }
    if (!missing(max.words)) {
        max_words <- max.words
        arg_dep <- c(arg_dep, 'max_words' = 'max.words')
    }
    if (!missing(random.order)) {
        random_order <- random.order
        arg_dep <- c(arg_dep, 'random_order' = 'random.order')
    }
    if (!missing(rot.per)) {
        rotation <- rot.per
        arg_dep <- c(arg_dep, 'rotation' = 'rot.per')
    }
    if (!missing(use.r.layout)) {
        warning('use.r.layout is no longer used', call. = FALSE)
    }
    if (!missing(title.size)) {
        labelsize <- title.size
        arg_dep <- c(arg_dep, 'labelsize' = 'title.size')
    }
    if (length(arg_dep)) {
        warning(paste(arg_dep), " is deprecated; use ", paste(names(arg_dep)), " instead", call. = FALSE)
    }
    
    font <- check_font(font)
    x <- dfm_trim(x, min_termfreq = min_count)
    x <- dfm_weight(x, 'prop')
    x <- t(x) - Matrix::colMeans(x)
    x <- as.matrix(x)

    ndoc <- ncol(x)
    theta_bins <- seq(0, 2 * pi, length = ndoc + 1)

    if (length(color) < ndoc)
        color <- RColorBrewer::brewer.pal(8, "Paired")
    group <- apply(x, 1, which.max)
    word <- rownames(x)
    freq <- apply(x, 1, max)
    
    tails <- "g|j|p|q|y"
    nc <- length(color)
    
    ord <- rank(-freq, ties.method = "random")
    word <- word[ord <= max_words]
    freq <- freq[ord <= max_words]
    group <- group[ord <= max_words]
    
    if (random_order) {
        ord <- sample.int(length(word))
    } else {
        ord <- order(freq, decreasing = TRUE)
    }
    word <- word[ord]
    freq <- freq[ord]
    group <- group[ord]
    theta_step <- 0.05
    r_step <- 0.05
    
    op <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(0, 0, 0, 0), usr = c(-1, 1, -1, 1), family = font)
    graphics::plot.new()
    if (labelsize > 0) {
        graphics::plot.window(c(-0.1, 1.1), c(-0.1, 1.1), asp = 1)
    } else {
        graphics::plot.window(c(0, 1), c(0, 1), asp = 1)
    }
    freq <- freq / max(freq)
    size <- (max_size - min_size) * freq + min_size
    size <- size * (min(grDevices::dev.size("in")) / 7) # default window size is 7 in
    boxes <- list()
    
    docnames <- colnames(x)
    for (i in seq(ncol(x))) {
        theta <- mean(theta_bins[seq(i, i + 1)])
        label <- docnames[i]
        if (labelsize > 0) {
            wd <- graphics::strwidth(label, cex = labelsize)
            ht <- graphics::strheight(label, cex = labelsize)
    
            x1 <- 0.5 + (0.5 + (labeloffset / 0.5)) * cos(theta)
            y1 <- 0.5 + (0.5 + (labeloffset / 0.5)) * sin(theta)
            text(x1, y1, label, cex = labelsize, offset = 0, col = labelcolor)
            boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wd, y1 - 0.5 * ht, wd, ht)
        }
    }
    
    for (i in seq_along(word)) {
        rot <- stats::runif(1) < rotation
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        wd <- graphics::strwidth(word[i], cex = size[i])
        ht <- graphics::strheight(word[i], cex = size[i])
        
        if (grepl(tails, word[i]))
            ht <- ht * 1.2 # extra height for g, j, p, q, y
        if (rot) {
            tm <- ht
            ht <- wd
            wd <- tm
        }
        is_overlapped <- TRUE
        while (is_overlapped) {
            in_correct_region <- theta > theta_bins[group[i]] && theta < theta_bins[group[i] + 1]
            if (in_correct_region && !qatd_cpp_is_overlap(
                x1 - 0.5 * wd, y1 - 0.5 * ht, wd, ht, boxes) &&
                x1 - 0.5 * wd > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wd < 1 && y1 + 0.5 * ht < 1) {
                
                text(x1, y1, word[i], cex = (1 + adjust) * size[i], offset = 0, srt = rot * 90, col = color[group[i]], ...)
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wd, y1 - 0.5 * ht, wd, ht)
                is_overlapped <- FALSE
                
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(word[i], "could not be fit on page. It will not be plotted."))
                    is_overlapped <- FALSE
                }
                theta <- theta + theta_step
                if (theta > 2 * pi)
                    theta <- theta - 2 * pi
                r <- r + r_step * theta_step / (2 * pi)
                x1 <- 0.5 + r * cos(theta)
                y1 <- 0.5 + r * sin(theta)
            }
        }
    }
    #abline(v=c(0, 0.25, 0.75, 1), h=c(0, 0.25, 0.75, 1))
    graphics::par(op)
}
