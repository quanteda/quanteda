#' Plot features as a wordcloud
#'
#' Plot a \link{dfm} object as a wordcloud, where the feature labels are plotted
#' with their sizes proportional to their numerical values in the dfm.  When
#' \code{comparison = TRUE}, it plots comparison word clouds by document.
#' @details The default is to plot the word cloud of all features, summed across
#'   documents.  To produce word cloud plots for specific document or set of
#'   documents, you need to slice out the document(s) from the dfm object.
#'
#'   Comparison wordcloud plots may be plotted by setting \code{comparison =
#'   TRUE}, which plots a separate grouping for \emph{each document} in the dfm.
#'   This means that you will need to slice out just a few documents from the
#'   dfm, or to create a dfm where the "documents" represent a subset or a
#'   grouping of documents by some document variable.
#'
#' @param x a dfm object
#' @param min_size size of the smallest word
#' @param max_size size of the largest word
#' @param max_words maximum number of words to be plotted. least frequent terms
#'   dropped
#' @param random_order plot words in random order. If \code{FALSE}, they will be
#'   plotted in decreasing frequency
#' @param random_color choose colors randomly from the colors. If \code{FALSE},
#'   the color is chosen based on the frequency
#' @param ordered_color if \code{TRUE}, then colors are assigned to words in order
#' @param rotation proportion words with 90 degree rotation colors
#' @param color words from least to most frequent
#' @param labelsize
#' @param labeloffset  
#' @param fixed_aspect if \code{TRUE}, the aspect ratio is fixed. Variable aspect ratio
#'   only supported if rotation = 0
#' @param comparison if \code{TRUE}, plot a wordclound that compares documents
#'   in the same way as \code{\link[wordcloud]{comparison.cloud}}
#' @param ... additional parameters passed to \link{text} (and \link{strheight},
#'   \link{strwidth})
#' @examples
#'   # plot the features (without stopwords) from Obama's two inaugural addresses
#'   mydfm <- dfm(corpus_subset(data_corpus_inaugural, President == "Obama"),
#'                remove = stopwords("english"), remove_punct = TRUE)
#'   mydfm <- dfm_trim(mydfm, min_count = 3)
#'   textplot_wordcloud(mydfm)
#'
#'   # plot in colors with some additional options passed to wordcloud
#'   textplot_wordcloud(mydfm, random_color = TRUE, rotation = 0.25,
#'                      color = sample(colors()[2:128], 5))
#'
#'   # old and new
#'   textplot_wordcloudold(mydfm, random.order = FALSE)
#'   textplot_wordcloud(mydfm)
#'
#'   \dontrun{
#'   # comparison plot of Irish government vs opposition
#'   docvars(data_corpus_irishbudget2010, "govtopp") <-
#'           factor(ifelse(data_corpus_irishbudget2010[, "party"] %in%
#'                  c("FF", "Green"), "Govt", "Opp"))
#'   govtopp_dfm <- dfm(data_corpus_irishbudget2010, groups = "govtopp",
#'                      remove_punct = TRUE)
#'   textplot_wordcloud(dfm_tfidf(govtopp_dfm), comparison = TRUE)
#'   # compare to non-tf-idf version
#'   textplot_wordcloud(govtopp_dfm, comparison = TRUE)
#'   }
#' @export
#' @keywords textplot
#' @import ggplot2
textplot_wordcloud <- function(x, 
                               min_size = 0.5, 
                               max_size = 4,
                               max_words = 1000,
                               color = "skyblue",
                               font = NULL,
                               adjust = 0,
                               rotation = 0.1,
                               random_order = TRUE,
                               random_color = FALSE,
                               ordered_color = FALSE,
                               labelcolor = 'black',
                               labelsize = 1,
                               labeloffset = 0.1,
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
                                   max_words = 1000,
                                   color = "skyblue",
                                   font = NULL,
                                   adjust = 0,
                                   rotation = 0.1,
                                   random_order = TRUE,
                                   random_color = FALSE,
                                   ordered_color = FALSE,
                                   labelcolor = 'black',
                                   labelsize = 1,
                                   labeloffset = 0.1,
                                   fixed_aspect = TRUE,
                                   ...,
                                   comparison = FALSE) {

    x <- as.dfm(x)
    if (comparison) {
        if (ndoc(x) > 8) 
            stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud_comparison(x, min_size , max_size, max_words,
                             color, font, adjust, rotation,
                             random_order, random_color, ordered_color,
                             labelcolor, labelsize, labeloffset, fixed_aspect, ...)
    } else {
        wordcloud(x, min_size, max_size, max_words,
                  color, font, adjust, rotation,
                  random_order, random_color, ordered_color,
                  labelcolor, labelsize, labeloffset, fixed_aspect, ...)
    }
}

#' Internal function for textplot_wordcloud
#'
#' This function impliments wordcloud without dependecies. Code is adopted from 
#' \code{\link[wordcloud]{wordcloud}}.
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
#' @keywords internal
#' @author Ian Fellows
wordcloud <- function(x, min_size, max_size, max_words,
                      color, font, adjust, rotation,
                      random_order, random_color, ordered_color,
                      labelcolor, labelsize, labeloffset, fixed_aspect,
                      # deprecated arguments
                      colors, scale, min.freq, max.words, random.order, 
                      random.color, rot.per, ordered.colors, use.r.layout, fixed.asp,
                      ...) {
    
    if (!missing(min.freq)) {
        warning('min.freq is deprecated; use dfm_trim() before textplot_wordcloud()', call. = FALSE)
        x <- dfm_trim(x, min_count = min.freq)
    }
    arg_dep <- character()
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

    thetaStep <- 0.1
    rStep <- 0.05
    
    graphics::plot.new()
    op <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(0, 0, 0, 0), usr = c(-1, 1, -1, 1), family = font)
    if (fixed_aspect) {
        graphics::plot.window(c(0, 1), c(0, 1), asp = 1)
    } else {
        graphics::plot.window(c(0, 1), c(0, 1))
    }
    normedFreq <- freq / max(freq)
    size <- (max_size - min_size) * normedFreq + min_size
    boxes <- list()
    words <- data.frame()
    for (i in seq_along(word)) {
        rotWord <- stats::runif(1) < rotation
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        #size <- numeric(2)
        wid <- graphics::strwidth(word[i], cex = size[i], ...)
        ht <- graphics::strheight(word[i], cex = size[i], ...)
        #mind your ps and qs
        if (grepl(tails, word[i]))
            ht <- ht + ht * 0.2
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        is_overlaped <- TRUE
        while (is_overlaped) {
            if (!qatd_cpp_is_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes) &&
                x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
                if (!random_color) {
                    if (ordered_color) {
                        cc <- color[i]
                    } else {
                        cc <- ceiling(nc * normedFreq[i])
                        cc <- color[cc]
                    }
                } else {
                    cc <- color[sample(seq(nc), 1)]
                }
                text(x1, y1, word[i], cex = size[i], offset = 0, srt = rotWord * 90, col = cc,  ...)
                words <- rbind(words, data.frame(x = x1, y = y1, word = word[i], size = size[i], 
                                                 offset = 0, srt = rotWord * 90, col = cc))
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                is_overlaped <- FALSE
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(word[i], "could not be fit on page. It will not be plotted."))
                    is_overlaped <- FALSE
                }
                theta <- theta + thetaStep
                r <- r + rStep * thetaStep / (2 * pi)
                x1 <- 0.5 + r * cos(theta)
                y1 <- 0.5 + r * sin(theta)
            }
        }
    }
    # abline(v=c(0, 1), h=c(0, 1))
    # abline(v=c(0.25, 0.75), h=c(0.25, 0.75), col=2:3)
    graphics::par(op)
    grDevices::dev.off()
    
    plot <- ggplot() + 
        geom_text(data = words, aes(x, y, label = word), color = words$col, family = font,
                  size = words$size * 4 * (1 + adjust), angle = words$srt, 
                  lineheight = 1, vjust = "center") +
        # geom_vline(xintercept = c(0, 0.25, 0.75, 1)) + # for debug
        # geom_hline(yintercept = c(0, 0.25, 0.75, 1)) + # for debug
        coord_fixed() + 
        scale_x_continuous(limits = c(0, 1), breaks = NULL) + 
        scale_y_continuous(limits = c(0, 1), breaks = NULL) +
        theme(
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank())
    
    return(plot)
}

#' Internal function for textplot_wordcloud
#'
#' This function impliments wordcloud that compares documents. Code is adopted from
#' \code{\link[wordcloud]{comparison.cloud}}.
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
#' @author Ian Fellows
wordcloud_comparison <- function(x, min_size, max_size, max_words,
                                 color, font, adjust, rotation,
                                 random_order, random_color, ordered_color,
                                 labelcolor, labelsize, labeloffset, fixed_aspect,
                                 # deprecated arguments
                                 colors, scale, min.freq, max.words, 
                                 random.order, rot.per, use.r.layout, title.size,
                                 ...) {
    
    if (!missing(min.freq)) {
        warning('min.freq is deprecated; use dfm_trim() before textplot_wordcloud()', call. = FALSE)
        x <- dfm_trim(x, min_count = min.freq)
    }
    arg_dep <- character()
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
    x <- x / rowSums(x)
    x <- x - rowMeans(x)
    x <- t(as.matrix(x))

    ndoc <- ncol(x)
    thetaBins <- seq(0, 2 * pi, length = ndoc + 1)

    if (is.null(color) < ndoc)
        color <- RColorBrewer::brewer.pal(8, "Dark2")
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
    thetaStep <- 0.05
    rStep <- 0.05
    
    graphics::plot.new()
    op <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(0, 0, 0, 0), family = font)
    graphics::plot.window(c(0, 1), c(0, 1), asp = 1)
    
    normedFreq <- freq / max(freq)
    size <- (max_size - min_size) * normedFreq + min_size
    boxes <- list()
    
    #add titles
    docnames <- colnames(x)
    words <- data.frame()
    
    for (i in seq(ncol(x))) {
        th <- mean(thetaBins[seq(i, i + 1)])
        label <- docnames[i]
        if (labelsize > 0) {
            wid <- graphics::strwidth(label, cex = labelsize)
            ht <- graphics::strheight(label, cex = labelsize)
    
            # leaves 5% margin around the cloud
            x1 <- 0.5 + ((0.45 + labeloffset) * cos(th))
            y1 <- 0.5 + ((0.45 + labeloffset) * sin(th))
    
            words <- rbind(words, data.frame(x = x1, y = y1, word = label, size = labelsize, 
                                             offset = 0, srt = 0, col = labelcolor))
            boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
        }
    }
    
    for (i in seq_along(word)) {
        rotWord <- stats::runif(1) < rotation
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        wid <- graphics::strwidth(word[i], cex = size[i], ...)
        ht <- graphics::strheight(word[i], cex = size[i], ...)
        
        if (grepl(tails, word[i]))
            ht <- ht + ht * 0.2 # extra height for g, j, p, q, y
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        is_overlaped <- TRUE
        while (is_overlaped) {
            in_correct_region <- theta > thetaBins[group[i]] && theta < thetaBins[group[i] + 1]
            if (in_correct_region && !qatd_cpp_is_overlap(
                x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes) &&
                x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
                
                # text(x1, y1, word[i], cex = size[i], offset = 0, srt = rotWord * 90, col = color[group[i]], ...)
                words <- rbind(words, data.frame(x = x1, y = y1, word = word[i], size = size[i], 
                                                 offset = 0, srt = rotWord * 90, col = color[group[i]]))
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                is_overlaped <- FALSE
                
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(word[i], "could not be fit on page. It will not be plotted."))
                    is_overlaped <- FALSE
                }
                theta <- theta + thetaStep
                if (theta > 2 * pi)
                    theta <- theta - 2 * pi
                r <- r + rStep * thetaStep / (2 * pi)
                x1 <- 0.5 + r * cos(theta)
                y1 <- 0.5 + r * sin(theta)
            }
        }
    }
    # abline(v=0:1, h=0:1)
    # abline(v=c(0.25, 0,75), h=c(0.25, 0,75), col='red')
    graphics::par(op)
    grDevices::dev.off()
    
    plot <- ggplot() + 
        geom_text(data = words, aes(x, y, label = word), color = words$col, family = font,
                  size = words$size * 4 * (1 + adjust), angle = words$srt, 
                  lineheight = 1, vjust = "center") +
        coord_fixed() + 
        #geom_vline(xintercept = c(0, 0.25, 0.75, 1)) + # for debug
        #geom_hline(yintercept = c(0, 0.25, 0.75, 1)) + # for debug
        scale_x_continuous(limits = c(0, 1), breaks = NULL) + 
        scale_y_continuous(limits = c(0, 1), breaks = NULL) +
        theme(
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank())
    
    return(plot)
}


wordlayout <- function(x, y, word, cex = 1, rotate90 = FALSE, 
                       xlim = c(-Inf, Inf),  ylim = c(-Inf, Inf), tstep = 0.1, rstep = 0.1, ...) {
    
    tails <- "g|j|p|q|y"
    n <- length(word)
    sdx <- sd(x, na.rm = TRUE)
    sdy <- sd(y, na.rm = TRUE)
    if (sdx == 0)
        sdx <- 1
    if (sdy == 0)
        sdy <- 1
    if (length(cex) == 1)
        cex <- rep(cex, n)
    if (length(rotate90) == 1)
        rotate90 <- rep(rotate90, n)
    
    boxes <- list()
    for (i in seq_along(word)) {
        rotWord <- rotate90[i]
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- xo <- x[i]
        y1 <- yo <- y[i]
        wid <- graphics::strwidth(word[i], cex = cex[i], ...)
        ht <- graphics::strheight(word[i], cex = cex[i], ...)
        
        if (grepl(tails, word[i]))
            ht <- ht + ht * 0.2 # extra height for g, j, p, q, y
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        is_overlaped <- TRUE
        while (is_overlaped) {
            if (!qatd_cpp_is_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes) &&
                x1 - 0.5 * wid > xlim[1] && y1 - 0.5 * ht > ylim[1] &&
                x1 + 0.5 * wid < xlim[2] && y1 + 0.5 * ht < ylim[2]) {
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                is_overlaped <- FALSE
            } else{
                theta <- theta + tstep
                r <- r + rstep * tstep / (2 * pi)
                x1 <- xo + sdx * r * cos(theta)
                y1 <- yo + sdy * r * sin(theta)
            }
        }
    }
    result <- do.call(rbind, boxes)
    colnames(result) <- c("x", "y", "width", "ht")
    rownames(result) <- word
    return(result)
}
