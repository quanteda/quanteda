#' Plot features as a wordcloud
#'
#' Plot a \link{dfm} object as a wordcloud, where the feature
#' labels are plotted with their sizes proportional to their numerical values in
#' the dfm.  When \code{comparison = TRUE}, it plots comparison word clouds by
#' document.
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
#' @param scale  vector of length 2 indicating the range of the size of the
#'   words
#' @param max_words maximum number of words to be plotted. least frequent terms
#'   dropped
#' @param random_order plot words in random order. If \code{FALSE}, they will be
#'   plotted in decreasing frequency
#' @param random_color choose colors randomly from the colors. If \code{FALSE},
#'   the color is chosen based on the frequency
#' @param rot_per proportion words with 90 degree rotation colors
#' @param colors words from least to most frequent
#' @param ordered_colors if \code{TRUE}, then colors are assigned to words in
#'   order
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
#'   textplot_wordcloud(mydfm, random_color = TRUE, rot_per = 0.25, 
#'                      colors = sample(colors()[2:128], 5))
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
textplot_wordcloud <- function(x, scale = c(4, 0.5),
                               max_words = Inf,
                               random_order = FALSE,
                               random_color = FALSE,
                               rot_per = 0.1,
                               colors = "black",
                               ordered_colors = FALSE,
                               ...,
                               comparison = FALSE) {
    UseMethod("textplot_wordcloud")
}

#' @export
textplot_wordcloud.default <- function(x, ..., comparison = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textplot_wordcloud"))
}

#' @export
textplot_wordcloud.dfm <- function(x, ..., comparison = FALSE) {

    x <- as.dfm(x)
    if (comparison) {
        if (ndoc(x) > 8) stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud_comparison(x, ...)
    } else {
        wordcloud(x, ...)
    }
}

#' Internal function for textplot_wordcloud
#'
#' This function impliments wordcloud without dependecies. Code is adopted from 
#' \code{\link[wordcloud]{wordcloud}}.
#' @inheritParams textplot_wordcloud
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
wordcloud <- function(x,
                      scale = c(4, 0.5),
                      max_words = Inf,
                      random_order = FALSE,
                      random_color = FALSE,
                      rot_per = 0.1,
                      colors = "black",
                      ordered_colors = FALSE,
                      fixed_asp = TRUE,
                      min.freq,
                      max.words, 
                      random.order, 
                      random.color, 
                      rot.per, 
                      ordered.colors, 
                      use.r.layout, 
                      fixed.asp,
                      ...) {
    
    if (!missing(min.freq)) {
        warning('min.freq is deprecated; use dfm_trim() before textplot_wordcloud()', call. = FALSE)
        x <- dfm_trim(x, min_count = min.freq)
    }
    arg_dep <- character()
    if (!missing(max.words)) {
        max_words <- max.words
        arg_dep <- c(arg_dep, 'max.words')
    }
    if (!missing(random.order)) {
        random_order <- random.order
        arg_dep <- c(arg_dep, 'random.order')
    }
    if (!missing(random.color)) {
        random_color <- random.color
        arg_dep <- c(arg_dep, 'random.color')
    }
    if (!missing(rot.per)) {
        rot_per <- rot.per
        arg_dep <- c(arg_dep, 'rot.per')
    }
    if (!missing(ordered.colors)) {
        ordered_colors <- ordered.colors
        arg_dep <- c(arg_dep, 'ordered.colors')
    }
    if (!missing(use.r.layout)) {
        warning('use.r.layout is no longer used', call. = FALSE)
    }
    if (!missing(fixed.asp)) {
        fixed_asp <- fixed.asp
        arg_dep <- c(arg_dep, 'fixed.asp')
    }
    if (length(arg_dep)) {
        warning(paste(arg_dep), " is deprecated; use ",
                paste(stri_replace_all_fixed(arg_dep, '.', '_')), " instead", call. = FALSE)
    }
    
    if (!fixed_asp && rot_per > 0)
        stop("Variable aspect ratio not supported for rotated words. Set rot_per=0.")
    
    tails <- "g|j|p|q|y"
    nc <- length(colors)
    
    freq <- Matrix::colSums(x)
    words <- names(freq)
    freq <- unname(freq)
    
    if (ordered_colors) {
        if (length(colors) != 1 && length(colors) != length(words)) {
            stop("Length of colors does not match length of words vector")
        }
    }
    
    ord <- rank(-freq, ties.method = "random")
    words <- words[ord <= max_words]
    freq <- freq[ord <= max_words]
    if (ordered_colors) {
        colors <- colors[ord <= max_words]
    }
    
    if (random_order) {
        ord <- sample.int(length(words))
    } else {
        ord <- order(freq, decreasing = TRUE)
    }
    words <- words[ord]
    freq <- freq[ord]

    thetaStep <- 0.1
    rStep <- 0.05
    graphics::plot.new()
    op <- par("mar")
    par(mar = c(0, 0, 0, 0))
    if (fixed_asp) {
        graphics::plot.window(c(0, 1), c(0, 1), asp = 1)
    } else {
        graphics::plot.window(c(0, 1), c(0, 1))
    }
    normedFreq <- freq / max(freq)
    size <- (scale[1] - scale[2]) * normedFreq + scale[2]
    boxes <- list()
    
    for (i in seq_along(words)) {
        rotWord <- stats::runif(1) < rot_per
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        wid <- graphics::strwidth(words[i], cex = size[i], ...)
        ht <- graphics::strheight(words[i], cex = size[i], ...)
        #mind your ps and qs
        if (grepl(tails, words[i]))
            ht <- ht + ht * 0.2
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        isOverlaped <- TRUE
        while (isOverlaped) {
            if (!qatd_cpp_is_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes) &&
                x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
                if (!random_color) {
                    if (ordered_colors) {
                        cc <- colors[i]
                    } else {
                        cc <- ceiling(nc * normedFreq[i])
                        cc <- colors[cc]
                    }
                } else {
                    cc <- colors[sample(1:nc, 1)]
                }
                text(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord * 90, col = cc, ...)
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                isOverlaped <- FALSE
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(
                        words[i], "could not be fit on page. It will not be plotted."
                    ))
                    isOverlaped <- FALSE
                }
                theta <- theta + thetaStep
                r <- r + rStep * thetaStep / (2 * pi)
                x1 <- 0.5 + r * cos(theta)
                y1 <- 0.5 + r * sin(theta)
            }
        }
    }
    par(mar = op)
    invisible()
}

#' Internal function for textplot_wordcloud
#'
#' This function impliments wordcloud that compares documents. Code is adopted from
#' \code{\link[wordcloud]{comparison.cloud}}.
#' @inheritParams textplot_wordcloud
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
wordcloud_comparison <- function(x,
                                 scale = c(4, .5),
                                 max_words = Inf,
                                 random_order = FALSE,
                                 rot_per = .1,
                                 colors = NULL,
                                 title_size = 3,
                                 min.freq,
                                 max.words, 
                                 random.order,
                                 rot.per,
                                 use.r.layout,
                                 title.size,
                                 ...) {
    
    if (!missing(min.freq)) {
        warning('min.freq is deprecated; use dfm_trim() before textplot_wordcloud()', call. = FALSE)
        x <- dfm_trim(x, min_count = min.freq)
    }
    arg_dep <- character()
    if (!missing(max.words)) {
        max_words <- max.words
        arg_dep <- c(arg_dep, 'max.words')
    }
    if (!missing(random.order)) {
        random_order <- random.order
        arg_dep <- c(arg_dep, 'random.order')
    }
    if (!missing(rot.per)) {
        rot_per <- rot.per
        arg_dep <- c(arg_dep, 'rot.per')
    }
    if (!missing(use.r.layout)) {
        warning('use.r.layout is no longer used', call. = FALSE)
    }
    if (!missing(title.size)) {
        title_size <- title.size
        arg_dep <- c(arg_dep, 'title.size')
    }
    if (length(arg_dep)) {
        warning(paste(arg_dep), " is deprecated; use ",
                paste(stri_replace_all_fixed(arg_dep, '.', '_')), " instead", call. = FALSE)
    }
    
    term_matrix <- t(as.matrix(x))
    ndoc <- ncol(term_matrix)
    thetaBins <- seq(0, 2 * pi, length = ndoc + 1)
    for (i in seq(ndoc)) {
        term_matrix[, i] <- term_matrix[, i] / sum(term_matrix[, i])
    }
    mean.rates <- rowMeans(term_matrix)
    for (i in seq(ndoc)) {
        term_matrix[, i] <- term_matrix[, i] - mean.rates
    }
    
    if (is.null(colors))
        colors <- RColorBrewer::brewer.pal(ndoc, "Dark2")
    group <- apply(term_matrix, 1, function(x) which.max(x))
    words <- rownames(term_matrix)
    freq <- apply(term_matrix, 1, function(x) max(x))
    
    tails <- "g|j|p|q|y"
    nc <- length(colors)
    
    ord <- rank(-freq, ties.method = "random")
    words <- words[ord <= max_words]
    freq <- freq[ord <= max_words]
    group <- group[ord <= max_words]
    
    if (random_order) {
        ord <- sample.int(length(words))
    } else {
        ord <- order(freq, decreasing = TRUE)
    }
    words <- words[ord]
    freq <- freq[ord]
    group <- group[ord]
    thetaStep <- 0.05
    rStep <- 0.05
    graphics::plot.new()
    op <- par("mar")
    par(mar = c(0, 0, 0, 0))
    plot.window(c(0, 1), c(0, 1), asp = 1)
    normedFreq <- freq / max(freq)
    size <- (scale[1] - scale[2]) * normedFreq + scale[2]
    boxes <- list()
    
    #add titles
    docnames <- colnames(term_matrix)
    for (i in seq(ncol(term_matrix))) {
        th <- mean(thetaBins[i:(i + 1)])
        word <- docnames[i]
        wid <- graphics::strwidth(word, cex = title_size) * 1.2
        ht <- graphics::strheight(word, cex = title_size) * 1.2
        x1 <- 0.5 + 0.45 * cos(th)
        y1 <- 0.5 + 0.45 * sin(th)
        rect(x1 - 0.5 * wid, y1 - 0.5 * ht, x1 + 0.5 * wid, y1 + 0.5 * ht,
             col = "grey90", border = "transparent")
        text(x1, y1, word, cex = title_size)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
    }
    
    for (i in 1:length(words)) {
        rotWord <- stats::runif(1) < rot_per
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        wid <- graphics::strwidth(words[i], cex = size[i], ...)
        ht <- graphics::strheight(words[i], cex = size[i], ...)
        #mind your ps and qs
        if (grepl(tails, words[i]))
            ht <- ht + ht * 0.2
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        isOverlaped <- TRUE
        while (isOverlaped) {
            inCorrectRegion <- theta > thetaBins[group[i]] && theta < thetaBins[group[i] + 1]
            if (inCorrectRegion && !qatd_cpp_is_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes) &&
                x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
                text(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord * 90, col = colors[group[i]], ...)
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                isOverlaped <- FALSE
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(words[i], "could not be fit on page. It will not be plotted."))
                    isOverlaped <- FALSE
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
    par(mar = op)
    invisible()
}


wordlayout <- function(x, y, words, cex = 1, rotate90 = FALSE, 
                       xlim = c(-Inf, Inf),  ylim = c(-Inf, Inf), tstep = .1, rstep = .1, ...) {
    
    tails <- "g|j|p|q|y"
    n <- length(words)
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
    for (i in seq_along(words)) {
        rotWord <- rotate90[i]
        r <- 0
        theta <- stats::runif(1, 0, 2 * pi)
        x1 <- xo <- x[i]
        y1 <- yo <- y[i]
        wid <- graphics::strwidth(words[i], cex = cex[i], ...)
        ht <- graphics::strheight(words[i], cex = cex[i], ...)
        # mind your ps and qs
        if (grepl(tails, words[i]))
            ht <- ht + ht * 0.2
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        isOverlaped <- TRUE
        while (isOverlaped) {
            if (!qatd_cpp_is_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes) &&
                x1 - 0.5 * wid > xlim[1] && y1 - 0.5 * ht > ylim[1] &&
                x1 + 0.5 * wid < xlim[2] && y1 + 0.5 * ht < ylim[2]) {
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                isOverlaped <- FALSE
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
    rownames(result) <- words
    result
}

textplot <- function(x, y, words, cex = 1, new = TRUE, show.lines = TRUE, ...) {
    
    if (new)
        plot(x, y, type = "n", ...)
    lay <- wordlayout(x, y, words, cex, ...)
    if (show.lines) {
        for (i in seq_along(x)) {
            xl <- lay[i, 1]
            yl <- lay[i, 2]
            w <- lay[i, 3]
            h <- lay[i, 4]
            if (x[i] < xl || x[i] > xl + w ||
                y[i] < yl || y[i] > yl + h) {
                points(x[i], y[i], pch = 16, col = "red", cex = 0.5)
                nx <- xl + 0.5 * w
                ny <- yl + 0.5 * h
                lines(c(x[i], nx), c(y[i], ny), col = "grey")
            }
        }
    }
    text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4], words, cex = cex, ...)
}
