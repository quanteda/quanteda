# Author: ianfellows
###############################################################################

wordcloud <-  function(freq,
                       scale = c(4, 0.5),
                       max_words = Inf,
                       random_order = FALSE,
                       random_color = FALSE,
                       rot_per = 0.1,
                       colors = "black",
                       ordered_colors = FALSE,
                       use_r_layout = FALSE,
                       fixed_asp = TRUE,
                       ...) {
        
        # trap older arguments, issue a warning, and call with correct arguments
        calls <- list(...)
        arg_dep <- character()
        if ('max.words' %in% names(calls)) {
            max_words <- calls[['max.words']]
            arg_dep <- c(arg_dep, 'max.words')
        }
        if ('random.order' %in% names(calls)) {
            random_order <- calls[['random.order']]
            arg_dep <- c(arg_dep, 'random.order')
        }
        if ('random.color' %in% names(calls)) {
            random_color <- calls[['random.color']]
            arg_dep <- c(arg_dep, 'random.color')
        }
        if ('rot.per' %in% names(calls)) {
            rot_per <- calls[['rot.per']]
            arg_dep <- c(arg_dep, 'rot.per')
        }
        if ('ordered.colors' %in% names(calls)) {
            ordered_colors <- calls[['ordered.colors']]
            arg_dep <- c(arg_dep, 'ordered.colors')
        }
        if ('use.r.layout' %in% names(calls)) {
            use_r_layout <- calls[['use.r.layout']]
            arg_dep <- c(arg_dep, 'use.r.layout')
        }
        if ('fixed.asp' %in% names(calls)) {
            fixed_asp <- calls[['fixed.asp']]
            arg_dep <- c(arg_dep, 'fixed.asp')
        }
        if (length(arg_dep)) {
            warning(paste(arg_dep), " is deprecated; use ",
                    paste(stri_replace_all_fixed(arg_dep, '.', '_')), " instead", call. = FALSE)
        }
        
        if (!fixed_asp && rot_per > 0)
            stop("Variable aspect ratio not supported for rotated words. Set rot_per=0.")
    
        tails <- "g|j|p|q|y"
        last <- 1
        nc <- length(colors)
        words <- names(freq)
        freq <- unname(freq)
    
        if (ordered_colors) {
            if (length(colors) != 1 && length(colors) != length(words)) {
                stop("Length of colors does not match length of words vector")
            }
        }
        
        # if (min.freq > max(freq))
        #     min.freq <- 0
        
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
        # words <- words[freq >= min.freq]
        # freq <- freq[freq >= min.freq]
        # if (ordered_colors) {
        #     colors <- colors[ord][freq >= min.freq]
        # }
        
        thetaStep <- 0.1
        rStep <- 0.05
        plot.new()
        op <- par("mar")
        par(mar = c(0, 0, 0, 0))
        if (fixed_asp) {
            plot.window(c(0, 1), c(0, 1), asp = 1)
        } else {
            plot.window(c(0, 1), c(0, 1))
        }
        normedFreq <- freq / max(freq)
        size <- (scale[1] - scale[2]) * normedFreq + scale[2]
        boxes <- list()
        
        for (i in seq_along(words)) {
            rotWord <- runif(1) < rot_per
            r <- 0
            theta <- runif(1, 0, 2 * pi)
            x1 <- 0.5
            y1 <- 0.5
            wid <- strwidth(words[i], cex = size[i], ...)
            ht <- strheight(words[i], cex = size[i], ...)
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
                if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes, use_r_layout) &&
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
                    text(x1, y1, words[i], cex = size[i], offset = 0,
                        srt = rotWord * 90, col = cc, ...
                    )
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

#Call down to c++ to find out if any overplotting would occur
# .overlap <- function(x11, y11, sw11, sh11, boxes1) {
#     qatd_cpp_is_overlap(x11, y11, sw11, sh11, boxes1)
# }



#a cloud comparing the frequencies of words across documents
wordcloud_comparison <- function(term_matrix,
                                 scale = c(4, .5),
                                 max_words = Inf,
                                 random_order = FALSE,
                                 rot_per = .1,
                                 colors = NULL,
                                 use_r_layout = FALSE,
                                 title_size = 3,
                                 ...) {
        
    
    calls <- list(...)
    arg_dep <- character()
    if ('max.words' %in% names(calls)) {
        max_words <- calls[['max.words']]
        arg_dep <- c(arg_dep, 'max.words')
    }
    if ('random.order' %in% names(calls)) {
        random_order <- calls[['random.order']]
        arg_dep <- c(arg_dep, 'random.order')
    }
    if ('rot.per' %in% names(calls)) {
        rot_per <- calls[['rot.per']]
        arg_dep <- c(arg_dep, 'rot.per')
    }
    if ('use.r.layout' %in% names(calls)) {
        use_r_layout <- calls[['use.r.layout']]
        arg_dep <- c(arg_dep, 'use.r.layout')
    }
    if ('title.size' %in% names(calls)) {
        title_size <- calls[['title.size']]
        arg_dep <- c(arg_dep, 'title.size')
    }
    if (length(arg_dep)) {
        warning(paste(arg_dep), " is deprecated; use ",
                paste(stri_replace_all_fixed(arg_dep, '.', '_')), " instead", call. = FALSE)
    }
    
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
    last <- 1
    nc <- length(colors)
    
    ord <- rank(-freq, ties.method = "random")
    words <- words[ord <= max_words]
    freq <- freq[ord <= max_words]
    group <- group[ord <= max_words]
    
    if (random_order) {
        ord <- sample.int(length(words))
    } else{
        ord <- order(freq, decreasing = TRUE)
    }
    words <- words[ord]
    freq <- freq[ord]
    group <- group[ord]
    thetaStep <- .05
    rStep <- .05
    plot.new()
    op <- par("mar")
    par(mar = c(0, 0, 0, 0))
    plot.window(c(0, 1), c(0, 1), asp = 1)
    normedFreq <- freq / max(freq)
    size <- (scale[1] - scale[2]) * normedFreq + scale[2]
    boxes <- list()
    
    #add titles
    docnames <- colnames(term_matrix)
    for (i in 1:ncol(term_matrix)) {
        th <- mean(thetaBins[i:(i + 1)])
        word <- docnames[i]
        wid <- strwidth(word, cex = title_size) * 1.2
        ht <- strheight(word, cex = title_size) * 1.2
        x1 <- 0.5 + 0.45 * cos(th)
        y1 <- 0.5 + 0.45 * sin(th)
        rect(x1 - 0.5 * wid,
             y1 - 0.5 * ht,
             x1 + 0.5 * wid,
             y1 + 0.5 * ht,
             col = "grey90",
             border = "transparent")
        text(x1, y1, word, cex = title_size)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
    }
    
    for (i in 1:length(words)) {
        rotWord <- runif(1) < rot_per
        r <- 0
        theta <- runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        wid <- strwidth(words[i], cex = size[i], ...)
        ht <- strheight(words[i], cex = size[i], ...)
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
            if (inCorrectRegion && !overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht, boxes, use_r_layout) &&
                x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 0 &&
                x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
                text(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord * 90, col = colors[group[i]], ...)
                #rect(x1-.5*wid,y1-.5*ht,x1+.5*wid,y1+.5*ht)
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
                isOverlaped <- FALSE
            } else {
                if (r > sqrt(0.5)) {
                    warning(paste(
                        words[i],
                        "could not be fit on page. It will not be plotted."
                    ))
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


wordlayout <- function(x,
                       y,
                       words,
                       cex = 1,
                       rotate90 = FALSE,
                       xlim = c(-Inf, Inf),
                       ylim = c(-Inf, Inf),
                       tstep = .1,
                       rstep = .1,
                       ...) {
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
    for (i in 1:length(words)) {
        rotWord <- rotate90[i]
        r <- 0
        theta <- runif(1, 0, 2 * pi)
        x1 <- xo <- x[i]
        y1 <- yo <- y[i]
        wid <- strwidth(words[i], cex = cex[i], ...)
        ht <- strheight(words[i], cex = cex[i], ...)
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

textplot <- function(x, y,
                     words,
                     cex = 1,
                     new = TRUE,
                     show.lines = TRUE,
                     ...) {
            
    if (new)
        plot(x, y, type = "n", ...)
    lay <- wordlayout(x, y, words, cex, ...)
    if (show.lines) {
        for (i in 1:length(x)) {
            xl <- lay[i, 1]
            yl <- lay[i, 2]
            w <- lay[i, 3]
            h <- lay[i, 4]
            if (x[i] < xl || x[i] > xl + w ||
                y[i] < yl || y[i] > yl + h) {
                points(x[i],
                       y[i],
                       pch = 16,
                       col = "red",
                       cex = 0.5)
                nx <- xl + 0.5 * w
                ny <- yl + 0.5 * h
                lines(c(x[i], nx), c(y[i], ny), col = "grey")
            }
        }
    }
    text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4], words, cex = cex, ...)
}


overlap <- function(x1, y1, sw1, sh1, boxes, use_r_layout) {
    if (!use_r_layout)
        return(qatd_cpp_is_overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0)
        return(FALSE)
    for (i in c(last, seq_along(boxes))) {
        bnds <- boxes[[i]]
        x2 <- bnds[1]
        y2 <- bnds[2]
        sw2 <- bnds[3]
        sh2 <- bnds[4]
        if (x1 < x2) {
            overlap <- x1 + sw1 > x2 - s
        } else {
            overlap <- x2 + sw2 > x1 - s
        }
        if (y1 < y2) {
            overlap <- overlap && (y1 + sh1 > y2 - s)
        } else {
            overlap <- overlap && (y2 + sh2 > y1 - s)
        }
        if (overlap) {
            last <<- i
            return(TRUE)
        }
    }
    FALSE
}
