# Detect trigram collocations from text
# 
# Detects trigram collocations from texts or a corpus, returning a data.frame
# of collocations and their scores, sorted by the likelihood ratio \eqn{G^2}.
# @param x a text, a character vector of texts, or a corpus
# @param method association measure for detecting collocations.  Currently only
#    \code{lr} (likelihood ratio statistic \eqn{G^2}) is implemented.
# @param n length of the collocation.  Only trigrams (\code{n=3}) implemented so
#   far.
# @param top the number of collocations to return, sorted in descending order
#   of the requested statistic, or \eqn{G^2} if none is specified.
# @param ... additional parameters
# @return A data.frame of collocations, their frequencies, and the computed 
#   association measure.
# @export
# @author Kenneth Benoit
# @examples
# collocations(inaugTexts, top=10)
# collocations(inaugCorpus, top=10, method="chi2")
collocations3 <- function(x, ...) {
    UseMethod("collocations")
}

# @rdname collocations
# @export    
collocations3.character <- function(x, method=c("lr"), n=3, top=NULL, ...) {
    method <- match.arg(method)
    if (n != 3) stop("Hey, this method is for trigrams only! (n=3).")
    
    text <- clean(x, ...)
    t <- unlist(tokenize(text), use.names=FALSE)
    
    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-2)], 
                            w2 = t[2:(length(t)-1)],
                            w3 = t[3:(length(t))],
                            count = 1)
    
    # set the data.table sort key
    #setkey(wordpairs, w1, w2, w3)
    
    ## counts of trigrams and bigrams
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, j=sum(count), by="w1,w2,w3"]
    setnames(wordpairsTable, "V1", "c123")
    
    # tabulate all w1 counts
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "c1")
    setkey(w1Table, w1)
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table]) # otherwise gives an encoding warning
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "c2")
    setkey(w2Table, w2)
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    
    # tabulate all w3 counts
    w3Table <- wordpairs[, sum(count), by=w3]
    setnames(w3Table, "V1", "c3")
    setkey(w3Table, w3)
    setkey(allTable2, w3)
    suppressWarnings(allTable3 <- allTable2[w3Table])
    
    # paired occurrence counts
    w12Table <- wordpairs[, sum(count), by="w1,w2"]
    setnames(w12Table, "V1", "c12")
    setkey(w12Table, w1, w2)
    setkey(allTable3, w1, w2)
    suppressWarnings(allTable4 <- allTable3[w12Table])
    
    w13Table <- wordpairs[, sum(count), by="w1,w3"]
    setnames(w13Table, "V1", "c13")
    setkey(w13Table, w1, w3)
    setkey(allTable4, w1, w3)
    suppressWarnings(allTable5 <- allTable4[w13Table])
    
    w23Table <- wordpairs[, sum(count), by="w2,w3"]
    setnames(w23Table, "V1", "c23")
    setkey(w23Table, w2, w3)
    setkey(allTable5, w2, w3)
    suppressWarnings(allTable6 <- allTable5[w23Table])
    
    ## cell counts
    # total table counts
    N <- sum(allTable6$c123)  # total number of collocations (table N for all tables)
    
    # observed counts n_{ijk}
    allTable <- within(allTable6, {
        n111 <- c123
        n112 <- c12 - c123
        n121 <- c13 - c123
        n122 <- c1 - c12 - n121
        n211 <- c23 - c123
        n212 <- c2 - c12 - n211
        n221 <- c3 - c13 - n211
        n222 <- N - c1 - n211 - n212 - n221
    })
    
    # expected counts m_{ijk}
    allTable <- within(allTable, {
        m111 <- (c12 + c13 + c23) / c123
        m211 <- c23 - c123
        m121 <- c13 - c123
        m122 <- c1 - c12 - m121
        
        m112 <- c12 - c123
        m212 <- c2 - c12 - m211
        m221 <- c3 - c13 - m211
        m222 <- N - c1 - n211 - n212 - n221
    })
    
    mijk = nipp npjp nppk / nppp
    
    allTable3$w1w2notw3 c1c2
    allTable3$w1notw2w3
    allTable3$w1notw2notw3    
    allTable3$notw1w2w3
    allTable3$notw1w2notw3
    allTable3$notw1notw2w3
    allTable3$notw1notw2notw3
    
    # calculate expected values
    allTable3$Ew1w2w3         <- allTable3$w1n * allTable3$w2n * allTable3$w3n / N 
    allTable3$Ew1w2notw3
    allTable3$Ew1notw2w3
    allTable3$Ew1notw2notw3    
    allTable3$Enotw1w2w3
    allTable3$Enotw1w2notw3
    allTable3$Enotw1notw2w3
    allTable3$Enotw1notw2notw3
    
    # vectorized lr stat
    epsilon <- .000000001  # to offset zero cell counts
    allTable3$lrratio <- 2 *  ((allTable3$w1w2n * log(allTable3$w1w2n / allTable3$w1w2Exp + epsilon)) +
                                   (allTable3$w1notw2 * log(allTable3$w1notw2 / allTable3$w1notw2Exp + epsilon)) +
                                   (allTable3$notw1w2 * log(allTable3$notw1w2 / allTable3$notw1w2Exp + epsilon)) +
                                   (allTable3$notw1notw2 * log(allTable3$notw1notw2 / allTable3$notw1notw2Exp + epsilon)))
    allTable3 <- allTable3[order(-lrratio)]
    df <- data.frame(collocation=paste(allTable3$w1, allTable3$w2, allTable3$w3),
                     count=allTable3$w1w2nw3n,
                     G2=allTable3$lrratio) 
    
    df[1:ifelse(is.null(top), N, top), ]
}

