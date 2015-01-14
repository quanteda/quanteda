#' Detect collocations from text
#' 
#' Detects collocations (currently, bigrams and trigrams) from texts or a corpus, returning a
#' data.frame of collocations and their scores, sorted in descending order of the association
#' measure.
#' @param x a text, a character vector of texts, or a corpus
#' @param method association measure for detecting collocations.  Available
#'   measures are:
#'   \describe{ 
#'   \item{\code{"lr"}}{The likelihood ratio statistic \eqn{G^2}, computed as:
#'          \deqn{2 * \sum_i \sum_j (n_{ij} * log \frac{n_{ij}}{m_{ij}}}
#'      }
#'   \item{\code{"chi2"}}{Pearson's \eqn{\chi^2} statistic, computed as:
#'          \deqn{\sum_i \sum_j \frac{(n_{ij} - m_{ij})^2}{m_{ij}}}
#'      }
#'   \item{\code{"pmi"}}{point-wise mutual information score, computed as log \eqn{n_{11}/m{11}}}
#'   \item{\code{"dice"}}{the Dice coefficient, computed as \eqn{n_{11}/n_{1.} + n_{.1}}}
#'   \item{\code{"all"}}{returns all of the above}
#'   }
#' @param n length of the collocation.  Only bigram (\code{n=2}) and trigram (\code{n=3}) 
#' collocations are implemented so far.
#' @param top the number of collocations to return, sorted in descending order
#'   of the requested statistic, or \eqn{G^2} if none is specified.
#' @param ... additional parameters passed to \link{clean}
#' @return A data.table of collocations, their frequencies, and the computed 
#'   association measure(s).
#' @export
#' @import data.table
#' @references McInnes, B T. 2004. "Extending the Log Likelihood Measure to Improve Collocation Identification."  M.Sc. Thesis, University of Minnesota.
#' @seealso \link{bigrams}, \link{ngrams}
#' @author Kenneth Benoit
#' @examples
#' collocations(inaugTexts, top=10)
#' collocations(inaugCorpus, top=10, method="all")
#' collocations(inaugTexts, top=10, n=3)
#' collocations(inaugCorpus, top=10, method="all", n=3)
collocations <- function(x, ...) {
    UseMethod("collocations")
}
    
#' @rdname collocations
#' @export    
collocations.character <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), n=2, top=NULL, ...) {
    method <- match.arg(method)
    if (n > 3) 
        stop("Only bigram and trigram collocations implemented so far.")
    if (n == 2)
        collocations2(x, method, 2, top, ...)
    else
        collocations3(x, method, 3, top, ...)
}
    

collocations2 <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), n=2, top=NULL, ...) {
    method <- match.arg(method)
    if (n != 2) stop("Only bigrams (n=2) implemented so far.")
    
    # to prevent warning messages during CHECK
    #w1 <- w2 <- count <- w1w2n <- w1w2Exp <- w1notw2Exp <- notw1w2 <- notw1w2Exp <- NULL
    #notw1notw2 <- notw1notw2Exp <- NULL
    
    text <- clean(x, ...)
    t <- unlist(tokenize(text), use.names=FALSE)
    
    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-1)], 
                            w2 = t[2:length(t)], 
                            count = 1)
    
    # set the data.table sort key
    setkey(wordpairs, w1, w2)
    
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, j=sum(count), by="w1,w2"]
    setnames(wordpairsTable, "V1", "w1w2n")
    
    # tabulate all word marginal counts
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "w1n")
    setkey(w1Table, w1)
    
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table])
    # otherwise gives an encoding warning
    rm(w1Table)
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "w2n")
    setkey(w2Table, w2)
    
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    # otherwise gives an encoding warning
    rm(w2Table)
    rm(allTable)
    
    setkey(allTable2, w1, w2)
    
    N <- wordpairsTable[, sum(w1w2n)]  # total number of collocations (table N for all tables)
    
    # fill in cells of 2x2 tables
    allTable2$w1notw2 <- allTable2$w1n - allTable2$w1w2
    allTable2$notw1w2 <- allTable2$w2n - allTable2$w1w2
    allTable2$notw1notw2 <- N - (allTable2$w1w2 + allTable2$w1notw2 + allTable2$notw1w2)
    
    # calculate expected values
    allTable2$w1w2Exp <- allTable2$w1n * allTable2$w2n / N
    allTable2$w1notw2Exp <- allTable2$w1n * (N - allTable2$w2n) / N
    allTable2$notw1w2Exp <- allTable2$w2n * (N - allTable2$w1n) / N
    allTable2$notw1notw2Exp <- (N - allTable2$w2n) * (N - allTable2$w1n) / N
    
    # vectorized lr stat
    epsilon <- .000000001  # to offset zero cell counts
    if (method=="all" | method=="lr") {
        allTable2$lrratio <- 2 *  ((allTable2$w1w2n * log(allTable2$w1w2n / (allTable2$w1w2Exp + epsilon) + epsilon)) +
                                   (allTable2$w1notw2 * log(allTable2$w1notw2 / (allTable2$w1notw2Exp + epsilon) + epsilon)) +
                                   (allTable2$notw1w2 * log(allTable2$notw1w2 / (allTable2$notw1w2Exp + epsilon) + epsilon)) +
                                   (allTable2$notw1notw2 * log(allTable2$notw1notw2 / (allTable2$notw1notw2Exp + epsilon) + epsilon)))
    }
    if (method=="all" | method=="chi2") {
        allTable2$chi2 <- (allTable2$w1w2n - allTable2$w1w2Exp)^2 / allTable2$w1w2Exp +
            (allTable2$w1notw2 - allTable2$w1notw2Exp)^2 / allTable2$w1notw2Exp +
            (allTable2$notw1w2 - allTable2$notw1w2Exp)^2 / allTable2$notw1w2Exp +
            (allTable2$notw1notw2 - allTable2$notw1notw2Exp)^2 / allTable2$notw1notw2Exp
    }
    if (method=="all" | method=="pmi") {
        allTable2$pmi <- log(allTable2$w1w2n / allTable2$w1w2Exp)
    }
    if (method=="all" | method=="dice") {
        allTable2$dice <- 2 * allTable2$w1w2n / (2*allTable2$w1w2n + allTable2$w1notw2 + allTable2$notw1w2) 
    }
    if (method=="chi2") {
        setorder(allTable2, -chi2)
        df <- data.table(collocation=paste(allTable2$w1, allTable2$w2),
                         count=allTable2$w1w2n,
                         X2=allTable2$chi2)
    } else if (method=="pmi") {
        setorder(allTable2, -pmi)
        df <- data.table(collocation=paste(allTable2$w1, allTable2$w2),
                         count=allTable2$w1w2n,
                         pmi=allTable2$pmi) 
    
    } else if (method=="dice") {
        setorder(allTable2, -dice)
        df <- data.table(collocation=paste(allTable2$w1, allTable2$w2),
                         count=allTable2$w1w2n,
                         dice=allTable2$dice) 
    } else {
        setorder(allTable2, -lrratio)
        df <- data.table(collocation=paste(allTable2$w1, allTable2$w2),
                         count=allTable2$w1w2n,
                         G2=allTable2$lrratio) 
    }
        
    if (method=="all") {
        df$G2 <- allTable2$lrratio
        df$X2 <- allTable2$chi2
        df$pmi <- allTable2$pmi
        df$dice <- allTable2$dice
    }
        
    class(df) <- c("collocations", class(df))
    df[1:ifelse(is.null(top), nrow(df), top), ]
}

#' @rdname collocations
#' @export
collocations.corpus <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), n=2, top=NULL, ...) {
    collocations(texts(x), method, n, top, ...)
}



collocations3 <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), n=3, top=NULL, ...) {
    method <- match.arg(method)
    
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
    rm(w1Table)
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "c2")
    setkey(w2Table, w2)
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    rm(w2Table)
    rm(allTable)
    
    # tabulate all w3 counts
    w3Table <- wordpairs[, sum(count), by=w3]
    setnames(w3Table, "V1", "c3")
    setkey(w3Table, w3)
    setkey(allTable2, w3)
    suppressWarnings(allTable3 <- allTable2[w3Table])
    rm(w3Table)
    rm(allTable2)
    
    # paired occurrence counts
    w12Table <- wordpairs[, sum(count), by="w1,w2"]
    setnames(w12Table, "V1", "c12")
    setkey(w12Table, w1, w2)
    setkey(allTable3, w1, w2)
    suppressWarnings(allTable4 <- allTable3[w12Table])
    rm(w12Table)
    rm(allTable3)
    
    w13Table <- wordpairs[, sum(count), by="w1,w3"]
    setnames(w13Table, "V1", "c13")
    setkey(w13Table, w1, w3)
    setkey(allTable4, w1, w3)
    suppressWarnings(allTable5 <- allTable4[w13Table])
    rm(w13Table)
    rm(allTable4)
    
    w23Table <- wordpairs[, sum(count), by="w2,w3"]
    setnames(w23Table, "V1", "c23")
    setkey(w23Table, w2, w3)
    setkey(allTable5, w2, w3)
    suppressWarnings(allTable6 <- allTable5[w23Table])
    rm(w23Table)
    rm(allTable5)
    
    # total table counts
    N <- allTable6[, sum(c123)]
    
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
    rm(allTable6)
    
    #     ## testing from McInnes thesis Tables 19-20 example
    #     allTable <- rbind(allTable, allTable[478,])
    #     allTable$n111[479] <- 171
    #     allTable$n112[479] <- 3000
    #     allTable$n121[479] <- 2
    #     allTable$n122[479] <- 20805
    #     allTable$n211[479] <- 4
    #     allTable$n212[479] <- 2522
    #     allTable$n221[479] <- 7157
    #     allTable$n222[479] <- 88567875
    #     N <- 88601536
    
    # expected counts m_{ijk} for first independence model
    allTable <- within(allTable, {
        m1.111 <- (n111 + n121 + n112 + n122) * (n111 + n211 + n112 + n212) * (n111 + n211 + n121 + n221) / N^2
        m1.112 <- (n111 + n121 + n112 + n122) * (n111 + n211 + n112 + n212) * (n112 + n212 + n122 + n222) / N^2
        m1.121 <- (n111 + n121 + n112 + n122) * (n121 + n221 + n122 + n222) * (n111 + n211 + n121 + n221) / N^2
        m1.122 <- (n111 + n121 + n112 + n122) * (n121 + n221 + n122 + n222) * (n112 + n212 + n122 + n222) / N^2
        m1.211 <- (n211 + n221 + n212 + n222) * (n111 + n211 + n112 + n212) * (n111 + n211 + n121 + n221) / N^2
        m1.212 <- (n211 + n221 + n212 + n222) * (n111 + n211 + n112 + n212) * (n112 + n212 + n122 + n222) / N^2
        m1.221 <- (n211 + n221 + n212 + n222) * (n121 + n221 + n122 + n222) * (n111 + n211 + n121 + n221) / N^2
        m1.222 <- (n211 + n221 + n212 + n222) * (n121 + n221 + n122 + n222) * (n112 + n212 + n122 + n222) / N^2
    })
    
    epsilon <- .000000001  # to offset zero cell counts
    allTable <- within(allTable, {
        lrratio <- 2 * ((n111 * log(n111 / m1.111 + epsilon)) + (n112 * log(n112 / m1.112 + epsilon)) +
                        (n121 * log(n121 / m1.121 + epsilon)) + (n122 * log(n122 / m1.122 + epsilon)) +
                        (n211 * log(n211 / m1.211 + epsilon)) + (n212 * log(n212 / m1.212 + epsilon)) +
                        (n221 * log(n221 / m1.221 + epsilon)) + (n222 * log(n222 / m1.222 + epsilon)))
        chi2 <- ((n111 - m1.111)^2 / m1.111) + ((n112 - m1.112)^2 / m1.112) +
                ((n121 - m1.121)^2 / m1.121) + ((n122 - m1.122)^2 / m1.122) +
                ((n211 - m1.211)^2 / m1.211) + ((n212 - m1.212)^2 / m1.212) +
                ((n221 - m1.221)^2 / m1.221) + ((n222 - m1.222)^2 / m1.222)
        pmi <- log(n111 / m1.111)
        dice <- 2 * n111 / (n111 + n121 + n112 + n122 + n111 + n211 + n112 + n212 + n111 + n211 + n121 + n221)
    })         
    
    dt <- data.table(collocation = paste(allTable$w1, allTable$w2, allTable$w3),
                     count = allTable$c123)
    
    if (method=="chi2") {
        dt$X2 <- allTable$chi2
        setorder(dt, -X2)
    } else if (method=="pmi") {
        dt$pmi <- allTable$pmi
        setorder(dt, -pmi)
    } else if (method=="dice") {
        dt$dice <- allTable$dice
        setorder(dt, -dice)
    } else {
        dt$G2 <- allTable$lrratio
        setorder(dt, -G2)
    }
    
    if (method=="all") {
        dt$X2 <- allTable$chi2
        dt$pmi <- allTable$pmi
        dt$dice <- allTable$dice
    }

    class(dt) <- c("collocations", class(dt))
    dt[1:ifelse(is.null(top), nrow(dt), top), ]
}



#' convert phrases into single tokens
#' 
#' Replace multi-word phrases in text(s) with a compound version of the phrases 
#' concatenated with  \code{connector} (by default, the "\code{_}" character) to
#' form a single token.  This prevents tokenization of the phrases during 
#' subsequent processing by eliminating the whitespace delimiter.
#' @param txts character or character vector of texts
#' @param dictionary a list or named list (such as a quanteda dictionary) that 
#'   contains some phrases, defined as multiple words delimited by whitespace. 
#'   These can be up to 9 words long.
#' @param connector the concatenation character that will connect the words 
#'   making up the multi-word phrases.  The default \code{_} is highly 
#'   recommended since it will not be removed during normal cleaning and 
#'   tokenization (while nearly all other punctuation characters, at least those
#'   in the POSIX class \code{[[:punct:]]}) will be removed.
#' @return character or character vector of texts with phrases replaced by 
#'   compound "words" joined by the connector
#' @export
#' @examples
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised a taxes: an income tax and a sales tax.")
#' mydict <- list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax"))
#' (cw <- compoundWords(mytexts, mydict))
#' print(dfm(cw), show.values=TRUE)
#' 
#' # when used as a dictionary for dfm creation
#' mydfm2 <- dfm(cw, dictionary=lapply(mydict, function(x) gsub(" ", "_", x)))
#' print(mydfm2, show.values=TRUE)
#' # to pick up "taxes" in the second text, set regular_expression=TRUE
#' mydfm3 <- dfm(cw, dictionary=lapply(mydict, function(x) gsub(" ", "_", x)),
#'               dictionary_regex=TRUE)
#' print(mydfm3, show.values=TRUE)
compoundWords <- function(txts, dictionary, connector="_") {
    # get the tokenized list of compound phrases from a dictionary (list)
    phrases <- unlist(dictionary, use.names=FALSE)
    compoundPhrases <- phrases[grep(" ", phrases)]
    compoundPhrasesList <- tokenize(compoundPhrases)
    
    # contenate the phrases in
    # gsub("(word1)\\s(word2)", "\\1_\\2", "word1 word2")
    ## [1] "word1_word2"
    for (l in compoundPhrasesList) {
        re.pattern <- paste("(", 
                            paste(l, collapse=")\\s("),
                            ")", sep="")
        re.replace <- paste("\\", 1:length(l), sep="", collapse=connector)
        txts <- gsub(re.pattern, re.replace, txts, perl=TRUE)
    }
    txts    
}

