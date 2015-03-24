#' @include dictionaryFunctions.R
NULL

#' Detect collocations from text
#' 
#' Detects collocations (currently, bigrams and trigrams) from texts or a corpus, returning a
#' data.frame of collocations and their scores, sorted in descending order of the association
#' measure.  Words separated by punctuation delimiters \code{.,!?;:(){}[]} are not counted as adjacent
#' and hence are not eligible to be collocations.
#' @param x a text, a character vector of texts, or a corpus
#' @param method association measure for detecting collocations.  Let \eqn{i} index documents, and
#' \eqn{j} index features, \eqn{n_{ij}} refers to observed counts, 
#' and \eqn{m_{ij}} the expected counts in a collocations frequency table of dimensions \eqn{(J - size + 1)^2}.  
#' Available measures are computed as:
#'   \describe{ 
#'   \item{\code{"lr"}}{The likelihood ratio statistic \eqn{G^2}, computed as:
#'          \deqn{2 * \sum_i \sum_j ( n_{ij} * log \frac{n_{ij}}{m_{ij}} )}
#'      }
#'   \item{\code{"chi2"}}{Pearson's \eqn{\chi^2} statistic, computed as:
#'          \deqn{\sum_i \sum_j \frac{(n_{ij} - m_{ij})^2}{m_{ij}}}
#'      }
#'   \item{\code{"pmi"}}{point-wise mutual information score, computed as log \eqn{n_{11}/m_{11}}}
#'   \item{\code{"dice"}}{the Dice coefficient, computed as \eqn{n_{11}/n_{1.} + n_{.1}}}
#'   \item{\code{"all"}}{returns all of the above}
#'   }
#' @details Because of incompatibilities with the join operations in \link{data.table} 
#' when input files have slightly different encoding settings, \code{collocations} currently 
#' converts all text to ASCII prior to processing.  We hope to improve on this in the 
#' future.
#' @param size length of the collocation.  Only bigram (\code{n=2}) and trigram (\code{n=3}) 
#' collocations are implemented so far.  Can be \code{c(2,3)} (or \code{2:3}) to return
#' both bi- and tri-gram collocations.
#' @param n the number of collocations to return, sorted in descending order
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
#' collocations(inaugTexts, n=10)
#' collocations(inaugCorpus, method="all", n=10)
#' collocations(inaugTexts, method="chi2", size=3, n=10)
#' collocations(inaugCorpus, method="pmi", size=3, n=10)
#' txt <- c("This is software testing: looking for (word) pairs!  
#'          This [is] a software testing again. For.",
#'          "Here: is a software testing, looking again for word pairs.")
#' collocations(txt)
#' collocations(txt, size=2:3)
#' removeFeatures(collocations(txt, size=2:3), stopwords("english"))
collocations <- function(x, ...) {
    UseMethod("collocations")
}
 
wFIRSTGREP <- "[])};:,.?!]$"
wMIDDLEGREP <- "[][({)};:,.?!]"
wLASTGREP <- "^[[({]"
containsPunct <- NULL

#' @rdname collocations
#' @export    
collocations.character <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), size=2, n=NULL, ...) {
    method <- match.arg(method)
    
    #  "Enough is enough! I have had it with these motherfucking snakes on this motherfucking plane!"
    x <- iconv(x, "UTF-8", "ASCII",  sub="") # opening some windows
    
    if (any(!(size %in% 2:3)))
        stop("Only bigram and trigram collocations implemented so far.")
    
    coll <- NULL
    if (2 %in% size)
        coll <- collocations2(x, method, 2, n, ...)
    if (3 %in% size) {
        if (is.null(coll)) 
            coll <- collocations3(x, method, 3, n, ...)
        else {
            coll <- rbind(coll, collocations3(x, method, 3, n, ...))
            class(coll) <- c("collocations", class(coll))
        }
    }
    coll
}
    

collocations2 <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), size=2, n=NULL, ...) {
    
    # to not issue the check warnings:
    w1 <- w2 <- count <- w1wn <- w1w2n <- chi2 <- pmi <- dice <- lrratio <- NULL
    
    method <- match.arg(method)
    if (size != 2) stop("Only bigrams (n=2) implemented so far.")
    
    # to prevent warning messages during CHECK
    #w1 <- w2 <- count <- w1w2n <- w1w2Exp <- w1notw2Exp <- notw1w2 <- notw1w2Exp <- NULL
    #notw1notw2 <- notw1notw2Exp <- NULL
    
    text <- clean(x, removePunct=FALSE, ...)
    t <- unlist(tokenizeOnlyCppKB(text), use.names=FALSE)
    
    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-1)], 
                            w2 = t[2:length(t)],
                            count = 1)
    
    # eliminate non-adjacent words (where a blank is in a pair)
    wordpairs[, containsPunct := grepl(wFIRSTGREP, w1) | grepl(wLASTGREP, w2)]
    wordpairs <- wordpairs[containsPunct==FALSE]
    # then remove any remaining punctuation
    wordpairs[, w1 := clean(w1, removePunct=TRUE, removeDigits=FALSE, toLower=FALSE, removeURL=FALSE)]
    wordpairs[, w2 := clean(w2, removePunct=TRUE, removeDigits=FALSE, toLower=FALSE, removeURL=FALSE)]
    
    # eliminate any pair with a word cleaned to the null string
    wordpairs <- wordpairs[w1 != "" & w2 != ""]
    
    # set the data.table sort key
    setkey(wordpairs, w1, w2)
    
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, j=sum(count), by="w1,w2"]
    setnames(wordpairsTable, "V1", "w1w2n")
    
    # tabulate all word marginal counts
    setkey(wordpairs, w1)
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "w1n")
    # sort by w1 and set key
    setkey(w1Table, w1)

    # eliminate any duplicates in w1 - although this ought not to happen!
    # bug in data.table??  encoding problem on our end??
    dups <- which(duplicated(w1Table[,w1]))
    if (length(dups)) {
        cat("  ...NOTE: dropping duplicates in word1:", w1Table[dups, w1], "\n")
        w1Table <- w1Table[-dups]
    }
    
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table])
    # otherwise gives an encoding warning
    rm(w1Table)
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "w2n")
    setkey(w2Table, w2)
    
    # eliminate any duplicates in w2 - although this ought not to happen!
    dups <- which(duplicated(w2Table[,w2]))
    if (length(dups)) {
        cat("...NOTE: dropping duplicates found in word2:", w2Table[dups, w2], "\n")
        w2Table <- w2Table[-dups]
    }
    
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    # otherwise gives an encoding warning
    rm(w2Table)
    rm(allTable)
    
    setkey(allTable2, w1, w2)
    
    # remove any rows where count is NA
    missingCounts <- which(is.na(allTable2$w1w2n))
    if (length(missingCounts))
        allTable2 <- allTable2[-missingCounts]
    
    # N <- wordpairsTable[, sum(w1w2n)]  # total number of collocations (table N for all tables)
    N <- allTable2[, sum(w1w2n)] 
    
    # fill in cells of 2x2 tables
    allTable2$w1notw2 <- allTable2$w1n - allTable2$w1w2
    allTable2$notw1w2 <- allTable2$w2n - allTable2$w1w2
    allTable2$notw1notw2 <- N - (allTable2$w1w2 + allTable2$w1notw2 + allTable2$notw1w2)
    
    # calculate expected values
    allTable2$w1w2Exp <- exp(log(allTable2$w1n) + log(allTable2$w2n) - log(N))
    allTable2$w1notw2Exp <- exp(log(allTable2$w1n) + log((N - allTable2$w2n)) - log(N))
    allTable2$notw1w2Exp <- exp(log(allTable2$w2n) + log((N - allTable2$w1n)) - log(N))
    allTable2$notw1notw2Exp <- exp(log(N - allTable2$w2n) + log(N - allTable2$w1n) - log(N))
    
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
        df <- data.table(word1=allTable2$w1, 
                         word2=allTable2$w2,
                         word3="",
                         count=allTable2$w1w2n,
                         X2=allTable2$chi2)
    } else if (method=="pmi") {
        setorder(allTable2, -pmi)
        df <- data.table(word1=allTable2$w1, 
                         word2=allTable2$w2,
                         word3="",
                         count=allTable2$w1w2n,
                         pmi=allTable2$pmi) 
    
    } else if (method=="dice") {
        setorder(allTable2, -dice)
        df <- data.table(word1=allTable2$w1, 
                         word2=allTable2$w2,
                         word3="",
                         count=allTable2$w1w2n,
                         dice=allTable2$dice) 
    } else {
        setorder(allTable2, -lrratio)
        df <- data.table(word1=allTable2$w1, 
                         word2=allTable2$w2,
                         word3="",
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
    df[1:ifelse(is.null(n), nrow(df), n), ]
}

#' @rdname collocations
#' @export
collocations.corpus <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), size=2, n=NULL, ...) {
    collocations(texts(x), method, size, n, ...)
}



collocations3 <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), size=3, n=NULL, ...) {
    method <- match.arg(method)
    
    # to not issue the check warnings:
    w1 <- w2 <- w3 <- c123 <- c12 <- c13 <- c1 <- c23 <- c2 <- c3 <- X2 <- G2 <- count <- NULL
    
    text <- clean(x, removePunct=FALSE, ...)
    t <- unlist(tokenizeOnlyCppKB(text), use.names=FALSE)
    
    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-2)], 
                            w2 = t[2:(length(t)-1)],
                            w3 = t[3:(length(t))],
                            count = 1)
    
    # eliminate non-adjacent words (where a blank is in a pair)
    wordpairs[, containsPunct := grepl(wFIRSTGREP, w1) | grepl(wMIDDLEGREP, w2) | grepl(wLASTGREP, w3)]
    wordpairs <- wordpairs[containsPunct==FALSE]
    # then remove any remaining punctuation
    #wordpairs[, w1 := clean(w1, removePunct=TRUE, removeDigits=FALSE, toLower=FALSE, removeURL=FALSE)]
    #wordpairs[, w2 := clean(w2, removePunct=TRUE, removeDigits=FALSE, toLower=FALSE, removeURL=FALSE)]
    #wordpairs[, w3 := clean(w3, removePunct=TRUE, removeDigits=FALSE, toLower=FALSE, removeURL=FALSE)]
    wordpairs[, w1 := gsub("[[:punct:]]", "", w1)]
    wordpairs[, w2 := gsub("[[:punct:]]", "", w2)]
    wordpairs[, w3 := gsub("[[:punct:]]", "", w3)]    
    
    # eliminate non-adjacent words (where a blank is in a triplet)
    wordpairs <- wordpairs[w1!="" & w2!="" & w3!=""]
    
    # set the data.table sort key
    setkey(wordpairs, w1, w2, w3)
    
    ## counts of trigrams and bigrams
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, j=sum(count), by="w1,w2,w3"]
    setnames(wordpairsTable, "V1", "c123")
    
    # tabulate all w1 counts
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "c1")
    setkey(w1Table, w1)
    # eliminate any duplicates in w1 - see note above in collocations2
    dups <- which(duplicated(w1Table[,w1]))
    if (length(dups)) {
        cat("  ...NOTE: dropping duplicates in word1:", w1Table[dups, w1], "\n")
        w1Table <- w1Table[-dups]
    }
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table]) # otherwise gives an encoding warning
    rm(w1Table)
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "c2")
    setkey(w2Table, w2)
    setkey(allTable, w2)
    # eliminate any duplicates in w2 - see note above in collocations2
    dups <- which(duplicated(w2Table[,w2]))
    if (length(dups)) {
        cat("  ...NOTE: dropping duplicates in word2:", w2Table[dups, w2], "\n")
        w2Table <- w2Table[-dups]
    }
    suppressWarnings(allTable2 <- allTable[w2Table])
    rm(w2Table)
    rm(allTable)
    
    # tabulate all w3 counts
    w3Table <- wordpairs[, sum(count), by=w3]
    setnames(w3Table, "V1", "c3")
    setkey(w3Table, w3)
    setkey(allTable2, w3)
    # eliminate any duplicates in w3 - see note above in collocations2
    dups <- which(duplicated(w3Table[,w3]))
    if (length(dups)) {
        cat("  ...NOTE: dropping duplicates in word3:", w3Table[dups, w3], "\n")
        w3Table <- w3Table[-dups]
    }
    suppressWarnings(allTable3 <- allTable2[w3Table])
    rm(w3Table)
    rm(allTable2)
    
    # paired occurrence counts
    w12Table <- wordpairs[, sum(count), by="w1,w2"]
    setnames(w12Table, "V1", "c12")
    setkey(w12Table, w1, w2)
    setkey(allTable3, w1, w2)
#     # eliminate any duplicates in w3 - see note above in collocations2
#     dups <- which(duplicated(w12Table[, w1, w2]))
#     if (length(dups)) {
#         cat("  ...NOTE: dropping duplicates in word1,2: ... \n", w12Table[dups, w1, w2], "\n")
#         w3Table <- w3Table[-dups]
#     }
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

    # remove any rows where count is NA
    missingCounts <- which(is.na(allTable6$c123))
    if (length(missingCounts))
        allTable6 <- allTable6[-missingCounts]
    
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
        m1.111 <- exp(log(n111 + n121 + n112 + n122) + log(n111 + n211 + n112 + n212) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.112 <- exp(log(n111 + n121 + n112 + n122) + log(n111 + n211 + n112 + n212) + log(n112 + n212 + n122 + n222) - 2*log(N))
        m1.121 <- exp(log(n111 + n121 + n112 + n122) + log(n121 + n221 + n122 + n222) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.122 <- exp(log(n111 + n121 + n112 + n122) + log(n121 + n221 + n122 + n222) + log(n112 + n212 + n122 + n222) - 2*log(N))
        m1.211 <- exp(log(n211 + n221 + n212 + n222) + log(n111 + n211 + n112 + n212) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.212 <- exp(log(n211 + n221 + n212 + n222) + log(n111 + n211 + n112 + n212) + log(n112 + n212 + n122 + n222) - 2*log(N))
        m1.221 <- exp(log(n211 + n221 + n212 + n222) + log(n121 + n221 + n122 + n222) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.222 <- exp(log(n211 + n221 + n212 + n222) + log(n121 + n221 + n122 + n222) + log(n112 + n212 + n122 + n222) - 2*log(N))
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
    
    dt <- data.table(word1=allTable$w1, 
                     word2=allTable$w2,
                     word3=allTable$w3,
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
    dt[1:ifelse(is.null(n), nrow(dt), n), ]
}



#' convert phrases into single tokens
#' 
#' Replace multi-word phrases in text(s) with a compound version of the phrases 
#' concatenated with  \code{concatenator} (by default, the "\code{_}" character) to
#' form a single token.  This prevents tokenization of the phrases during 
#' subsequent processing by eliminating the whitespace delimiter.
#' @param object source texts, a character or character vector
#' @param phrases a \code{\link{dictionary}} object that 
#'   contains some phrases, defined as multiple words delimited by whitespace, 
#'   up to 9 words long; or a quanteda collocation object created
#'   by \code{\link{collocations}}
#' @param concatenator the concatenation character that will connect the words 
#'   making up the multi-word phrases.  The default \code{_} is highly 
#'   recommended since it will not be removed during normal cleaning and 
#'   tokenization (while nearly all other punctuation characters, at least those
#'   in the POSIX class \code{[:punct:]}) will be removed.
#' @return character or character vector of texts with phrases replaced by 
#'   compound "words" joined by the concatenator
#' @export
#' @author Kenneth Benoit
#' @examples
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised a taxes: an income tax and a sales tax.")
#' mydict <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax")))
#' (cw <- phrasetotoken(mytexts, mydict))
#' dfm(cw, verbose=FALSE)
#' 
#' # when used as a dictionary for dfm creation
#' mydfm2 <- dfm(cw, dictionary=lapply(mydict, function(x) gsub(" ", "_", x)))
#' mydfm2
#' # to pick up "taxes" in the second text, set dictionary_regex=TRUE
#' mydfm3 <- dfm(cw, dictionary=lapply(mydict, phrasetotoken, mydict),
#'               dictionary_regex=TRUE)
#' mydfm3
#' ## one more token counted for "tax" than before
setGeneric("phrasetotoken", 
           function(object, phrases, concatenator="_") standardGeneric("phrasetotoken"))
# phrasetotoken <- function(x, dictionary, concatenator="_") {
#     UseMethod("phrasetotoken")
# }

#' @rdname phrasetotoken
#' @export
setMethod("phrasetotoken", signature=c("character", "dictionary", "ANY"), 
          function(object, phrases, concatenator="_") {
              phrases <- unlist(phrases, use.names=FALSE)
              compoundPhrases <- phrases[grep(" ", phrases)]
              compoundPhrasesList <- tokenize(compoundPhrases)
              
              # contenate the phrases in
              # gsub("(word1)\\s(word2)", "\\1_\\2", "word1 word2")
              ## [1] "word1_word2"
              for (l in compoundPhrasesList) {
                  # match phrases with space delimiters or (already added) concatenator
                  # this catches trigram collocations that have already been processed partly as bigrams
                  re.pattern <- paste("(", paste(l, collapse=paste0(")[\\s", concatenator, "](")), ")", sep="")
                  re.replace <- paste("\\", 1:length(l), sep="", collapse=concatenator)
                  object <- gsub(re.pattern, re.replace, object, perl=TRUE, ignore.case=TRUE)
              }
              object
          })

#  need an S3 method for the S3 corpus object
#' @rdname phrasetotoken
#' @export
phrasetotoken.corpus <- function(object, phrases, concatenator="_") {
    texts(object) <- phrasetotoken(texts(object), phrases, concatenator)
    object
}


#' @rdname phrasetotoken
#' @export
setMethod("phrasetotoken", signature=c("character", "data.table", "ANY"), 
          function(object, phrases, concatenator="_") {
              word1 <- word2 <- word3 <- NULL
              # concatenate the words                               
              word123 <- phrases[, list(word1, word2, word3)]
              mwes <- apply(word123, 1, paste, collapse=" ")
              # strip trailing white space (if no word 3)
              mwes <- gsub("\\s$", "", mwes)
              
              phrasetotoken(object, dictionary(list(mwes=mwes)), concatenator)
          })


# @rdname phrasetotoken
# @export
# phrasetotoken.character <- function(x, dictionary, concatenator="_") {
#     # get the tokenized list of compound phrases from a dictionary (list)
#     phrases <- unlist(dictionary, use.names=FALSE)
#     compoundPhrases <- phrases[grep(" ", phrases)]
#     compoundPhrasesList <- tokenize(compoundPhrases)
#     
#     # contenate the phrases in
#     # gsub("(word1)\\s(word2)", "\\1_\\2", "word1 word2")
#     ## [1] "word1_word2"
#     for (l in compoundPhrasesList) {
#         re.pattern <- paste("(", 
#                             paste(l, collapse=")\\s("),
#                             ")", sep="")
#         re.replace <- paste("\\", 1:length(l), sep="", collapse=concatenator)
#         x <- gsub(re.pattern, re.replace, x, perl=TRUE)
#     }
#     x    
# }

# gapTokenize <- function(txt) {
#     tokenVec <- tokenize(txt, removePunct=FALSE, simplify=TRUE)
#     punctEndIndex <- grep("[])};:,.?!]", tokenVec) # don't pad if last token
#     if (length(punctEndIndex) > 0) {
#         for (i in 1:(length(punctEndIndex))) {
#             if (punctEndIndex[i]+i-1 == length(tokenVec)) break
#             tokenVec <- c(tokenVec[1:(i-1+punctEndIndex[i])], "", tokenVec[(i+punctEndIndex[i]):length(tokenVec)])
#         }
#     }
#     punctBegIndex <- grep("[[({]", tokenVec)
#     if (length(punctBegIndex) > 0) {
#         for (i in 1:(length(punctBegIndex))) {
#             if (punctBegIndex[i] == 1) continue  # don't pad if first token
#             tokenVec <- c(tokenVec[1:(i-2+punctBegIndex[i])], "", tokenVec[(i-1+punctBegIndex[i]):length(tokenVec)])
#         }
#     }
#     # now remove the rest of the stuff not yet cleaned
#     clean(tokenVec, removeDigits = FALSE, toLower = FALSE, removeURL = FALSE)
# }
