#' @include corpus.R
#' @include dictionaries.R
NULL

#' Detect collocations from text
#' 
#' Detects collocations from texts or a corpus, returning a data.frame of
#' collocations and their scores, sorted in descending order of the association
#' measure.  Words separated by punctuation delimiters are not counted by
#' default (\code{spanPunct = FALSE})  as adjacent and hence are not eligible to
#' be collocations.
#' @param x a text, a character vector of texts, or a corpus
#' @param method association measure for detecting collocations.  Let \eqn{i} 
#'   index documents, and \eqn{j} index features, \eqn{n_{ij}} refers to 
#'   observed counts, and \eqn{m_{ij}} the expected counts in a collocations 
#'   frequency table of dimensions \eqn{(J - size + 1)^2}. Available measures 
#'   are computed as: \describe{ \item{\code{"lr"}}{The likelihood ratio 
#'   statistic \eqn{G^2}, computed as: \deqn{2 * \sum_i \sum_j ( n_{ij} * log 
#'   \frac{n_{ij}}{m_{ij}} )} } \item{\code{"chi2"}}{Pearson's \eqn{\chi^2} 
#'   statistic, computed as: \deqn{\sum_i \sum_j \frac{(n_{ij} - 
#'   m_{ij})^2}{m_{ij}}} } \item{\code{"pmi"}}{point-wise mutual information 
#'   score, computed as log \eqn{n_{11}/m_{11}}} \item{\code{"dice"}}{the Dice 
#'   coefficient, computed as \eqn{n_{11}/n_{1.} + n_{.1}}} 
#'   \item{\code{"all"}}{returns all of the above} }
#' @param size length of the collocation.  Only bigram (\code{n=2}) and trigram 
#'   (\code{n=3}) collocations are currently implemented.  Can be \code{c(2,3)}
#'   (or \code{2:3}) to return both bi- and tri-gram collocations.
#' @param n the number of collocations to return, sorted in descending order of 
#'   the requested statistic, or \eqn{G^2} if none is specified.
#' @param punctuation how to handle tokens separated by punctuation characters.  Options are:
#'   \describe{
#'   \item{\code{dontspan}}{do not form collocations from tokens separated by punctuation characters (default)}
#'   \item{\code{ignore}}{ignore punctuation characters when forming collocations, meaning that collocations will 
#'   include those separated by punctuation characters in the text.  The punctuation characters themselves are not
#'   returned.}
#'   \item{\code{include}}{do not treat punctuation characters specially, meaning that collocations will include
#'   punctuation characters as tokens}
#'   }
#' @param toLower convert collocations to lower case if \code{TRUE} (default)
#' @param ... additional parameters passed to \code{\link{tokenize}}
#' @return A data.table of collocations, their frequencies, and the computed 
#'   association measure(s).
#' @export
#' @import data.table
#' @references McInnes, B T. 2004. "Extending the Log Likelihood Measure to 
#'   Improve Collocation Identification."  M.Sc. Thesis, University of 
#'   Minnesota.
#' @seealso \link{ngrams}
#' @author Kenneth Benoit
#' @examples
#' txt <- c("This is software testing: looking for (word) pairs!  
#'          This [is] a software testing again. For.",
#'          "Here: this is more Software Testing, looking again for word pairs.")
#' collocations(txt, punctuation = "dontspan") # default
#' collocations(txt, punctuation = "dontspan", removePunct = TRUE)  # includes "testing looking"
#' collocations(txt, punctuation = "ignore", removePunct = TRUE)    # same as previous 
#' collocations(txt, punctuation = "include", removePunct = FALSE)  # keep punctuation as tokens
#'
#' collocations(txt, size = 2:3)
#' removeFeatures(collocations(txt, size = 2:3), stopwords("english"))
#' 
#' collocations("@@textasdata We really, really love the #quanteda package - thanks!!")
#' collocations("@@textasdata We really, really love the #quanteda package - thanks!!",
#'               removeTwitter = TRUE)
#' 
#' collocations(inaugTexts[49:57], n = 10)
#' collocations(inaugTexts[49:57], method = "all", n = 10)
#' collocations(inaugTexts[49:57], method = "chi2", size = 3, n = 10)
#' collocations(subset(inaugCorpus, Year>1980), method = "pmi", size = 3, n = 10)
collocations <- function(x, ...) {
#     addedArgs <- names(list(...))
#     if (length(addedArgs) && any(!(addedArgs %in% names(formals(getS3method("tokenize", "character"))))))
#         warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), addedArgs, " not used.", sep = "", noBreaks. = TRUE)
    UseMethod("collocations")
}
 
wFIRSTGREP <- "[])};:,.?!$\u2014]"
wMIDDLEGREP <- "[][({)};:,.?!\u2014]"
wLASTGREP <- "[][^({]"
containsPunct <- NULL

wFIRSTGREPpenn <- "([,:.]|''|``|-rrb-)_.*"
wMIDDLEGREPpenn <- "([,:.]|''|``|-[lr]rb-)_.*"
wLASTGREPpenn <- "-lrb-_.*"

#' @rdname collocations
#' @export
collocations.corpus <- function(x, method = c("lr", "chi2", "pmi", "dice", "all"), size = 2, 
                                n = NULL, toLower = TRUE, 
                                punctuation = c("dontspan", "ignore", "include"), ...) {
    collocations(texts(x), method = method, size = size, n = n, punctuation = punctuation, ...)
}

#' @rdname collocations
#' @export    
collocations.character <- function(x, method = c("lr", "chi2", "pmi", "dice", "all"), size = 2, 
                                   n = NULL, toLower = TRUE, 
                                   punctuation = c("dontspan", "ignore", "include"), ...) {
    method <- match.arg(method)
    x <- tokenize((if (toLower) toLower(x) else x), ...)
    collocations(x, method = method, size = size , n = n, punctuation = punctuation)
}

#' @rdname collocations
#' @export    
collocations.tokenizedTexts <- function(x, method = c("lr", "chi2", "pmi", "dice", "all"), size = 2, 
                                        n = NULL, toLower = FALSE,
                                        punctuation = c("dontspan", "ignore", "include"), ...) {

    punctuation <- match.arg(punctuation)
    
    # add a dummy token denoting the end of the line
    DUMMY_TOKEN <- "_END_OF_TEXT_"
    x <- lapply(x, function(toks) c(toks, DUMMY_TOKEN))
    x <- unlist(x, use.names = FALSE)
    method <- match.arg(method)
    if (any(!(size %in% 2:3)))
        stop("Only bigram and trigram collocations implemented so far.")
    
    coll <- NULL
    if (2 %in% size)
        coll <- collocations2(x, method, 2, n, punctuation = punctuation)
    if (3 %in% size) {
        if (is.null(coll)) 
            coll <- collocations3(x, method, 3, n, punctuation = punctuation, ...)
        else {
            coll <- rbind(coll, collocations3(x, method, 3, n, punctuation = punctuation, ...))
            class(coll) <- c("collocations", class(coll))
        }
    }
    # remove any "collocations" containing the dummy token, return
    word1 <- word2 <- word3 <- NULL
    coll[word1 != DUMMY_TOKEN & word2 != DUMMY_TOKEN & word3 != DUMMY_TOKEN]
}


collocations3 <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), size=3, n=NULL, 
                          punctuation =  c("dontspan", "ignore", "include"), ...) {
    method <- match.arg(method)
    
    # to not issue the check warnings:
    w1 <- w2 <- w3 <- c123 <- c12 <- c13 <- c1 <- c23 <- c2 <- c3 <- X2 <- G2 <- count <- NULL
    
    t <- x
    
    # remove punctuation if called for
    if (punctuation == "ignore") {
        t <- t[!stringi::stri_detect_regex(w1, "^\\p{P}$")]
    }

    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-2)], 
                            w2 = t[2:(length(t)-1)],
                            w3 = t[3:(length(t))],
                            count = 1)
    
    # eliminate non-adjacent words
    if (punctuation == "dontspan") {
        wordpairs <- wordpairs[!(stringi::stri_detect_regex(w1, "^\\p{P}$") | 
                                 stringi::stri_detect_regex(w2, "^\\p{P}$") |
                                 stringi::stri_detect_regex(w3, "^\\p{P}$"))]
    }

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
        catm("  ...NOTE: dropping duplicates in word1:", w1Table[dups, w1], "\n")
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
        catm("  ...NOTE: dropping duplicates in word2:", w2Table[dups, w2], "\n")
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
        catm("  ...NOTE: dropping duplicates in word3:", w3Table[dups, w3], "\n")
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
#         catm("  ...NOTE: dropping duplicates in word1,2: ... \n", w12Table[dups, w1, w2], "\n")
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
    epsilon <- .000000001  # to offset zero cell counts
    allTable <- within(allTable, {
        # "Model 1": P(w1,w2,w3) = P(w1)P(w2)P(w3)
        m1.111 <- exp(log(n111 + n121 + n112 + n122) + log(n111 + n211 + n112 + n212) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.112 <- exp(log(n111 + n121 + n112 + n122) + log(n111 + n211 + n112 + n212) + log(n112 + n212 + n122 + n222) - 2*log(N))
        m1.121 <- exp(log(n111 + n121 + n112 + n122) + log(n121 + n221 + n122 + n222) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.122 <- exp(log(n111 + n121 + n112 + n122) + log(n121 + n221 + n122 + n222) + log(n112 + n212 + n122 + n222) - 2*log(N))
        m1.211 <- exp(log(n211 + n221 + n212 + n222) + log(n111 + n211 + n112 + n212) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.212 <- exp(log(n211 + n221 + n212 + n222) + log(n111 + n211 + n112 + n212) + log(n112 + n212 + n122 + n222) - 2*log(N))
        m1.221 <- exp(log(n211 + n221 + n212 + n222) + log(n121 + n221 + n122 + n222) + log(n111 + n211 + n121 + n221) - 2*log(N))
        m1.222 <- exp(log(n211 + n221 + n212 + n222) + log(n121 + n221 + n122 + n222) + log(n112 + n212 + n122 + n222) - 2*log(N))
        
        # "Model 2": P(w1,w2,w3) = P(w1,w2)P(w3)
#         m2.111 <- exp(log(n111 + n112) + log(n111 + n211 + n121 + n221) - log(N))
#         m2.112 <- exp(log(n111 + n112) + log(n112 + n212 + n122 + n222) - log(N))
#         m2.121 <- exp(log(n121 + n122) + log(n111 + n211 + n121 + n221) - log(N))
#         m2.122 <- exp(log(n121 + n122) + log(n112 + n212 + n122 + n222) - log(N))
#         m2.211 <- exp(log(n211 + n212) + log(n111 + n211 + n121 + n221) - log(N))
#         m2.212 <- exp(log(n211 + n212) + log(n112 + n212 + n122 + n222) - log(N))
#         m2.221 <- exp(log(n221 + n222) + log(n111 + n211 + n121 + n221) - log(N))
#         m2.222 <- exp(log(n221 + n222) + log(n112 + n212 + n122 + n222) - log(N))
#         
#         # "Model 2": P(w1,w2,w3) = P(w1)P(w2,w3)
#         m3.111 <- exp(log(n111 + n211) + log(n111 + n121 + n112 + n122) - log(N))
#         m3.112 <- exp(log(n112 + n212) + log(n111 + n121 + n112 + n122) - log(N))
#         m3.121 <- exp(log(n121 + n221) + log(n111 + n121 + n112 + n122) - log(N))
#         m3.122 <- exp(log(n122 + n222) + log(n111 + n121 + n112 + n122) - log(N))
#         m3.211 <- exp(log(n111 + n211) + log(n211 + n221 + n212 + n222) - log(N))
#         m3.212 <- exp(log(n112 + n212) + log(n211 + n221 + n212 + n222) - log(N))
#         m3.221 <- exp(log(n121 + n221) + log(n211 + n221 + n212 + n222) - log(N))
#         m3.222 <- exp(log(n122 + n222) + log(n211 + n221 + n212 + n222) - log(N))
        
        lrratioM1 <- 2 * ((n111 * log(n111 / m1.111 + epsilon)) + (n112 * log(n112 / m1.112 + epsilon)) +
                              (n121 * log(n121 / m1.121 + epsilon)) + (n122 * log(n122 / m1.122 + epsilon)) +
                              (n211 * log(n211 / m1.211 + epsilon)) + (n212 * log(n212 / m1.212 + epsilon)) +
                              (n221 * log(n221 / m1.221 + epsilon)) + (n222 * log(n222 / m1.222 + epsilon)))
#         lrratioM2 <- 2 * ((n111 * log((n111 + epsilon) / (m2.111 + epsilon))) + (n112 * log((n112 + epsilon) / (m2.112 + epsilon))) +
#                               (n121 * log((n121 + epsilon) / (m2.121 + epsilon))) + (n122 * log((n122 + epsilon) / (m2.122 + epsilon))) +
#                               (n211 * log((n211 + epsilon) / (m2.211 + epsilon))) + (n212 * log((n212 + epsilon) / (m2.212 + epsilon))) +
#                               (n221 * log((n221 + epsilon) / (m2.221 + epsilon))) + (n222 * log((n222 + epsilon) / (m2.222 + epsilon))))
#         lrratioM3 <- 2 * ((n111 * log((n111 + epsilon) / (m3.111 + epsilon))) + (n112 * log((n112 + epsilon) / (m3.112 + epsilon))) +
#                               (n121 * log((n121 + epsilon) / (m3.121 + epsilon))) + (n122 * log((n122 + epsilon) / (m3.122 + epsilon))) +
#                               (n211 * log((n211 + epsilon) / (m3.211 + epsilon))) + (n212 * log((n212 + epsilon) / (m3.212 + epsilon))) +
#                               (n221 * log((n221 + epsilon) / (m3.221 + epsilon))) + (n222 * log((n222 + epsilon) / (m3.222 + epsilon))))
        
    })

    allTable <- within(allTable, {
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
#         dt$G2m1 <- allTable$lrratioM1
#         dt$G2m2 <- allTable$lrratioM2
#         dt$G2m3 <- allTable$lrratioM3
#         setorder(dt, -G2m1)
        dt$G2 <- allTable$lrratioM1
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



collocations2 <- function(x, method=c("lr", "chi2", "pmi", "dice", "all"), 
                          size=2, n=NULL, 
                          punctuation =  c("dontspan", "ignore", "include"), ...) {
    
    # to not issue the check warnings:
    w1 <- w2 <- count <- w1wn <- w1w2n <- chi2 <- pmi <- dice <- lrratio <- NULL

    method <- match.arg(method)

    t <- x
    
    # remove punctuation if called for
    if (punctuation == "ignore") {
        t <- t[!stringi::stri_detect_regex(t, "^[\\p{P}\\p{S}]$")]
    }

    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-1)], 
                            w2 = t[2:length(t)],
                            count = 1)
    
    # eliminate non-adjacent words (where a blank is in a pair)
    if (punctuation == "dontspan") {
        wordpairs <- wordpairs[!(stringi::stri_detect_regex(w1, "^[\\p{P}\\p{S}]$") | 
                                 stringi::stri_detect_regex(w2, "^[\\p{P}\\p{S}]$"))]
    }
    
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
        catm("  ...NOTE: dropping duplicates in word1:", w1Table[dups, w1], "\n")
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
        catm("...NOTE: dropping duplicates found in word2:", w2Table[dups, w2], "\n")
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
    
    #df[, word1 := factor(word1, levels = 1:length(tlevels), labels = tlevels)]
    #df[, word2 := factor(word2, levels = 1:length(tlevels), labels = tlevels)]
    class(df) <- c("collocations", class(df))
    df[1:ifelse(is.null(n), nrow(df), n), ]
}

