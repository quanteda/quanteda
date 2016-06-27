#' detect the encoding of texts
#' 
#' Detect the encoding of texts in a character, \link{corpus}, or 
#' \link{corpusSource-class} object and report on the most likely encoding.  Useful in
#' detecting the encoding of input texts, so that a source encoding can be 
#' (re)specified when inputting a set of texts using \code{\link{textfile}}, prior
#' to constructing a corpus.
#' 
#' Based on \link[stringi]{stri_enc_detect}, which is in turn based on the 
#' ICU libraries.  See the ICU User Guide, 
#' \url{http://userguide.icu-project.org/conversion/detection}.
#' @param x character vector, corpus, or corpusSource object whose texts' 
#'   encodings will be detected.
#' @param verbose if \code{FALSE}, do not print diagnostic report
#' @param ... additional arguments passed to \link[stringi]{stri_enc_detect}
#' @examples 
#' encoding(encodedTexts)
#' # show detected value for each text, versus known encoding
#' data.frame(labelled = names(encodedTexts), detected = encoding(encodedTexts)$all)
#' 
#' encoding(ukimmigTexts)
#' encoding(inaugCorpus)
#' encoding(ie2010Corpus)
#' 
#' \dontrun{# Russian text, Windows-1251
#' mytextfile <- textfile("http://www.kenbenoit.net/files/01_er_5.txt", cache = FALSE)
#' encoding(mytextfile)}
#' @export
encoding <- function(x, verbose = TRUE, ...) {
    UseMethod("encoding")
}

#' @rdname encoding
#' @export
encoding.character <- function(x, verbose = TRUE, ...) {

    addedArgs <- names(list(...))
    if (length(addedArgs) && any(!(addedArgs %in% names(formals(stringi::stri_enc_detect)))))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), addedArgs, " not used.", sep = "", noBreaks. = TRUE)

    confidence <- conf <- NULL
    n <- 1

    # assign names if none
    if (is.null(names(x))) 
        names(x) <- paste("text", 1:length(x), sep="")
    
    # detect encoding
    detectedEncodings <- stringi::stri_enc_detect(x, ...)
    dt <- data.table(text = rep(names(x), each=n),
                     encoding = unlist(lapply(detectedEncodings, function(x) x$Encoding[1:n])),
                     rank = rep(1:n, length(x)),
                     confidence = unlist(lapply(detectedEncodings, function(x) x$Confidence[1:n])))
    
    conftable <- dt[, mean(confidence), by = encoding]
    conftable <- conftable[!is.na(encoding)]
    setnames(conftable, "V1", "conf")
    conftable[, conf := conf/sum(conf)]
    conftable <- conftable[order(-conf)]
    
    # what are the top encodings
    topEncodingsTable <- dt[rank==1, mean(confidence), by = encoding]
    topEncodingsTable <- topEncodingsTable[!is.na(encoding)]
    setnames(topEncodingsTable, "V1", "conf")
    topEncodingsTable[, conf := conf/sum(conf)]
    topEncodingsTable <- topEncodingsTable[order(-conf)]
        
    if (verbose) catm("Probable encoding: ", topEncodingsTable[1, encoding], sep = "")
    if (nrow(topEncodingsTable) == 1 & topEncodingsTable[1, encoding] == "ISO-8859-1")
        if (verbose) catm("\n  (but note: detector often reports ISO-8859-1 when encoding is actually UTF-8.)")
    if (nrow(topEncodingsTable) > 1) {
        if (verbose) 
            catm("   (but ", "other encodings",
            # paste(topEncodingsTable[2:nrow(topEncodingsTable), encoding], collapse = ", "),
            " also detected)\n", sep = "")
        barsize <- 60
        proportions <- round(topEncodingsTable$conf * barsize)
        plotsymbols <- c("*", "-", ".", "~", letters[1:5])
        if (verbose) catm("  Encoding proportions: ")
        if (verbose) catm("[", rep(plotsymbols[1:nrow(topEncodingsTable)], proportions[1:nrow(topEncodingsTable)]), "]", sep="")
#         spacer <- "\n                         "
#         catm(spacer)
#         catm(paste(topEncodingsTable$encoding, " (", 
#                   format(topEncodingsTable$conf, nsmall=3, digits=3), ") ", plotsymbols[1:nrow(topEncodingsTable)],
#                   collapse = spacer, sep=""))
        if (verbose) {
            catm("\n  Samples of the first text as:\n")
            for (i in 1:nrow(topEncodingsTable)) {
                catm(sprintf("%-21s", paste0("  [", plotsymbols[i], "] ",
                                            topEncodingsTable$encoding[i])),
                    stri_sub(suppressWarnings(stri_encode(x[1], topEncodingsTable$encoding[i])), length = 60),
                    "\n", sep = "")
            }
        }
    } else 
        if (verbose) catm("\n")

    invisible(list(probably = topEncodingsTable[1, encoding], 
                   all = sapply(detectedEncodings, function(x) x$Encoding[1])))
}


#' @rdname encoding
#' @export
encoding.corpus <- function(x, verbose = TRUE, ...) {
    if (verbose) show(x)
    encoding(texts(x), verbose, ...)    
}

#' @rdname encoding
#' @export
encoding.corpusSource <- function(x, verbose = TRUE, ...) {
    if (verbose) show(x)
    encoding(texts(x), verbose, ...)    
}


##
## previously from corpus.R, no longer used
##

# get the encoding of documents in a corpus
# 
# Get or set the \code{_encoding} document-level metadata field(s) in a corpus.
# @param x a corpus object
# @param drop  return as a vector if \code{TRUE}, otherwise return a \code{data.frame}
# @details This function modifies the \code{_encoding} value set by 
#   \code{\link{metadoc}}.  It is a wrapper for \code{metadoc(corp, "encoding")}.
#   
# @note This function differs from R's built-in \link{Encoding} function, which
#   only allows the four values of "latin1", "UTF-8", "bytes", and "unknown"
#   (and which assigns "unknown" to any text that contains only ASCII characters).
#   Legal values for encodings must be from \link{iconvlist}.  Note that encoding
#   does not convert or set encodings, it simply records a user declaration of a 
#   valid encoding.  (We hope to implement checking and conversion later.)
# @export
# encoding <- function(x, drop=TRUE) {
#     if ("_encoding" %in% names(metadoc(x))) {
#         result <- metadoc(x, "encoding") 
#         return(result[,1, drop=drop])
#     } else
#         return(rep(NULL, ndoc(x)))
# }

# @param value a character vector or scalar representing the new value of the encoding (see Note)
# @rdname encoding
# @export
# "encoding<-" <- function(x, value){
#     metadoc(x, "encoding") <- value
#     x
# }
