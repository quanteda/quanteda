#' create a sparse matrix dfm
#' 
#' Create a sparse matrix dfm from a vector of texts.
#' @param x Character vector from which to generate the document-feature matrix
#' @param ... additional arguments passed to \code{\link{clean}}
#' @return A specially classed \link[Matrix]{Matrix} object with row names equal to the 
#'   document names and column names equal to the feature labels.
#' @author Kenneth Benoit
#' @export
#' @examples
#' # with inaugural texts
#' dfmsInaug <- dfms(inaugTexts)
#' (size1 <- object.size(dfmsInaug))
#' (size2 <- object.size(dfm(inaugTexts)))
#' cat("Compacted by ", round(as.numeric((1-size1/size2)*100), 1), "%.\n", sep="")
#'
#' \dontrun{
#' # try it with approx 35,000 court documents from Lauderdale and Clark (200?)
#' load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
#' txts <- unlist(Opinion_files[1])
#' names(txts) <- NULL
#' 
#' # dfms without cleaning
#' require(Matrix)
#' system.time(dfmsBig <- dfms(txts, clean=FALSE, verbose=FALSE))
#' object.size(dfmsBig)
#' dim(dfmsBig)
#' # compare with tm
#' require(tm)
#' tmcorp <- VCorpus(VectorSource(txts))
#' system.time(tmDTM <- DocumentTermMatrix(tmcorp))
#' object.size(tmDTM)
#' dim(tmDTM)
#'  
#' # with cleaning - the gsub() calls in clean() take a long time
#' system.time(dfmsBig <- dfms(txts, clean=TRUE, additional="[-_—]")) 
#' object.size(dfmsBig)
#' dim(dfmsBig) 
#' # 100 top features
#' topf <- colSums(dfmsBig)
#' names(topf) <- colnames(dfmsBig)
#' head(sort(topf, decreasing=TRUE), 100)
#' }
dfms <- function(x, verbose=TRUE, clean=TRUE, ...) {
    require(Matrix)
    if (verbose) cat("Creating dfm from character vector ...")
        
    if (verbose) cat("\n   ... indexing documents")
    docIndex <- 1:length(x)
    if (is.null(names(x))) 
        names(docIndex) <- factor(paste("text", 1:length(x), sep="")) else
            names(docIndex) <- names(x)
    
    if (verbose) cat("\n   ... tokenizing texts")
    tokenizedTexts <- lapply(x, tokenizeSingle, sep=" ")
    
    if (verbose) cat("\n   ... shaping tokens into data.table")
    alltokens <- data.table(docIndex = rep(docIndex, sapply(tokenizedTexts, length)),
                            features = unlist(tokenizedTexts))
    
    if (clean) {
        if (verbose) cat("\n   ... cleaning the tokens")
        alltokens$features <- clean(alltokens$features, ...)
    }
    alltokens <- alltokens[features!=""]
    
    if (verbose) cat("\n   ... summing tokens by document")
    alltokens[, n:=1L]
    alltokens <- alltokens[, by=list(docIndex,features), sum(n)]
    
    if (verbose) cat("\n   ... indexing features")
    uniqueFeatures <- sort(unique(unlist(alltokens$features)))
    # much faster than using factor(alltokens$features, levels=uniqueFeatures) !!
    featureTable <- data.table(featureIndex = 1:length(uniqueFeatures),
                               features = uniqueFeatures)
    setkey(alltokens, features)
    setkey(featureTable, features)
    # merge, data.table style.  warnings suppressed or it moans about mixed encodings
    suppressWarnings(alltokens <- alltokens[featureTable])
    
    if (verbose) cat("\n   ... building sparse matrix")
    suppressWarnings(dfmsparse <- sparseMatrix(i = alltokens$docIndex, 
                                               j = alltokens$featureIndex, 
                                               x = alltokens$V1, 
                                               dimnames=list(docs=names(docIndex), features=uniqueFeatures)))
    # class(dfmsparse) <- c("dfms", class(dfmsparse))
    # NEED ANOTHER CLASS METHODS SINCE THIS IS S4
    
    if (verbose) cat(" ... done.\n")
    return(dfmsparse)
}


tokenizeSingle <- function(s, sep=" ", useclean=FALSE, ...) {
    if (useclean) s <- clean(s, ...)
    # s <- unlist(s)
    tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
    return(tokens)
}





