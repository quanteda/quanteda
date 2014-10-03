

#' resampling methods for a corpus
#' 
#' Draw a set of random resamples from a corpus object, at a specified level of 
#' resampling, and record additional "resampled texts" as document-level
#' metadata, stored as \code{_resampleXXX} for the XXXth resample.
#' @param x corpus object containing the texts to be resampled
#' @param n number of resamples to be drawn
#' @param unit resampling unit for drawing the random samples, can be
#'   \code{sentences} or \code{paragraphs}.
#' @param ... additional arguments passed to \code{\link{segment}}
#' @return a corpus object containing new resampled texts.
#' @examples 
#' testCorp <- resample(subset(inaugCorpus, Year>2000), n=10, "sentences")
#' testCorpPara <- resample(corpus(uk2010immig), n=10, "paragraphs")
#' names(metadoc(testCorp))
#' x <- corpus(c("Sentence One C1.  Sentence Two C1.  Sentence Three C1.", 
#'               "Sentence One C2.  Sentence Two C2.  Sentence Three C2. Sentence Four C2.  Sentence Five C2.  Sentence Six C2."),
#'             docnames=c("docTwo", "docOne"))
#' testRS <- resample(x, n=3)
#' metadoc(testRS)
#' testRS$documents
#' @note Additional resampling units to be added will include fixed length
#'   samples and random length samples.
#' @export
resample <- function(x, ...) {
    UseMethod("resample")
}
    
#' @rdname resample
#' @export
resample.corpus <- function(x, n=100, unit=c("sentences", "paragraphs"), ...) {
    unit <- match.arg(unit)
    
    # add a document serial number
    metadoc(x, "docID") <- 1:ndoc(x)
    unitCorpus <- changeunits(x, unit, ...)
    
    unitTexts <- as.data.table(unitCorpus$documents[, c("texts", "_docID")])
    setkeyv(unitTexts, "_docID")
    
    # resampling loop
    for (i in 1:n) {
         temp <- unitTexts[, paste(sample(texts, replace=TRUE), collapse=" "), by="_docID"]
         metadoc(x, paste("resample", i, sep="")) <- temp$V1
    }
    
    return(x)
} 

