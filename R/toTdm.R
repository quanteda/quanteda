#' convert a dfm to a non-quanteda format
#'
#' Convert a quanteda \link{dfm-class} object to a format useable by
#' other text analysis packages.
#' @param object dfm to be converted
#' @param to target conversion format, one of
#' \describe{
#' \item{"lda"}{a list with components "documents" and "vocab" as needed by 
#'   \link[lda]{lda.collapsed.gibbs.sampler}}
#' \item{"tm"}{a \link[tm]{DocumentTermMatrix} from the \pkg{tm} package} 
#' \item{"stm"}{the data format for the \pkg{\link{stm}} package}
#' \item{"austin"}{the \link[austin]{wfm} format from the \pkg{\link{austin}} package}
#' }
#' @return a converted object determined by the value of \code{to} (see above)
#' @importFrom slam as.simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @importFrom tm weightTf
#' @export
#' @examples
#' mycorpus <- subset(inaugCorpus, Year>1970)
#' quantdfm <- dfm(mycorpus, verbose=FALSE)
#' 
#' # austin's wfm format
#' austindfm <- as.wfm(quantdfm)
#' wftest <- austin::wordfish(austindfm)
#' 
#' # tm's DocumentTermMatrix format
#' tmdfm <- as.DocumentTermMatrix(quantdfm)
#' 
#' # stm package format
#' 
#' 
#' # lda format used by topicmodels package
#' if (require(austin)) austin::is.wfm(austindfm)
# td <- dfm2tmformat(d)
# length(td$v)
# if (require(topicmodels)) (tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 5))
convert <- function(object, ...) {
    UseMethod("convert")
}

#' @export
#' @rdname convert
#' @importFrom topicmodels dtm2ldaformat
convert.dfm <- function(object, to = c("lda", "tm", "stm", "austin"), weighting=weightTf) {
    to <- match.arg(to)
    if (to=="tm") 
        return(dfm2tmformat(object, weighting))
    else if (to=="lda")
        return(dfm2ldaformat(object))
    else if (to=="stm")
        return(dfm2stmformat(object))
    else if (to=="austin")
        return(dfm2austinformat(object))
    
}

#' @export
#' @rdname convert
as.wfm <- function(object) {
    UseMethod("as.wfm")
}


#' @export
#' @rdname convert
as.wfm.dfm <- function(object) {
    convert(object, to = "austin")
}

dfm2austinformat <- function(d) {
    d <- as.matrix(d)
    names(dimnames(d))[2] <- "words"
    class(d) <- c("wfm", "matrix")
    d
}

## convert to tm format
dfm2tmformat <- function(d, weighting=weightTf) {
    sl <- slam::as.simple_triplet_matrix(d)
    td <- tm::as.TermDocumentMatrix(sl, weighting=weighting)
    return(td)
}

#' @export
#' @rdname convert
as.DocumentTermMatrix <- function(x, ...) {
    UseMethod("as.DocumentTermMatrix")
}

#' @export
#' @rdname convert
as.DocumentTermMatrix.dfm <- function(x, weighting=weightTf) {
    convert(x, to="tm", weighting)
}

# @return A list with components "documents" and "vocab" as needed by 
#   \link[lda]{lda.collapsed.gibbs.sampler}
# @import topicmodels
# @export
# @examples
# mycorpus <- subset(inaugCorpus, Year>1970)
# d <- dfm(mycorpus, stopwords=TRUE)
# d <- trim(d, minCount=5, minDoc=3)
# td <- dfm2ldaformat(d)
# if (require(lda)) {
#     tmodel.lda <- lda.collapsed.gibbs.sampler(documents=td$documents, 
#                                               K=10,  
#                                               vocab=td$vocab,
#                                               num.iterations=50, alpha=0.1, eta=0.1) 
#     top.topic.words(tmodel.lda$topics, 10, by.score=TRUE) # top five words in each topic
# }
dfm2ldaformat <- function(d) {
    tmDTM <- dfm2tmformat(d)
    return(topicmodels::dtm2ldaformat(tmDTM))
}


# convert a dfm to stm's input document format
#
# Convert a quanteda dfm object into the indexed format needed for estimating
# a structural topic model from the \pkg{stm} package using \link[stm]{stm}.
# @param data dfm object to be converted
# @note
# Meta-data will need to be passed separately to \link[stm]{stm} as this 
# information is not included in a dfm object.
# @return A list containing the following elements:
# \item{documents}{A list containing the documents in the stm format.}
# \item{vocab}{Character vector of vocabulary.}
# \item{meta}{NULL} 
# @examples
# mydfm <- dfm(inaugTexts)
# mydfmStm <- dfm2stmformat(mydfm)
# str(mydfmStm)
# @export
dfm2stmformat <- function(data) {
    sortedData <- data[, order(features(data))]
    vocab <- features(sortedData)
    stmdocs <- list()
    length(stmdocs) <- ndoc(data)
    names(stmdocs) <- docnames(data)
    for (d in docnames(data)) {
        temp <- as.matrix(rbind(1:length(vocab), as.integer(as.matrix(data[d, ]))))
        stmdocs[[d]] <- temp[, which(temp[2, ] > 0), drop=FALSE]
    }
    list(documents=stmdocs, vocab=vocab, meta=NULL)
}


