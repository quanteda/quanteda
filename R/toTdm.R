#' Convert a \link{dfm} into a \pkg{tm} \link[tm]{DocumentTermMatrix}
#' 
#' \pkg{tm} represents sparse document-feature matrixes in the 
#' \link[slam:matrix]{simple triplet matrix} format of the package \pkg{slam}. 
#' This function converts a \code{dfm} into a 
#' \code{\link[tm]{DocumentTermMatrix}}, enabling a dfm to be used with other 
#' packages that expect this format, such as \pkg{topicmodels}.
#' @param d A \link[quanteda]{dfm} object
#' @param weighting weight function  arguments passed to 
#'   \code{as.TermDocumentMatrix}, defaults to term frequency (see 
#'   \code{\link[tm]{as.DocumentTermMatrix}} for a list of options, such as
#'   tf-idf).
#' @return A simple triplet matrix of class \link[tm]{as.DocumentTermMatrix}
#' @importFrom slam as.simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @importFrom tm weightTf
#' @export
#' @examples
#' mycorpus <- subset(inaugCorpus, Year>1970)
#' d <- trim(dfm(mycorpus), minCount=5, minDoc=3)
#' dim(d)
#' td <- dfm2tmformat(d)
#' length(td$v)
#' if (require(topicmodels)) (tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 5))
dfm2tmformat <- function(d, weighting=weightTf){
    sl <- slam::as.simple_triplet_matrix(d)
    td <- tm::as.TermDocumentMatrix(sl, weighting=weighting)
    return(td)
}

#' Convert a \link{dfm} into the format needed by \pkg{lda}
#' 
#' Convert a quanteda \link{dfm} object into the indexed format required by the
#' topic modelling package \pkg{lda}.
#' @param d A \code{\link{dfm}} object
#' @return A list with components "documents" and "vocab" as needed by 
#'   \link[lda]{lda.collapsed.gibbs.sampler}
#' @import topicmodels
#' @export
#' @examples
#' mycorpus <- subset(inaugCorpus, Year>1970)
#' d <- dfm(mycorpus, stopwords=TRUE)
#' d <- trim(d, minCount=5, minDoc=3)
#' td <- dfm2ldaformat(d)
#' if (require(lda)) {
#'     tmodel.lda <- lda.collapsed.gibbs.sampler(documents=td$documents, 
#'                                               K=10,  
#'                                               vocab=td$vocab,
#'                                               num.iterations=50, alpha=0.1, eta=0.1) 
#'     top.topic.words(tmodel.lda$topics, 10, by.score=TRUE) # top five words in each topic
#' }
dfm2ldaformat <- function(d) {
    tmDTM <- dfm2tmformat(d)
    return(topicmodels::dtm2ldaformat(tmDTM))
}


#' convert a dfm to stm's input document format
#'
#' Convert a quanteda dfm object into the indexed format needed for estimating
#' a structural topic model from the \pkg{stm} package using \link[stm]{stm}.
#' @param data dfm object to be converted
#' @note
#' Meta-data will need to be passed separately to \link[stm]{stm} as this 
#' information is not included in a dfm object.
#' @return A list containing the following elements:
#' \item{documents}{A list containing the documents in the stm format.}
#' \item{vocab}{Character vector of vocabulary.}
#' \item{meta}{NULL} 
#' @examples
#' mydfm <- dfm(inaugTexts)
#' mydfmStm <- dfm2stmformat(mydfm)
#' str(mydfmStm)
#' @export
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


