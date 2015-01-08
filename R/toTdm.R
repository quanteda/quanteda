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
#' d <- trimdfm(dfm(mycorpus), minCount=5, minDoc=3)
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
#' d <- trimdfm(d, minCount=5, minDoc=3)
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


