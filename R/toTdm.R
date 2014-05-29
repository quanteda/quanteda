#' Convert a quanteda \code{\link{dfm}} (document feature matrix) 
#'  into a 
#'  \pkg{tm} \link[tm:DocumentTermMatrix]{DocumentTermMatrix}
#'
#' \pkg{tm} represents document-feature matrixes as sparse matrixes, 
#' in the \link[slam:matrix]{simple triplet matrix} format of the package \pkg{slam}. 
#' This function converts a \code{dfm} into a \link[tm:DocumentTermMatrix]{DocumentTermMatrix}, 
#' for working with the \code{dfm} in \pkg{tm} or in other packages that expect this format,
#' such as \pkg{topicmodels}.
#' 
#' 
#' @param d A \link[quanteda]{dfm} object
#' @param weight \pkg{tm}'s coercion function accepts weightings such as tf-idf, see \pkg{tm}'s 
#'  \link[tm]{as.DocumentTermMatrix} for a list of possible arguments. The default is just tf (term frequency)
#' @return A triplet matrix of class \link[tm]{as.DocumentTermMatrix}
#' @export
#' @examples
#' data(iebudgets)
#' iebudgets2010 <- subset(iebudgets, year==2010)
#' d <- dfmTrim(dfm(iebudgets2010), minCount=5, minDoc=3)
#' dim(d)
#' td <- dfm2tmformat(d)
#' length(td$v)
#' if (require(topicmodels)) tmodel.lda <- LDA(td, control = list(alpha = 0.1), k = 4)
dfm2tmformat <- function(d, weighting=weightTf, ...){
    require(slam)
    require(tm)
    sl <- as.simple_triplet_matrix(d)
    td <- as.TermDocumentMatrix(sl, weighting=weighting)
    return(td)
}

#' Convert a quanteda \code{\link{dfm}} (document feature matrix) 
#'  into a the data format needed by \link{\pkg{lda}}
#'
#' @param d A \code{\link{dfm}} object
#' @return A list with components "documents" and "vocab" as needed by \link[pkg]{lda.collapsed.gibbs.sampler}
#' @export
#' @examples
#' data(iebudgets)
#' iebudgets2010 <- subset(iebudgets, year==2010)
#' d <- dfmTrim(dfm(iebudgets2010), minCount=5, minDoc=3)
#' td <- dfm2ldaformat(d)
#' if (require(lda)) {
#'     tmodel.lda <- result <- lda.collapsed.gibbs.sampler(documents=td$documents, 
#'                                                         K=10,  
#'                                                         vocab=td$vocab,
#'                                                         num.iterations=50, alpha=0.1, eta=0.1) 
#' }
#' top.topic.words(tmodel.lda$topics, 10, by.score=TRUE) # top five words in each topic
dfm2ldaformat <- function(d) {
    tmDTM <- dfm2tmformat(d)
    return(dtm2ldaformat(tmDTM))
}


## taken from the topicmodels package
dtm2ldaformat <- function(x, omit_empty = TRUE) {
    split.matrix <- 
        function (x, f, drop = FALSE, ...) 
            lapply(split(seq_len(ncol(x)), f, drop = drop, ...),
                   function(ind) x[,ind, drop = FALSE])
    
    documents <- vector(mode = "list", length = nrow(x))
    names(documents) <- rownames(x)
    documents[rowSums(x) > 0] <- split(rbind(as.integer(x$j) - 1L, as.integer(x$v)), as.integer(x$i))
    if (omit_empty)
        documents[rowSums(x) == 0] <- NULL
    else 
        documents[rowSums(x) == 0] <- rep(list(matrix(integer(), ncol = 0, nrow = 2)), sum(rowSums(x) == 0))
    list(documents = documents,
         vocab = colnames(x))
}
