
# with scan
# profiling shows that using scan is 3x faster than using strsplit
#' @export
tokenizeSingle <- function(s, clean=TRUE){
  if (clean) {
      s <- clean(s)
  }
  s <- unlist(s)
  tokens <- scan(what="char", text=s, quiet=TRUE)
  return(tokens)
}


#' @export
tokenize <- function(x, ...) {
  UseMethod("tokenize")
}

#' @export
tokenize.character <- function(text, clean=TRUE, simplify=FALSE){
    result <- lapply(text, tokenizeSingle, clean=clean)
    if (simplify | length(result)==1) {
        result <- unlist(result)
    }
    return(result)
}


#' @export
tokenize.corpus <- function(corpus, clean=TRUE){
  tokens(corpus) <- tokenize(texts(corpus), clean)
  return(corpus)
}


########
########  These functions grab the settings and tokenise the corpus
########  and store the results in a list called tokens
########

#' tokenise a corpus and store the results
#' 
#' Tokenize the corpus and store the results as a dfm, along with counts for the syllables of each token, 
#' the number of sentences per text, and the number of paragraphs per text.
#' @export
tokenise <- function(x, ...) {
    UseMethod("tokenise")
}

#' @param corp Corpus to be tokenized
#' @export
#' @S3method tokenise corpus
#' @rdname tokenise
#' @return \item{tokens}{A list consisting of the following:}
#' @return \item{$dfm}{A \link{dfm} document-feature matrix object created with \link{settings}.}
#' @return \item{$nwords}{A vector of token counts for each document.}
#' @return \item{$ntypes}{A vector of type counts for each document.}
#' @return \item{$nsents}{A vector of sentence counts for each document.}
#' @return \item{$nparagr}{A vector of paragraph counts for each document.}
#' @examples
#' mycorpus <- corpus(uk2010immig)
#' mycorpus
#' tokenise(mycorpus)
#' mycorpus
tokenise.corpus <- function(corp) {
    thisdfm <- dfm(corp, 
                   stem=settings(corp, "stem"),
                   stopwords=settings(corp, "stopwords"),
                   dictionary=settings(corp, "dictionary"),
                   dictionary_regex=settings(corp, "dictionary_regex"))
    nwords <- rowSums(thisdfm)
    ntypes <- rowSums(thisdfm>0)
    nsents <- sapply(texts(corp), 
                     function(s) length(gregexpr(paste("[", settings(corp, "delimiter_sentence"), "]", sep=""), s)[[1]]))
    nparagr <- sapply(texts(corp), 
                       function(s) length(gregexpr(paste("[", settings(corp, "delimiter_paragraph"), "]", sep=""), s)[[1]]))
    
    corpusName <- deparse(substitute(corp))
    env <- parent.frame()
    env[[corpusName]]$tokens <- list(dfm=thisdfm, nwords, ntypes, nsents, nparagr)
}
