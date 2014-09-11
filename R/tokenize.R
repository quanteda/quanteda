
# with scan
# profiling shows that using scan is 3x faster than using strsplit
#' @export
tokenizeSingle <- function(s, clean=FALSE) {
    if (clean) {
        s <- clean(s)
    }
    s <- unlist(s)
    tokens <- scan(what="char", text=s, quiet=TRUE)
    return(tokens)
}

#' tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @return A list of length \code{\link{ndoc}(texts)} of the tokens found in each text.
#' @export
tokenize <- function(x, ...) {
    UseMethod("tokenize")
}

#' @S3method tokenize character
#' @rdname tokenize
#' @export
tokenize.character <- function(text, clean=FALSE, simplify=FALSE) {
    # should pass ... from parent functions, so that these can be
    # passed to clean().  I suggest not using "clean" as an argument.
    # instead, let the clean() defaults come into effect, unless
    # overrides are passed through ...
    result <- lapply(text, tokenizeSingle, clean)
    if (simplify | length(result)==1) {
        result <- unlist(result)
    }
    return(result)
}

#' @S3method tokenize corpus
#' @rdname tokenize
#' @export
tokenize.corpus <- function(corpus, ...) {
    # get the settings for clean from the corpus and use those, 
    # unless more specific arguments are passed -- ADD THE ABILITY TO PASS THESE
    tokenize(texts(corpus), ...)
}


########
########  These functions grab the settings and tokenise the corpus
########  and store the results in a list called tokens
########


#' preprocess the tokens in a corpus
#'
#' Applies pre-processing rules to the text and compiles a frequency table of features (word types)
#' including counts of types, tokens, sentences, and paragraphs.
#' @note This will eventually become an
#' indexing function.  At the moment it creates and saves a \link{dfm} in addition to 
#' some summary information compiled from this, in order to speed up subsequent processing.
#' Unlike most R functions which return a value, this one changes the object passed
#' to it.  (And they say R can't pass by reference...)
#' @param corp Corpus to be preprocessed
#' @return no return but modifies the object in place by changing
#' @return \item{tokens, }{a list consisting of the following:}
#' @return \item{$dfm}{A \link{dfm} document-feature matrix object created with \link{settings}.}
#' @return \item{$nwords}{A vector of token counts for each document.}
#' @return \item{$ntypes}{A vector of type counts for each document.}
#' @return \item{$nsents}{A vector of sentence counts for each document.}
#' @return \item{$nparagr}{A vector of paragraph counts for each document.}
# @export
#' @examples
#' mycorpus <- corpus(uk2010immig)
#' mycorpus
#' preprocess(mycorpus)
#' mycorpus
#' mydfm <- dfm(mycorpus)
preprocess <- function(corp) {
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
    env[[corpusName]]$tokens <- list(dfm=thisdfm, nwords=nwords, ntypes=ntypes, nsent=nsents, nparagr=nparagr)
}

