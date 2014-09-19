
# with scan

#' tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @rdname tokenize
#' @aliases tokenise
#' @param x The text(s) or corpus to be tokenized
#' @param ... additional arguments passed to \code{\link{clean}}
#' @return A list of length \code{\link{ndoc}(x)} of the tokens found in each text.
#' @export
#' @examples 
#' # same for character vectors and for lists
#' tokensFromChar <- tokenize(inaugTexts)
#' tokensFromCorp <- tokenize(inaugCorpus)
#' identical(tokensFromChar, tokensFromCorp)
#' str(tokensFromChar)
#' 
tokenize <- function(x, ...) {
    UseMethod("tokenize")
}

#' @S3method tokenize character
#' @rdname tokenize
#' @param simplify If \code{TRUE}, return a character vector of tokens rather 
#'   than a list of length \code{\link{ndoc}(texts)}, with each element of the 
#'   list containing a character vector of the tokens corresponding to that
#'   text.
#' @return  A list of length \code{\link{ndoc}(texts)} of the tokens found in 
#'   each text.
#' @export
#' @examples 
#' # returned as a list
#' head(tokenize(inaugTexts[57])[[1]], 10)
#' # returned as a character vector using simplify=TRUE
#' head(tokenize(inaugTexts[57], simplify=TRUE), 10)
#' 
#' # demonstrate some options with clean
#' head(tokenize(inaugTexts[57], simplify=TRUE, lower=FALSE), 30)
tokenize.character <- function(x, simplify=FALSE, sep=" ", ... ) {
    # function to tokenize a single element character
    # profiling shows that using scan is 3x faster than using strsplit
    tokenizeSingle <- function(s, sep=" ", ...) {
        s <- clean(s, ...)
        # s <- unlist(s)
        tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
        return(tokens)
    }
    
    # apply to each texts, return a list
    result <- lapply(x, tokenizeSingle, sep, ...)
    
    # remove empty "tokens" caused by multiple whitespace characters in sequence
    result <- lapply(result, function(x) x[which(x != "")])
    
    #if (simplify | length(result)==1) {
    # change to a character vector of tokens if simplify==TRUE 
    # this will concatenate the token lists if length(result)>1
    if (simplify) {
        result <- unlist(result, use.names=FALSE)
    }
    return(result)
}

#' @S3method tokenize corpus
#' @rdname tokenize
#' @export
tokenize.corpus <- function(corpus, ...) {
    # get the settings for clean from the corpus and use those, 
    # unless more specific arguments are passed -- ADD THE ABILITY TO PASS THESE
    # need to include sep in this list too 
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

