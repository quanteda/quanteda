
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

#' @rdname tokenize
#' @param simplify If \code{TRUE}, return a character vector of tokens rather 
#'   than a list of length \code{\link{ndoc}(texts)}, with each element of the 
#'   list containing a character vector of the tokens corresponding to that 
#'   text.
#' @param sep by default, tokenize expects a "white-space" delimiter between
#'   tokens. Alternatively, \code{sep} can be used to specify another character
#'   which delimits fields.
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
        
        # s <- unlist(s)
        tokens <- scan(what="char", text=s, quiet=TRUE, quote="", sep=sep)
        tokens <- clean(tokens, ...)
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

#' @rdname tokenize
#' @export
tokenize.corpus <- function(x, ...) {
    # get the settings for clean from the corpus and use those, 
    # unless more specific arguments are passed -- ADD THE ABILITY TO PASS THESE
    # need to include sep in this list too 
    tokenize(texts(x), ...)
}

#' @rdname segment
#' @return \code{segmentSentence} returns a character vector of sentences that
#'   have been segmented
#' @export
#' @examples
#' # segment sentences of the UK 2010 immigration sections of manifestos
#' segmentSentence(uk2010immig[1])[1:5]   # 1st 5 sentences from first (BNP) text
#' str(segmentSentence(uk2010immig[1]))   # a 143-element char vector
#' str(segmentSentence(uk2010immig[1:2])) # a 155-element char vector (143+ 12)
#' 
segmentSentence <- function(x, delimiter="[.!?:;]") {
    # strip out CRs and LFs, tabs
    text <- gsub("\\n+|\\t+", " ", x)
    # remove trailing and leading spaces
    text <- gsub("^ +| +$", "", text)
    
    # remove . delimiter from common title abbreviations
    exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph",  
                    "M", "MM")
    findregex <- paste("\\b(", paste(exceptions, collapse="|"), ")\\.", sep="")
    text <- gsub(findregex, "\\1", text)

    # deal with i.e. e.g. pp. p. Cf. cf.
    text <- gsub("i\\.e\\.", "_IE_", text)
    text <- gsub("e\\.g\\.", "_EG_", text)
    text <- gsub("(\\b|\\()(p\\.)", "\\1_P_", text)
    text <- gsub("(\\b|\\()(pp\\.)", "\\1_PP_", text)
    text <- gsub("(\\b|\\()([cC]f\\.)", "\\1_CF_", text)
    
    exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph", "M", "MM")
    findregex <- paste("\\b(", paste(exceptions, collapse="|"), ")\\.", sep="")
    text <- gsub(findregex, "\\1", text)
    
    # preserve decimals - also i.e. pp. p. e.g. etc.
    numbersWithDecimalsregex <- "([\\d])\\.([\\d])"
    text <- gsub(numbersWithDecimalsregex, "\\1_DECIMAL_\\2", text, perl=TRUE)
    
    # preserve ellipses
    text <- gsub("\\.{3}", "_ELIPSIS_", text)
    
    # recover punctuation characters
    tkns <- tokenize(text, removePunct=FALSE, simplify=TRUE)
    punctpos <- grep(paste(delimiter, "$", sep=""), tkns)
    puncts <- substr(tkns[punctpos], nchar(tkns[punctpos]), nchar(tkns[punctpos]))
    
    # split the text into sentences
    sentences <- unlist(strsplit(text, delimiter))
    # paste punctuation marks back onto sentences
    result <- paste(sentences, puncts, sep="")
    # put decimals back
    result <- gsub("_DECIMAL_", "\\.", result)
    # put elipses back
    result <- gsub("_ELIPSIS_", "...", result)
    
    # put i.e. e.g. pp. p. Cf. cf. back
    result <- gsub("_IE_", "i.e.", result)
    result <- gsub("_EG_", "e.g.", result)
    result <- gsub("(\\b|\\()_P_", "\\1p.", result)
    result <- gsub("(\\b|\\()_PP_", "\\1pp.", result)
    result <- gsub("(\\b|\\()_CF_", "\\1cf.", result)
    
    # remove leading and trailing spaces and return
    gsub("^ +| +$", "", result)
}

#' @rdname segment
#' @return \code{segmentParagraph} returns a character vector of paragraphs that
#'   have been segmented
#' @export
#' @examples
#' # segment paragraphs 
#' segmentParagraph(uk2010immig[3])[1:2]   # 1st 2 Paragraphs from 3rd (Con) text
#' str(segmentParagraph(uk2010immig[3]))   # a 12-element char vector
#' 
#' @export
segmentParagraph <- function(x, delimiter="\\n{2}") {
    unlist(strsplit(x, delimiter))
}


#' segment texts into component elements
#' 
#' Segment text(s) into tokens, sentences, paragraphs, or other sections. 
#' \code{segment} works on a character vector or corpus object, and allows the 
#' delimiters to be defined.  See details.
#' @param x text or corpus object to be segmented
#' @param ... provides additional arguments to be passed to \link{clean}
#' @return A list of segmented texts, with each element of the list correponding
#'   to one of the original texts.
#' @details Tokens are delimited by whitespace.  For sentences, the delimiter 
#'   can be defined by the user.  The default for sentences includes \code{.}, 
#'   \code{!}, \code{?}, plus \code{;} and \code{:}.
#'   
#'   For paragraphs, the default is two carriage returns, although this could be
#'   changed to a single carriage return by changing the value of 
#'  \code{delimiter} to \code{"\\\n{1}"} which is the R version of the 
#'   \link{regex} for one newline character.  (You might 
#'   need this if the document was created in a word processor, for instance, 
#'   and the lines were wrapped in the window rather than being hard-wrapped 
#'   with a newline character.)
#' @export
segment <- function(x, ...) {
    UseMethod("segment")
}

#' @rdname segment
#' @param what unit of segmentation.  Current options are tokens, sentences,
#'   paragraphs, and other.  Segmenting on \code{other} allows segmentation of a
#'   text on any user-defined value, and must be accompanied by the
#'   \code{delimiter} argument.
#' @param delimiter  delimiter defined as a \link{regex} for segmentation. Each 
#'   type has its own default, except \code{other}, which requires a value to be
#'   specified.
#' @export
#' @examples
#' # same as tokenize()
#' identical(tokenize(uk2010immig, lower=FALSE), segment(uk2010immig, lower=FALSE))
#' 
#' # segment into paragraphs
#' segment(uk2010immig[3:4], "paragraphs")
#' 
#' # segment a text into sentences
#' segmentedChar <- segment(uk2010immig, "sentences")
#' segmentedChar[2]
segment.character <- function(x, what=c("tokens", "sentences", "paragraphs", "other"), 
                              delimiter=ifelse(what=="tokens", " ", 
                                               ifelse(what=="sentences", "[.!?:;]", "\\n{2}")),
                              ...) {
    what <- match.arg(what)
    if (what=="tokens") {
        return(tokenize(x, sep=delimiter, ...)) 
    } else if (what=="sentences") {
        return(lapply(x, segmentSentence, delimiter)) 
    } else if (what=="paragraphs") {
        return(lapply(x, segmentParagraph, delimiter)) 
    } else if (what=="other") {
        if (!("delimiter" %in% names(list(...))))
            stop("For type other, you must supply a delimiter value.")
        return(lapply(x, segmentParagraph, delimiter)) 
    }
}

#' @rdname segment
#' @export
#' @examples
#' # segment a corpus into sentences
#' segmentedCorpus <- segment(corpus(uk2010immig), "sentences")
#' identical(segmentedCorpus, segmentedChar)
segment.corpus <- function(x, what=c("tokens", "sentences", "paragraphs", "other"), 
                           delimiter=ifelse(what=="tokens", " ", 
                                            ifelse(what=="sentences", "[.!?:;]", "\\n{2}")),
                           ...) {
    segment(texts(x), what, delimiter, ...)
}

# segment(uk2010immig[1], removePunct=FALSE, simplify=TRUE)
# segment(uk2010immig[1], what="sentences")
# segment(uk2010immig[1], what="paragraphs")





########
########  These functions grab the settings and tokenise the corpus
########  and store the results in a list called tokens
########


# preprocess the tokens in a corpus
#
# Applies pre-processing rules to the text and compiles a frequency table of features (word types)
# including counts of types, tokens, sentences, and paragraphs.
# @note This will eventually become an
# indexing function.  At the moment it creates and saves a \link{dfm} in addition to 
# some summary information compiled from this, in order to speed up subsequent processing.
# Unlike most R functions which return a value, this one changes the object passed
# to it.  (And they say R can't pass by reference...)
# @param corp Corpus to be preprocessed
# @return no return but modifies the object in place by changing
# @return \item{tokens, }{a list consisting of the following:}
# @return \item{$dfm}{A \link{dfm} document-feature matrix object created with \link{settings}.}
# @return \item{$nwords}{A vector of token counts for each document.}
# @return \item{$ntypes}{A vector of type counts for each document.}
# @return \item{$nsents}{A vector of sentence counts for each document.}
# @return \item{$nparagr}{A vector of paragraph counts for each document.}
# @export
# @examples
# mycorpus <- corpus(uk2010immig)
# mycorpus
# preprocess(mycorpus)
# mycorpus
# mydfm <- dfm(mycorpus)
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


tokenizeStrsplit <- function(s, sep=" ", ...) {
    s <- clean(s, ...)
    # s <- unlist(s)
    tokens <- strsplit(s, " ")
    return(tokens)
}

