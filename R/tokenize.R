
# @rdname segment
# @return \code{segmentSentence} returns a character vector of sentences that
#   have been segmented
# @export
# @examples
# # segment sentences of the UK 2010 immigration sections of manifestos
# segmentSentence(ukimmigTexts[1])[1:5]   # 1st 5 sentences from first (BNP) text
# str(segmentSentence(ukimmigTexts[1]))   # a 132-element char vector
# str(segmentSentence(ukimmigTexts[1:2])) # a 144-element char vector (143+ 12)
# 
segmentSentence <- function(x, delimiter="[.!?:;]", perl=FALSE) {
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
    sentences <- unlist(strsplit(text, delimiter, perl=perl))
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

# @rdname segment
# @return \code{segmentParagraph} returns a character vector of paragraphs that
#   have been segmented
# @export
# @examples
# # segment paragraphs 
# segmentParagraph(ukimmigTexts[3])[1:2]   # 1st 2 Paragraphs from 3rd (Con) text
# str(segmentParagraph(ukimmigTexts[3]))   # a 12-element char vector
# 
# @export
segmentParagraph <- function(x, delimiter="\\n{2}", perl=FALSE) {
    tmp <- unlist(strsplit(x, delimiter, perl=perl))
    tmp[which(tmp != "")]
}



#' segment texts into component elements
#' 
#' Segment text(s) into tokens, sentences, paragraphs, or other sections. 
#' \code{segment} works on a character vector or corpus object, and allows the 
#' delimiters to be defined.  See details.
#' @param x text or corpus object to be segmented
#' @param ... provides additional passed to the regular expression, such as \code{perl=TRUE},
#' or arguments to be passed to \link{clean} if \code{what=tokens},
#' @return A list of segmented texts, with each element of the list correponding
#'   to one of the original texts.
#' @details Tokens are delimited by Separators.  For sentences, the delimiter 
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
#' @param perl logical. Should Perl-compatible regular expressions be used?
#' @export
#' @examples
#' # same as tokenize()
#' identical(tokenize(ukimmigTexts, lower=FALSE), segment(ukimmigTexts, lower=FALSE))
#' 
#' # segment into paragraphs
#' segment(ukimmigTexts[3:4], "paragraphs")
#' 
#' # segment a text into sentences
#' segmentedChar <- segment(ukimmigTexts, "sentences")
#' segmentedChar[2]
segment.character <- function(x, what=c("tokens", "sentences", "paragraphs", "tags", "other"), 
                              delimiter = ifelse(what=="tokens", " ", 
                                                 ifelse(what=="sentences", "[.!?:;]", 
                                                        ifelse(what=="paragraphs", "\\n{2}", 
                                                               ifelse(what=="tags", "##\\w+\\b", 
                                                                      NULL)))),
                              perl=FALSE,
                              ...) {
    what <- match.arg(what)
    if (what=="tokens") {
        return(tokenize(x, sep=delimiter, ...)) 
    } else if (what=="sentences") {
        return(lapply(x, segmentSentence, delimiter, perl=perl)) 
    } else if (what=="paragraphs") {
        return(lapply(x, segmentParagraph, delimiter, perl=perl)) 
    } else if (what=="tags") {
        return(lapply(x, segmentParagraph, delimiter, perl=perl))         
    } else if (what=="other") {
        if (is.null(delimiter))
            stop("For type other, you must supply a delimiter value.")
        return(lapply(x, segmentParagraph, delimiter, perl=perl))
    }
}

#' @rdname segment
#' @export
#' @note Does not currently record document segments if segmenting a multi-text corpus
#' into smaller units. For this, use \link{changeunits} instead.
#' @examples
#' testCorpus <- corpus("##INTRO This is the introduction. 
#'                       ##DOC1 This is the first document.  
#'                       Second sentence in Doc 1.  
#'                       ##DOC3 Third document starts here.  
#'                       End of third document.")
#' testCorpusSeg <- segment(testCorpus, "tags")
#' summary(testCorpusSeg)
#' texts(testCorpusSeg)
#' # segment a corpus into sentences
#' segmentedCorpus <- segment(corpus(ukimmigTexts), "sentences")
#' identical(ndoc(segmentedCorpus), length(unlist(segmentedChar)))
segment.corpus <- function(x, what = c("tokens", "sentences", "paragraphs", "tags", "other"), 
                           delimiter = ifelse(what=="tokens", " ", 
                                              ifelse(what=="sentences", "[.!?:;]", 
                                                     ifelse(what=="paragraphs", "\\n{2}", 
                                                            ifelse(what=="tags", "##\\w+\\b", 
                                                                   NULL)))),
                           perl=FALSE,
                           ...) {
    newCorpus <- corpus(unlist(segment(texts(x), what, delimiter, perl=perl, ...)),
                        source = metacorpus(x, "source"),
                        notes = paste0("segment.corpus(", match.call(), ")"))
    
    if (what == "tags") {
        tagIndex <- gregexpr(delimiter, texts(x), perl=perl)[[1]]
        tags <- character()
        length(tags) <- ndoc(newCorpus)
        for (i in 1:length(tagIndex))
            tags[i] <- substr(texts(x), start = tagIndex[i],
                              stop = tagIndex[i] + attr(tagIndex, "match.length")[i] - 1)
        docvars(newCorpus, "tag") <- tags
    }
    
    newCorpus
}

# segment(ukimmigTexts[1], removePunct=FALSE, simplify=TRUE)
# segment(ukimmigTexts[1], what="sentences")
# segment(ukimmigTexts[1], what="paragraphs")


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
# mycorpus <- corpus(ukimmigTexts)
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



#' tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @rdname tokenize
#' @aliases tokenise
#' @param x The text(s) or corpus to be tokenized
#' @param ... additional arguments not used
#' @return A list of length \code{\link{ndoc}(x)} of the tokens found in each text.
#' @author Ken Benoit and Paul Nulty
#' @export
#' @examples 
#' # same for character vectors and for lists
#' tokensFromChar <- tokenize(inaugTexts[1:3])
#' tokensFromCorp <- tokenize(subset(inaugCorpus, Year<1798))
#' identical(tokensFromChar, tokensFromCorp)
#' str(tokensFromChar)
#' @export
tokenize <- function(x, ...) {
    UseMethod("tokenize")
}

#' @rdname tokenize
#' @param what the unit for splitting the text, defaults to \code{"word"}. 
#'   Available alternatives are \code{c("character", "word", "line_break", 
#'   "sentence")}. See \link[stringi]{stringi-search-boundaries}.
#' @param removeNumbers remove tokens that consist only of numbers, but not 
#'   words that start with digits, e.g. \code{2day}
#' @param removePunct remove all punctuation
#' @param removeTwitter remove Twitter characters \code{@@} and \code{#}; set to
#'   \code{FALSE} if you wish to eliminate these
#' @param removeSeparators remove Separators and separator characters (spaces 
#'   and variations of spaces, plus tab, newlines, and anything else in the 
#'   Unicode "separator" category) when \code{removePunct=FALSE}
#' @param simplify if \code{TRUE}, return a character vector of tokens rather 
#'   than a list of length \code{\link{ndoc}(texts)}, with each element of the 
#'   list containing a character vector of the tokens corresponding to that 
#'   text.
#' @param verbose if \code{TRUE}, print timing messages to the console; off by 
#'   default
#' @import stringi
#' @details The tokenizer is designed to be fast and flexible as well as to 
#'   handle Unicode correctly. Most of the time, users will construct \link{dfm}
#'   objects from texts or a corpus, without calling \code{tokenize()} as an 
#'   intermediate step.  Since \code{tokenize()} is most likely to be used by 
#'   more technical users, we have set its options to default to minimal
#'   intervention. This means that punctuation is tokenized as well, and that
#'   nothing is removed from the
#' @return a \strong{tokenizedText} (S3) object, essentially a list of character 
#'   vectors. If \code{simplify=TRUE} then return a single character vector.
#' @export
#' @examples 
#' # returned as a list
#' head(tokenize(inaugTexts[57])[[1]], 10)
#' # returned as a character vector using simplify=TRUE
#' head(tokenize(inaugTexts[57], simplify=TRUE), 10)
#' 
#' # removing punctuation marks and lowecasing texts
#' head(tokenize(toLower(inaugTexts[57]), simplify=TRUE, removePunct=TRUE), 30)
#' # keeping case and punctuation
#' head(tokenize(inaugTexts[57], simplify=TRUE), 30)
#' 
#' ## MORE COMPARISONS
#' txt <- "#textanalysis is MY <3 4U @@myhandle gr8 #stuff :-)"
#' tokenize(txt, removePunct=TRUE)
#' tokenize(txt, removePunct=TRUE, removeTwitter=TRUE)
#' #tokenize("great website http://textasdata.com", removeURL=FALSE)
#' #tokenize("great website http://textasdata.com", removeURL=TRUE)
#' 
#' txt <- c(text1="This is $10 in 999 different ways,\n up and down; left and right!", 
#'          text2="@@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
#' tokenize(txt, verbose=TRUE)
#' tokenize(txt, removeNumbers=TRUE, removePunct=TRUE)
#' tokenize(txt, removeNumbers=FALSE, removePunct=TRUE)
#' tokenize(txt, removeNumbers=TRUE, removePunct=FALSE)
#' tokenize(txt, removeNumbers=FALSE, removePunct=FALSE)
#' tokenize(txt, removeNumbers=FALSE, removePunct=FALSE, removeSeparators=FALSE)
#' 
#' # character level
#' tokenize("Great website: http://textasdata.com?page=123.", what="character")
#' tokenize("Great website: http://textasdata.com?page=123.", what="character", 
#'          removeSeparators=FALSE)
#' 
#' # sentence level         
#' tokenize(inaugTexts[c(2,40)], what = "sentence", simplify = TRUE)
tokenize.character <- function(x, what=c("word", "sentence", "character", "fastestword", "fasterword"),
                               removeNumbers = FALSE, 
                               removePunct = FALSE,
                               removeSeparators = TRUE,
                               removeTwitter = FALSE,
                               # removeURL = TRUE,
                               simplify=FALSE,
                               verbose = FALSE,  ## FOR TESTING
                               ...) {
    
    if (.Platform$OS.type == "windows") {
        cores <- 1
    }
    
    what <- match.arg(what)
    #     if (!(what %in% c("character", "word", "line_break", "sentence", "fastestword", "fasterword")))
    #         stop(what, " not a valid text boundary, see help(\"stringi-search-boundaries\", package=\"stringi\")")
    
    if (verbose) cat("Starting tokenization...\n")
    result <- x
    
    if (removeTwitter == FALSE & what != "fastword") {
        if (verbose) cat("  ...preserving Twitter characters (#, @)")
        startTimeClean <- proc.time()
        result <- stringi::stri_replace_all_fixed(result, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
        if (verbose) cat("...total elapsed:", (proc.time() - startTimeClean)[3], "seconds.\n")
    }
    
    if (verbose) cat("  ...tokenizing texts")
    startTimeTok <- proc.time()
    if (what %in% c("fasterword", "fastestword")) {
        
        if (verbose & removeNumbers==TRUE) cat(", removing numbers")
        if (verbose & removePunct==TRUE) cat(", removing punctuation")
        regexToEliminate <- paste0(ifelse(removeNumbers, "\\b\\d+\\b|", ""),
                                   ifelse(removePunct, paste0("(?![", ifelse(removeTwitter, "_", "@#_"), "])[[:punct:]]"), "|"))
        if (regexToEliminate != "|")
            result <- stri_replace_all_regex(result, regexToEliminate, "")
        
        if (verbose & removePunct==TRUE) cat(", ", what, "tokenizing", sep="")
        if (what=="fastestword")
            result <- stringi::stri_split_fixed(result, " ")
        else if (what=="fasterword")
            result <- stringi::stri_split_regex(result, "\\s")
        
    } else {
        if (what != "character") {
            result <- stringi::stri_split_boundaries(result, 
                                                     type = what, 
                                                     skip_word_none = removePunct, # this is what obliterates currency symbols, Twitter tags, and URLs
                                                     skip_word_number = removeNumbers) # but does not remove 4u, 2day, etc.
            ## remove trailing whitespaces
            #result <- lapply(result, stringi::stri_trim_right)
            # remove any "sentences" that were completely blanked out
            result <- lapply(result, function(x) x <- x[which(x != "")])
        } else {
            result <- stringi::stri_split_boundaries(result, type = what)
            # note: does not implement removePunct or removeNumbers
        }
    }
    if (verbose) cat("...total elapsed: ", (proc.time() - startTimeTok)[3], "seconds.\n")
    
    if (removeSeparators & !removePunct & (what == "character" | what == "word")) {
        if (verbose) cat("...removing separators.\n")
        result <- lapply(result, function(x) x[-which(stri_detect_regex(x, "^\\s$"))]) 
    }
    
    if (removeTwitter == FALSE & what != "fastword") {
        if (verbose) cat("  ...replacing Twitter characters (#, @)")
        startTimeClean <- proc.time()
        result <- lapply(result, stringi::stri_replace_all_fixed, c("_ht_", "_as_"), c("#", "@"), vectorize_all = FALSE)
        if (verbose) cat("...total elapsed:", (proc.time() - startTimeClean)[3], "seconds.\n")
    }
    
    if (simplify==FALSE) {
        # stri_* destroys names, so put them back
        startTimeClean <- proc.time()
        if (verbose) cat("  ...replacing names")
        names(result) <- names(x)
        if (verbose) cat("...total elapsed: ", (proc.time() - startTimeClean)[3], "seconds.\n")
        
    } else {
        # or just return the tokens as a single character vector
        if (verbose) cat("  ...unlisting results\n")
        result <- unlist(result)
    }
    if (verbose) 
        cat("Finished tokenizing and cleaning", format(length(result), big.mark=","), "texts\n") #, with a total of", format(length(unlist(result)), big.mark=","), "tokens.\n")
    
    # make this an S3 class item, if a list
    if (simplify == FALSE) {
        class(result) <- c("tokenizedTexts", class(result))
    }
    
    result
}

#' @rdname tokenize
#' @export
tokenize.corpus <- function(x, ...) {
    # get the settings for clean from the corpus and use those, 
    # unless more specific arguments are passed -- ADD THE ABILITY TO PASS THESE
    # need to include sep in this list too 
    tokenize(texts(x), ...)
}
