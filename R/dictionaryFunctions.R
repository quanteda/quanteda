#' create a dictionary
#' 
#' Create a quanteda dictionary, either from a list or by importing from a 
#' foreign format.  Currently supported formats are the Wordstat and LIWC 
#' formats.
#' @param x a list of character vector dictionary entries, including regular 
#'   expressions (see examples)
#' @param file file identifier for a foreign dictionary
#' @param format character identifier for the format of the foreign dictionary. 
#'   Available options are: \describe{ \item{"wordstat"}{format used by Provalis
#'   Research's Wordstat software} \item{"LIWC"}{format used by the Linguistic 
#'   Inquiry and Word Count software} }
#' @param enc optional encoding value for dictionaries imported in Wordstat 
#'   format
#' @param maxcats optional maximum categories to which a word could belong in a 
#'   LIWC dictionary file, defaults to 10 (which is more than the actual LIWC 
#'   2007 dictionary uses).  The default value of 10 is likely to be more than
#'   enough.
#' @source Wordstat dictionaries page, from Provalis Research 
#'   \url{http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/}.
#'   
#'   Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J. 
#'   (2007). The development and psychometric properties of LIWC2007. [Software 
#'   manual]. Austin, TX (\url{www.liwc.net}).
#' @seealso \link{dfm}
#' @examples
#' mycorpus <- subset(inaugCorpus, Year>1900)
#' mydict <- 
#'     dictionary(list(christmas=c("Christmas", "Santa", "holiday"),
#'                     opposition=c("Opposition", "reject", "notincorpus"),
#'                     taxing="taxing",
#'                     taxation="taxation",
#'                     taxregex="tax*",
#'                     country="united states"))
#' dfm(mycorpus, dictionary=mydict)                     
#' \dontrun{
#' # import the Laver-Garry dictionary from http://bit.ly/1FH2nvf
#' lgdict <- dictionary(file="~/Dropbox/QUANTESS/dictionaries/Misc Provalis/LaverGarry.cat",
#'                      format="wordstat")
#' dfm(inaugTexts, dictionary=lgdict)
#' 
#' # import a LIWC formatted dictionary
#' liwcdict <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic",
#'                        format = "LIWC")
#' dfm(inaugTexts, dictionary=liwcdict)
#' }
#' @export
dictionary <- function(x=NULL, file=NULL, format=NULL, enc="", tolower=TRUE, maxcats=10) {
    if (!is.null(x) & !is.list(x))
        stop("Dictionaries must be named lists.")
    x <- flatten.dictionary(x)
    if (!is.null(x) & !is.list(x))
        stop("Dictionaries must be named lists or lists of named lists.")
    
    if (!is.null(file)) {
        if (is.null(format))
            stop("You must specify a format for file", file)
        format <- match.arg(format, c("wordstat", "LIWC"))
        if (format=="wordstat") 
            x <- readWStatDict(file, enc=enc, lower=tolower)
        else if (format=="LIWC") 
            x <- readLIWCdict(file, maxcats=10, enc="")
    }
    
    class(x) <- c("dictionary", class(x))
    x
}


# Import a Wordstat dictionary
# 
# Make a flattened list from a hierarchical wordstat dictionary
# 
# @param path full pathname of the wordstat dictionary file (usually ending in .cat)
# @param enc a valid input encoding for the file to be read, see \link{iconvlist}
# @param lower if \code{TRUE} (default), convert the dictionary entries to lower case
# @return a named list, where each the name of element is a bottom level
#   category in the hierarchical wordstat dictionary. Each element is a list of
#   the dictionary terms corresponding to that level.
# @author Kohei Watanabe, Kenneth Benoit
# @export
# @examples
# \dontrun{
# path <- '~/Dropbox/QUANTESS/corpora/LaverGarry.cat'
# lgdict <- readWStatDict(path)
# }
readWStatDict <- function(path, enc="", lower=TRUE) {
    d <- read.delim(path, header=FALSE, fileEncoding=enc)
    d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
    thismajorcat <- d[1,1]
    # this loop fills in blank cells in the category|term dataframe
    for (i in 1:nrow(d)) {
        if (d[i,1] == "") {
            d[i,1] <- thismajorcat
        } else {
            thismajorcat <- d[i,1]
        }
        for (j in 1:(ncol(d)-1)) {
            if(d[i,j] == "" & length(d[i-1,j])!=0) {
                d[i,j] <- d[i-1,j] 
            }
        }
        if (nchar(d[i,ncol(d)-1]) > 0) {
            pat <- c("\\(")
            if (!length(grep(pat, d[i,ncol(d)-1]))==0) {
                d[i, ncol(d)] <- d[i, ncol(d)-1]
                d[i, ncol(d)-1] <- "_"
            }
        }
    }
    flatDict <- list()
    categ <- list()

    # this loop collapses the category cells together and
    # makes the list of named lists compatible with dfm
    for (i in 1:nrow(d)){
        if (d[i,ncol(d)]=='') next
        categ <- unlist(paste(d[i,(1:(ncol(d)-1))], collapse="."))
        w <- d[i, ncol(d)]
        w <- unlist(strsplit(w, '\\('))[[1]]
        if (lower) w <- tolower(w)
        # w <- gsub(" ", "", w)
        flatDict[[categ]] <- append(flatDict[[categ]], c(w))
    }
    # remove any left-over whitespace
    flatDict <- lapply(flatDict, function(x) gsub("\\s", "", x, perl=TRUE))
    return(flatDict)
}




# old code:
# makes a list of lists from a two-level wordstat dictionary
readWStatDictNested <- function(path) {
    lines <- readLines(path)
    allDicts <- list()
    curDict <- list()
    n <- list()
    for (i in 1:length(lines)) {
        word <- unlist(strsplit(lines[i], '\\('))[[1]]
        #if it doesn't start with a tab, it's a category
        if (substr(word,1,1) != "\t") {
            n <- c(n,word)
            if(length(curDict) >0) allDicts = c(allDicts, list(word=c(curDict)))
            curDict = list()
        } else {
            word <- gsub(' ','', word)
            curDict = c(curDict, gsub('\t','',(word)))
        } 
    }
    # add the last dicationary
    allDicts = c(allDicts, list(word=c(curDict)))
    names(allDicts) <- n
    return(allDicts)
}

# Import a LIWC-formatted dictionary
# 
# Make a flattened dictionary list object from a LIWC dictionary file.
# @param path full pathname of the LIWC-formatted dictionary file (usually a
#   file ending in .dic)
# @param enc a valid input encoding for the file to be read, see 
#   \link{iconvlist}
# @param maxcats the maximum number of categories to read in, set by the 
#   maximum number of dictionary categories that a term could belong to.  For 
#   non-exclusive schemes such as the LIWC, this can be up to 7.  Set to 10 by 
#   default, which ought to be more than enough.
# @return a dictionary class named list, where each the name of element is a
#   bottom level category in the hierarchical wordstat dictionary. Each element
#   is a list of the dictionary terms corresponding to that level.
# @author Kenneth Benoit
# @export
# @examples \dontrun{ 
# LIWCdict <- readLIWCdict("~/Dropbox/QUANTESS/corpora/LIWC/LIWC2001_English.dic") }
readLIWCdict <- function(path, maxcats=10, enc="") {
    # read in the dictionary as a (big, uneven) table
    d <- read.delim(path, header=FALSE, fileEncoding=enc,
                    col.names=c("category", paste("catno", 1:maxcats, sep="")),
                    stringsAsFactors=FALSE)
    
    # get the row number that signals the end of the category guide
    guideRowEnd <- max(which(d$category=="%"))
    # extract the category guide
    guide <- d[2:(guideRowEnd-1), 1:2]
    # initialize the dictionary as list of NAs
    dictionary <- list()
    length(dictionary) <- nrow(guide)
    # assign category labels as list element names
    names(dictionary) <- guide[,2]
    
    # make a list of terms with their category numbers
    catlist <- d[(guideRowEnd+1):nrow(d), ]
    rownames(catlist) <- catlist[,1]
    catlist <- catlist[, -1]
    suppressWarnings(catlist <- apply(catlist, c(1,2), as.numeric))
    # now put this into a (ragged) list 
    terms <- as.list(rep(NA, nrow(catlist)))
    names(terms) <- rownames(catlist)
    for (i in 1:nrow(catlist)) {
        terms[[i]] <- as.numeric(catlist[i, !is.na(catlist[i,])])
    }

    # now fill the dictionary with the terms
    for (t in 1:length(terms)) {
        for (c in terms[[t]]) {
            dictionary[[c]] <- append(dictionary[[c]], names(terms[t]))
        }
    }
    return(dictionary)
}

#readLIWCdict("~/Dropbox/QUANTESS/corpora/LIWC/LIWC2001_English.dic")


flatten.dictionary <- function(elms, parent = '', dict = list()) {
    for (self in names(elms)) {
        elm <- elms[[self]]
        if (parent != '') {
            self <- paste(parent, self, sep='.')
        }
        # print("-------------------")
        # print (paste("Name", self))
        if (is.list(elm)) {
            # print("List:")
            # print(names(elm))
            dict <- flatten.dictionary(elm, self, dict)
        } else {
            # print("Words:")
            dict[[self]] <- elm
            # print(dict)
        }
    }
    return(dict)
}

