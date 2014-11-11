#' Import a Wordstat dictionary
#' 
#' Make a flattened list from a hierarchical wordstat dictionary
#' 
#' @param path full pathname of the wordstat dictionary file (usually ending in .cat)
#' @param enc a valid input encoding for the file to be read, see \link{iconvlist}
#' @param lower if \code{TRUE} (default), convert the dictionary entries to lower case
#' @return a named list, where each the name of element is a bottom level
#'   category in the hierarchical wordstat dictionary. Each element is a list of
#'   the dictionary terms corresponding to that level.
#' @author Kohei Watanabe, Kenneth Benoit
#' @export
#' @examples
#' \dontrun{
#' path <- '~/Dropbox/QUANTESS/corpora/LaverGarry.cat'
#' lgdict <- readWStatDict(path)
#' }
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

#' Import a LIWC-formatted dictionary
#' 
#' Make a flattened dictionary list object from a LIWC dictionary file.
#' @param path full pathname of the LIWC-formatted dictionary file (usually a
#'   file ending in .dic)
#' @param enc a valid input encoding for the file to be read, see 
#'   \link{iconvlist}
#' @param maxcats the maximum number of categories to read in, set by the 
#'   maximum number of dictionary categories that a term could belong to.  For 
#'   non-exclusive schemes such as the LIWC, this can be up to 7.  Set to 10 by 
#'   default, which ought to be more than enough.
#' @return a dictionary class named list, where each the name of element is a
#'   bottom level category in the hierarchical wordstat dictionary. Each element
#'   is a list of the dictionary terms corresponding to that level.
#' @author Kenneth Benoit
#' @export
#' @examples \dontrun{ 
#' LIWCdict <- readLIWCdict("~/Dropbox/QUANTESS/corpora/LIWC/LIWC2001_English.dic") }
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
