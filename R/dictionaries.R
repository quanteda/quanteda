
setClassUnion("charNULL", c("character", "NULL"))

# @rdname dictionary
# @export 
# NEED TO ADD A VALIDATOR
setClass("dictionary", contains = c("list"),
         slots = c(concatenator = "character", format = "charNULL", file = "charNULL"),
         prototype = prototype(concatenator = " ", format = NULL, file = NULL))

#' print a dictionary object
#' 
#' Print/show method for dictionary objects.
#' @param object the dictionary to be printed
#' @export
setMethod("show", "dictionary", 
          function(object) {
              cat("Dictionary object with", length(object), "key entries.\n")
              keys <- names(object)
              lapply(seq_along(object), 
                     function(i, object, keys)
                         cat(" - ", keys[i], ": ", paste(object[[i]], collapse = ", "), "\n", sep = ""),
                     object = object, keys = names(object))
              
              # print(setClass("list", object))
          })

#' create a dictionary
#' 
#' Create a quanteda dictionary, either from a list or by importing from a 
#' foreign format.  Currently supported input file formats are the Wordstat and 
#' LIWC formats.
#' @param x a list of character vector dictionary entries, including regular 
#'   expressions (see examples)
#' @param file file identifier for a foreign dictionary
#' @param format character identifier for the format of the foreign dictionary. 
#'   Available options are: \describe{ \item{\code{"wordstat"}}{format used by 
#'   Provalis Research's Wordstat software} \item{\code{"LIWC"}}{format used by 
#'   the Linguistic Inquiry and Word Count software} }
#' @param concatenator the character in between multi-word dictionary values. 
#'   This defaults to \code{"_"} except LIWC-formatted files, which defaults to
#'   a single space \code{" "}.
#' @param encoding additional optional encoding value for reading in imported
#'   dictionaries. This uses the \link{iconv} labels for encoding.  See the
#'   "Encoding" section of the help for \link{file}.
#' @param toLower if \code{TRUE}, convert all dictionary values to lowercase
#' @return A dictionary class object, essentially a specially classed named list
#'   of characters.
#' @references Wordstat dictionaries page, from Provalis Research 
#'   \url{http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/}.
#'   
#'   
#'   Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J. 
#'   (2007). The development and psychometric properties of LIWC2007. [Software 
#'   manual]. Austin, TX (\url{www.liwc.net}).
#' @seealso \link{dfm}
#' @examples
#' mycorpus <- subset(inaugCorpus, Year>1900)
#' mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxing = "taxing",
#'                           taxation = "taxation",
#'                           taxregex = "tax*",
#'                           country = "united states"))
#' head(dfm(mycorpus, dictionary = mydict))
#' 
#' \dontrun{
#' # import the Laver-Garry dictionary from http://bit.ly/1FH2nvf
#' lgdict <- dictionary(file = "http://www.kenbenoit.net/courses/essex2014qta/LaverGarry.cat",
#'                      format = "wordstat")
#' head(dfm(inaugTexts, dictionary=lgdict))
#' 
#' # import a LIWC formatted dictionary from http://www.moralfoundations.org
#' mfdict <- dictionary(file = "http://ow.ly/VMRkL", format = "LIWC")
#' head(dfm(inaugTexts, dictionary = mfdict))}
#' @export
dictionary <- function(x = NULL, file = NULL, format = NULL, 
                       concatenator = " ", 
                       toLower = TRUE, encoding = "") {
    if (!is.null(x) & !is.list(x))
        stop("Dictionaries must be named lists or lists of named lists.")
    if (any(missingLabels <- which(names(x) == ""))) 
        stop("missing key name for list element", 
             ifelse(length(missingLabels)>1, "s ", " "),
             missingLabels, "\n") 
    x <- flatten.dictionary(x)
    if (!is.null(x) & !is.list(x))
        stop("Dictionaries must be named lists or lists of named lists.")

    if (!is.null(file)) {
        if (is.null(format))
            stop("You must specify a format for file", file)
        format <- match.arg(format, c("wordstat", "LIWC"))
        if (format=="wordstat") 
            x <- readWStatDict(file, enc = encoding, toLower = toLower)
        else if (format=="LIWC")
            x <- readLIWCdict(file, toLower = toLower, encoding = encoding)
    }
    
    new("dictionary", x, format = format, file = file, concatenator = concatenator)
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
readWStatDict <- function(path, enc="", toLower=TRUE) {
    d <- utils::read.delim(path, header=FALSE, fileEncoding=enc)
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
        if (toLower) w <- toLower(w)
        # w <- gsub(" ", "", w)
        flatDict[[categ]] <- append(flatDict[[categ]], c(w))
    }
    # remove any left-over whitespace
    flatDict <- lapply(flatDict, function(x) gsub("\\s", "", x, perl=TRUE))
    return(flatDict)
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
readLIWCdict <- function(path, toLower = TRUE, encoding = getOption("encoding")) {
    
    if (encoding == "") encoding <- getOption("encoding")
    d <- readLines(con <- file(path, encoding = encoding), warn = FALSE)
    close(con)

    # remove any lines with <of>
    oflines <- grep("<of>", d)
    if (length(oflines)) {
        cat("note: ", length(oflines), " term",
            ifelse(length(oflines)>1, "s", ""), 
            " ignored because contains unsupported <of> tag\n", sep = "")
        d <- d[-oflines]
    }
    
    # get the row number that signals the end of the category guide
    guideRowEnd <- max(grep("^%\\s*$", d))
    if (guideRowEnd < 1) {
        stop('Expected a guide (a category legend) delimited by percentage symbols at start of file, none found')
    }
    # extract the category guide
    guide <- d[2:(guideRowEnd-1)]
    
    guide <- data.frame(do.call(rbind, tokenize(guide)), stringsAsFactors = FALSE)
    #guide
    #guide <- data.frame(do.call(rbind, strsplit(guide, "\t")), stringsAsFactors = FALSE)
    colnames(guide) <- c('catNum', 'catName' )
    guide$catNum <- as.integer(guide$catNum)
    # if (toLower) guide$catName <- toLower(guide$catName)

    # initialize the dictionary as list of NAs
    dictionary <- list()
    length(dictionary) <- nrow(guide)
    # assign category labels as list element names
    names(dictionary) <- guide[["catName"]]
    
    # make a list of terms with their category numbers
    catlist <- d[(guideRowEnd+1):length(d)]
    
    # remove odd parenthetical codes
    foundParens <- grep("^\\w+\\s+\\(.+\\)", catlist)
    if (length(foundParens)) {
        cat("note: ignoring parenthetical expressions in lines:\n")
        for (i in foundParens)
            cat("  [line ", foundParens + guideRowEnd, ":] ", catlist[i], "\n", sep = "")
        catlist <- gsub("\\(.+\\)", "", catlist)
    }
        
    catlist <- strsplit(catlist, "\t")
    # catlist <- tokenize(catlist, what = "fasterword", removeNumbers = FALSE)
    catlist <- as.data.frame(do.call(rbind, lapply(catlist, '[', 1:max(sapply(catlist, length)))), stringsAsFactors = FALSE)
    catlist[, 2:ncol(catlist)] <- lapply(catlist[2:ncol(catlist)], as.integer)
    names(catlist)[1] <- "category"
    if (toLower) catlist$category <- toLower(catlist$category)
    # remove any blank rows
    blankRowIndex <- which(is.na(catlist$category))
    if (length(blankRowIndex)) 
        catlist <- catlist[-blankRowIndex, ]
    
    # remove any parentheses
    catlist[["category"]] <- gsub("(\\s|\\w|\\b)[()](\\w|\\s)", "\\1\\2", catlist[["category"]])
    
    # merge terms that appear on more than one line
    catlist <- split(catlist[, 2:ncol(catlist)], catlist$category)
    catlist <- lapply(catlist, function(y) sort(unique(unlist(y))))
    catnames <- names(catlist)
    catlist <- as.data.frame(do.call(rbind, lapply(catlist, '[', 1:max(sapply(catlist, length)))), stringsAsFactors = FALSE)
    rownames(catlist) <- catnames
 
    terms <- as.list(rep(NA, nrow(catlist)))
    names(terms) <- rownames(catlist)
    for (i in 1:nrow(catlist)) {
        terms[[i]] <- as.numeric(catlist[i, !is.na(catlist[i,])])
    }
    
    for(ind in 1:length(terms)){
        for(num in as.numeric(terms[[ind]])){
            thisCat <- guide$catName[which(guide$catNum==num)]
            thisTerm <- names(terms[ind])
            dictionary[[thisCat]] <- append(dictionary[[thisCat]], thisTerm)
        }
    }
    return(dictionary)
}

flatten.dictionary <- function(elms, parent = '', dict = list()) {
    if (any(names(elms) == ""))
        stop("missing name for a nested key")
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

#' apply a dictionary or thesarus to an object
#' 
#' Convert features into equivalence classes defined by values of a dictionary 
#' object.
#' @note Selecting only features defined in a "dictionary" is traditionally 
#'   known in text analysis as a \emph{dictionary method}, even though
#'   technically this "dictionary" operates more like a thesarus.  If a thesaurus-like
#'   application is desired, set \code{exclusive = FALSE} to convert features 
#'   defined as values in a dictionary into their keys, while keeping all other
#'   features.
#' @return an object of the type passed with the value-matching features
#'   replaced by dictionary keys
#' @param x object to which dictionary or thesaurus will be supplied
#' @param dictionary the \link{dictionary}-class object that will be applied to
#'   \code{x}
#' @export
applyDictionary <- function(x, dictionary, ...) {
    UseMethod("applyDictionary")
}

#' @rdname applyDictionary
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @param valuetype how to interpret dictionary values: \code{"glob"} for 
#'   "glob"-style wildcard expressions (the format used in Wordstat and LIWC
#'   formatted dictionary values); \code{"regex"} for regular expressions; or
#'   \code{"fixed"} for exact matching (entire words, for instance)
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param capkeys if \code{TRUE}, convert dictionary keys to
#'   uppercase to distinguish them from other features
#' @param verbose print status messages if \code{TRUE}
#' @param ... not used
#' @export
#' @examples
#' myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxglob = "tax*",
#'                           taxregex = "tax.+$",
#'                           country = c("United_States", "Sweden")))
#' myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              ignoredFeatures = stopwords("english"), verbose = FALSE)
#' myDfm
#' 
#' # glob format
#' applyDictionary(myDfm, myDict, valuetype = "glob")
#' applyDictionary(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE)
#'
#' # regex v. glob format: note that "united_states" is a regex match for "tax*"
#' applyDictionary(myDfm, myDict, valuetype = "glob")
#' applyDictionary(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE)
#' 
#' # fixed format: no pattern matching
#' applyDictionary(myDfm, myDict, valuetype = "fixed")
#' applyDictionary(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE)
applyDictionary.dfm <- function(x, dictionary, exclusive = TRUE, valuetype = c("glob", "regex", "fixed"), 
                                case_insensitive = TRUE,
                                capkeys = !exclusive,
                                verbose = TRUE, ...) {
    valuetype <- match.arg(valuetype)
    dictionary <- flatten.dictionary(dictionary)
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (verbose) cat("applying a dictionary consisting of ", length(dictionary), " key", 
                     ifelse(length(dictionary) > 1, "s", ""), "\n", sep="")
    
    # convert wildcards to regular expressions (if needed)
    if (valuetype == "glob") {
        dictionary <- lapply(dictionary, utils::glob2rx)
        # because glob2rx doesn't get closing parens
        dictionary <- lapply(dictionary, function(y) gsub("\\)", "\\\\\\)", y))
    } # else if (valuetype == "fixed")
    # dictionary <- lapply(dictionary, function(x) paste0("^", x, "$"))
    
    newDocIndex <- rep(1:nrow(x), length(dictionary))
    newFeatures <- names(dictionary)
    uniqueFeatures <- features(x)
    newFeatureIndexList <- lapply(dictionary, function(x) {
        # ind <- grep(paste(x, collapse = "|"), uniqueFeatures, ignore.case = case_insensitive)
        if (valuetype == "fixed") {
            if (case_insensitive)  
                ind <- which(toLower(uniqueFeatures) %in% (toLower(x)))
            else ind <- which(uniqueFeatures %in% x)
        }
        else ind <- which(stringi::stri_detect_regex(uniqueFeatures, paste(x, collapse = "|"), case_insensitive = case_insensitive))
        if (length(ind) == 0)
            return(NULL)
        else 
            return(ind)
    })
    if (capkeys) newFeatures <- stringi::stri_trans_toupper(newFeatures)
    newFeatureCountsList <- lapply(newFeatureIndexList,
                                   function(i) {
                                       if (!is.null(i)) 
                                           rowSums(x[, i])
                                       else 
                                           rep(0, nrow(x))
                                   })
    dfmresult2 <- new("dfmSparse", sparseMatrix(i = newDocIndex,
                               j = rep(1:length(dictionary), each = ndoc(x)),
                               x = unlist(newFeatureCountsList),
                               dimnames=list(docs = docnames(x), 
                                             features = newFeatures)))
    if (!exclusive) {
        if (!all(is.null(keyIndex <- unlist(newFeatureIndexList, use.names = FALSE))))
            dfmresult2 <- cbind(x[, -keyIndex], dfmresult2)
        else
            dfmresult2 <- cbind(x, dfmresult2)
    }
    
    dfmresult2
}

