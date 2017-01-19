
setClassUnion("charNULL", c("character", "NULL"))

#' @rdname dictionary-class
#' @export
#' @keywords internal dictionary
#' @slot .Data named list of mode character, where each element name is a
#'   dictionary "key" and each element is one or more dictionary entry "values"
#'   consisting of a pattern match
#' @slot concatenator character object specifying space between multi-word
#'   values
#' @slot format dictionary format (if imported)
#' @slot file file from which a dictionary was read (if imported)
setClass("dictionary", contains = c("list"),
         slots = c(concatenator = "character", format = "charNULL", file = "charNULL"),
         prototype = prototype(concatenator = " ", format = NULL, file = NULL))

setValidity("dictionary", function(object) {
    # does every element have a name? simply needs to pass
    validate_dictionary(object)
})


# Internal function to chekc if dictionary eintries are all chracters
validate_dictionary <- function(dict){
    
    if (is.null(names(dict))) {
        stop("dictionary elements must be named")
    }
    if (any(names(dict) == "")) {
        unnamed <- dict[which(names(dict) == "")]
        stop("unnamed dictionary entry: ", unnamed)
    }
    
    for (i in 1:length(dict)) {
        entry <- dict[[i]]
        if (is.list(entry)) {
            validate_dictionary(entry)
        } else {
            if (any(!is.character(entry))) {
                nonchar <- entry[!is.character(entry)]
                stop("non-character entries found: ", nonchar)
            }
        }   
    }
}

# Internal function to print dictionary
print_dictionary <- function(dict, level = 1){
    
    for (i in 1:length(dict)) {
        entry <- dict[[i]]
        if (is.list(entry)) {
            cat(rep('  ', level - 1), "- ", names(dict[i]), ':\n', sep = "")
            print_dictionary(entry, level + 1)
        } else {
            cat(rep('  ', level - 1), "- ", names(dict[i]) , ": ", paste(entry, collapse = ", "), "\n", sep = "")
        }   
    }
}


#' print a dictionary object
#' 
#' Print/show method for dictionary objects.
#' @param object the dictionary to be printed
#' @rdname dictionary-class
#' @export
setMethod("show", "dictionary", 
          function(object) {
              cat("Dictionary object with", length(unlist(object)), "key entries.\n")
              print_dictionary(object)
          })

#' create a dictionary
#' 
#' Create a quanteda dictionary, either from a list or by importing from a 
#' foreign format.  Currently supported input file formats are the Wordstat,
#' LIWC, Lexicoder v2 and v3, and Yoshikoder formats.  The import using the 
#' LIWC format works with 
#' all currently available dictionary files supplied as part of the LIWC 2001, 
#' 2007, and 2015 software (see References).
#' @param ... a named list of character vector dictionary entries, including \link{valuetype} pattern
#'  matches, and including multi-word expressions separated by \code{concatenator}.  The argument 
#'  may be an explicit list or named set of elements that can be turned into a list.  See examples.
#'  This argument may be omitted if the dictionary is read from \code{file}.
#' @param file file identifier for a foreign dictionary
#' @param format character identifier for the format of the foreign dictionary. 
#'   If not supplied, the format is guessed from the dictionary file's
#'   extension.
#'   Available options are: \describe{ \item{\code{"wordstat"}}{format used by 
#'   Provalis Research's Wordstat software} \item{\code{"LIWC"}}{format used by 
#'   the Linguistic Inquiry and Word Count software} \item{\code{"yoshikoder"}}{
#'   format used by Yoshikoder software} \item{\code{"lexicoder"}}{format used
#'   by Lexicoder} \item{\code{"YAML"}}{the standard YAML format}}
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
#'   Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J. 
#'   (2007). The development and psychometric properties of LIWC2007. [Software 
#'   manual]. Austin, TX (\url{www.liwc.net}).
#'   
#'   Yoshikoder page, from Will Lowe 
#'   \url{http://conjugateprior.org/software/yoshikoder/}.
#'   
#' @seealso \link{dfm}
#' @examples
#' mycorpus <- corpus_subset(data_corpus_inaugural, Year>1900)
#' mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxing = "taxing",
#'                           taxation = "taxation",
#'                           taxregex = "tax*",
#'                           country = "america"))
#' head(dfm(mycorpus, dictionary = mydict))
#' 
#' # also works
#' mydict2 <- dictionary(christmas = c("Christmas", "Santa", "holiday"),
#'                       opposition = c("Opposition", "reject", "notincorpus"))
#' dfm(mycorpus, dictionary = mydict2)
#' 
#' \dontrun{
#' # import the Laver-Garry dictionary from http://bit.ly/1FH2nvf
#' lgdict <- dictionary(file = "http://www.kenbenoit.net/courses/essex2014qta/LaverGarry.cat",
#'                      format = "wordstat")
#' head(dfm(data_char_inaugural, dictionary=lgdict))
#' 
#' # import a LIWC formatted dictionary from http://www.moralfoundations.org
#' mfdict <- dictionary(file = "http://ow.ly/VMRkL", format = "LIWC")
#' head(dfm(data_char_inaugural, dictionary = mfdict))}
#' @importFrom stats setNames
#' @importFrom tools file_ext
#' @export
dictionary <- function(..., file = NULL, format = NULL, 
                       concatenator = " ", 
                       toLower = TRUE, encoding = "") {
  # these allow implicit list construction through ...
  x <- list(...)
  if (length(x)==1) 
      x <- as.list(x[[1]])
  if (!is.null(x) & !is.list(x))
    stop("Dictionaries must be named lists or lists of named lists.")
  # if (any(missingLabels <- which(names(x) == ""))) 
  #   stop("missing key name for list element", 
  #        ifelse(length(missingLabels)>1, "s ", " "),
  #        missingLabels, "\n") 
  # if (!is.null(x) & !is.list(x))
  #   stop("Dictionaries must be named lists or lists of named lists.")

  dict_format_mapping <- c(cat="wordstat", dic="LIWC", ykd="yoshikoder", lcd="yoshikoder", 
                           lc3="lexicoder", yml="YAML")
  if (!is.null(file)) {

      if (is.null(format)) {
          ext <- file_ext(file)
          if (ext %in% names(dict_format_mapping)) {
              format <- dict_format_mapping[[ext]]
          }
          else {
              stop(paste("Unknown dictionary file extension", ext))
          }
      }
      else {
          format <- match.arg(format, dict_format_mapping)
      }
      format <- unname(format)
      
      if (format=="wordstat") {
          x <- readWStatDict(file, enc = encoding, toLower = toLower)
      } else if (format=="LIWC") {
          x <- readLIWCdict(file, toLower = toLower, encoding = encoding)
      } else if (format=="yoshikoder") {
          x <- readYKdict(file)
      } else if (format=="lexicoder") {
          x <- readLexicoderDict(file)
      } else if (format=="YAML") {
          x <- yaml::yaml.load_file(file, as.named.list = TRUE)  
      }    
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
    d <- utils::read.delim(path, header=FALSE, fileEncoding=enc, na.string = "__________")
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
    for (i in 1:nrow(d)) {
        if (d[i, ncol(d)] ==  "") next
        categ <- unlist(paste(d[i,(1:(ncol(d)-1))], collapse = "."))
        w <- d[i, ncol(d)]
        w <- unlist(strsplit(w, '\\('))[[1]]
        if (toLower) w <- char_tolower(w)
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
        catm("note: ", length(oflines), " term",
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
    colnames(guide) <- c('catNum', 'catName' )
    guide$catNum <- as.integer(guide$catNum)

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
        catm("note: ignoring parenthetical expressions in lines:\n")
        for (i in foundParens)
            catm("  [line ", foundParens + guideRowEnd, ":] ", catlist[i], "\n", sep = "")
        catlist <- gsub("\\(.+\\)", "", catlist)
    }
        
    ## clean up irregular dictionary files
    # remove any repeated \t
    catlist <- gsub("\t\t+", "\t", catlist)
    # remove any spaced before a \t
    catlist <- gsub(" +\t", "\t", catlist)
    # replace any blanks that should be \t with \t (e.g. in Moral Foundations dictionary)
    catlist <- gsub("(\\d+) +(\\d+)", "\\1\t\\2", catlist)
    # remove any \t only lines or empty lines
    if (length(blanklines <- grep("^\\s*$", catlist))) 
        catlist <- catlist[-blanklines]

    catlist <- strsplit(catlist, "\t")
    catlist <- as.data.frame(do.call(rbind, lapply(catlist, '[', 1:max(sapply(catlist, length)))), stringsAsFactors = FALSE)
    catlist[, 2:ncol(catlist)] <- sapply(catlist[, 2:ncol(catlist)], as.integer)
    names(catlist)[1] <- "category"
    if (toLower) catlist$category <- char_tolower(catlist$category)
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
    
    for (ind in 1:length(terms)) {
        for(num in as.numeric(terms[[ind]])){
            thisCat <- guide$catName[which(guide$catNum==num)]
            thisTerm <- names(terms[ind])
            dictionary[[thisCat]] <- append(dictionary[[thisCat]], thisTerm)
        }
    }
    return(dictionary)
}

# Import a Yoshikoder dictionary
# 
# Make a flattened list from a hierarchical Yoshikoder dictionary.  
# 
# Parsing Yoshikoder dictionary requires the XML package to be installed.
# 
# @param path full pathname of the Yoshikoder dictionary file (ending in \code{.ykd})
# @return a named list, where each the name of element is a \textit{top} level
#   category in the hierarchical Yoshikoder dictionary. Each element of the
#   list is is a vector of the dictionary patterns in that category or any of its
#   sub-categories.
# @author Will Lowe
# @export
# @examples
# \dontrun{
# path <- 'http://dl.conjugateprior.org/laver-garry-ajps.ykd'
# ykdict <- readYoshikoderDict(path)
# }
readYKdict <- function(path){
    if (!requireNamespace("XML", quietly = TRUE))
        stop("You must have package XML installed to parse Yoshikoder dictionary files.")
    
    xx <- XML::xmlParse(path)
    catnames <- XML::xpathSApply(xx, "/dictionary/cnode/cnode", 
                                 XML::xmlGetAttr, name="name")
    get_patterns_in_subtree <- function(x){
        XML::xpathSApply(x, ".//pnode", XML::xmlGetAttr, name="name")
    }
    cats <- XML::getNodeSet(xx, "/dictionary/cnode/cnode")
    stats::setNames(lapply(cats, get_patterns_in_subtree), catnames)
}

#  Flatten a hierarchical dictionary into a list of character vectors
# 
#  Converts a hierarchical dictionary (a named list of named lists, ending in character
#  vectors at the lowest level) into a flat list of character vectors.  Works like
#  \code{unlist(dictionary, recursive=TRUE)} except that the recursion does not go to the
#  bottom level.  Called by \code{\link{dfm}}.
# 
#  @param tree list to be flattened
#  @param levels integer vector indicating levels in the dictionary
#  @param level internal argument to pass current levels
#  @param key_tree internal argument to pass for parent keys
#  @param dict internal argument to pass flattend dicitonary
#  @return A dictionary flattened to variable levels
#  @keywords internal
#  @author Kohei Watanabe
#  @export
#  @examples
#  dictPopulismEN <- 
#      dictionary(list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
#                                 "corrupt*", "propagand", "politici*", "*deceit*",
#                                 "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
#                                 "dishonest*", "establishm*", "ruling*")))
#  dictionary_flatten(dictPopulismEN)
# 
#  hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
#                              level1a2 = c("l1a21", "l1a22")),
#                level1b = list(level1b1 = c("l1b11", "l1b12"),
#                               level1b2 = c("l1b21", "l1b22", "l1b23")),
#                level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
#                               level1c1b = list(level1c1b1 = c("lowestalone"))))
#  dictionary_flatten(hdict)
#  dictionary_flatten(hdict, 2)
#  dictionary_flatten(hdict, 1:2)

dictionary_flatten <- function(dict, levels = 1:100, level = 1, key = '', dict_flat = list()) {
    #cat("-------------------\n")
    #cat("level:", level, "\n")
    for (name in names(dict)) {
        entry <- dict[[name]]
        if (level %in% levels) {    
            if (key != '') {
                key_entry <- paste(key, name, sep = '.')
            } else {
                key_entry <- name
            }
        } else {
            key_entry <- key
        }
        #cat("key:", key, "\n")
        #cat("key_entry:", key_entry, "\n")
        if (is.list(entry)) {
            #cat("List:\n")
            #print(entry)
            dict_flat <- dictionary_flatten(entry, levels, level + 1, key_entry, dict_flat)
        } else {
            #cat("Vector:\n")
            #print(entry)
            dict_flat[[key_entry]] <- c(dict_flat[[key_entry]], entry)
        }
    }
    return(dict_flat)
}



# Import a Lexicoder dictionary
# 
# @param path full pathname of the lexicoder dictionary file (usually ending in .lcd)
# @param toLower if \code{TRUE} (default), convert the dictionary entries to lower case
# @return a named list, where each the name of element is a category/key and each element is a list of
#   the dictionary terms corresponding to that level.
# @author Adam Obeng
# @export
readLexicoderDict <- function(path, toLower=TRUE) {
    current_key <- NULL
    current_terms <- c()
    dict <- list() 
    #  Lexicoder 3.0 files are always UTF-8
    for (l in readLines(con <- file(path, encoding = 'utf-8'))) {
        if (toLower) l <- tolower(l)
        if (substr(l, 1, 1) == '+') {
            if (length(current_terms) > 0) {
                dict[[current_key]] <- current_terms
            }
            current_key <- substr(l, 2, nchar(l))
            current_terms <- c()
        }
        else {
            current_terms <- c(current_terms, l)
        }
    }
    dict[[current_key]] <- current_terms
    return(dict)
}

#' check if an object is a dictionary
#' 
#' Return \code{TRUE} if an object is a \pkg{quanteda} \link{dictionary}.
#' @param x any object
#' @export
is.dictionary <- function(x) {
    is(x, "dictionary")
}
    

 



