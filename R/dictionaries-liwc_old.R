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
read_dict_liwc_old <- function(path, encoding = "auto", toLower = FALSE) {
    
    if (encoding == "") encoding <- getOption("encoding")
    # d <- readLines(con <- file(path, encoding = encoding), warn = FALSE)
    d <- stringi::stri_read_lines(path, encoding = encoding, fallback_encoding = 'windows-1252')
    # close(con)
    
    # remove any lines with <of>
    oflines <- grep("<of>", d)
    if (length(oflines)) {
        catm("note: ", length(oflines), " term",
             if (length(oflines) > 1L) "s" else "", 
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
    
    guide <- data.frame(do.call(rbind, as.list(tokens(guide))), stringsAsFactors = FALSE)
    colnames(guide) <- c('catNum', 'catName')
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
    # remove any spaces before a \t
    catlist <- gsub(" +\t", "\t", catlist)
    # replace any blanks that should be \t with \t (e.g. in Moral Foundations dictionary)
    catlist <- gsub("(\\d+) +(\\d+)", "\\1\t\\2", catlist)
    # remove any \t only lines or empty lines
    if (length(blanklines <- grep("^\\s*$", catlist))) 
        catlist <- catlist[-blanklines]
    # remove spaces before and after
    catlist <- stri_trim_both(catlist)
    
    catlist <- strsplit(catlist, "\t")
    catlist <- as.data.frame(do.call(rbind, lapply(catlist, '[', 1:max(sapply(catlist, length)))), stringsAsFactors = FALSE)
    suppressWarnings(catlist[, 2:ncol(catlist)] <- sapply(catlist[, 2:ncol(catlist)], as.integer))
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
    for (i in seq_len(nrow(catlist))) {
        terms[[i]] <- as.numeric(catlist[i, !is.na(catlist[i,])])
    }
    
    for (ind in seq_along(terms)) {
        for(num in as.numeric(terms[[ind]])){
            thisCat <- guide$catName[which(guide$catNum==num)]
            thisTerm <- names(terms[ind])
            dictionary[[thisCat]] <- append(dictionary[[thisCat]], thisTerm)
        }
    }
    return(dictionary)
}
