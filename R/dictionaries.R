
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
    
    for (i in seq_along(dict)) {
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
    
    for (i in seq_along(dict)) {
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
#' Create a \pkg{quanteda} dictionary class object, either from a list or by importing from a 
#' foreign format.  Currently supported input file formats are the Wordstat,
#' LIWC, Lexicoder v2 and v3, and Yoshikoder formats.  The import using the 
#' LIWC format works with 
#' all currently available dictionary files supplied as part of the LIWC 2001, 
#' 2007, and 2015 software (see References).
#' @param x a named list of character vector dictionary entries, including \link{valuetype} pattern
#'  matches, and including multi-word expressions separated by \code{concatenator}.  See examples.
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
#' @param tolower if \code{TRUE}, convert all dictionary values to lowercase
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
#'   Lexicoder format, \url{http://www.lexicoder.com}
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
dictionary <- function(x, file = NULL, format = NULL, 
                       concatenator = " ", 
                       tolower = TRUE, encoding = "auto") {

    if (!missing(x)) {
        if (!is.list(x)) {
            stop("Dictionaries must be named lists or lists of named lists.")
        }
    }
    
    formats <- c(cat = "wordstat", dic = "LIWC", ykd = "yoshikoder", lcd = "yoshikoder", 
                 lc3 = "lexicoder", yml = "YAML")
    
    if (!is.null(file)) {
        if (is.null(format)) {
            ext <- file_ext(file)
            if (ext %in% names(formats)) {
                format <- formats[[ext]]
            } else {
                stop(paste("Unknown dictionary file extension", ext))
            }
        } else {
            format <- match.arg(format, formats)
        }
        
        if (format == "wordstat") {
            x <- read_dict_wordstat(file, encoding)
        } else if (format == "LIWC") {
            x <- read_dict_liwc(file, encoding)
        } else if (format == "yoshikoder") {
            x <- read_dict_yoshikoder(file)
        } else if (format == "lexicoder") {
            x <- read_dict_lexicoder(file)
        } else if (format == "YAML") {
            x <- yaml::yaml.load_file(file, as.named.list = TRUE)
        }
        if (tolower)
            x <- lapply(x, stringi::stri_trans_tolower)
    }
    new("dictionary", x, format = format, file = file, concatenator = concatenator)
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
#  flatten_dictionary(dictPopulismEN)
# 
#  hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
#                              level1a2 = c("l1a21", "l1a22")),
#                level1b = list(level1b1 = c("l1b11", "l1b12"),
#                               level1b2 = c("l1b21", "l1b22", "l1b23")),
#                level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
#                               level1c1b = list(level1c1b1 = c("lowestalone"))))
#  flatten_dictionary(hdict)
#  flatten_dictionary(hdict, 2)
#  flatten_dictionary(hdict, 1:2)

flatten_dictionary <- function(dict, levels = 1:100, level = 1, key = '', dict_flat = list()) {
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
            dict_flat <- flatten_dictionary(entry, levels, level + 1, key_entry, dict_flat)
        } else {
            #cat("Vector:\n")
            #print(entry)
            dict_flat[[key_entry]] <- c(dict_flat[[key_entry]], entry)
        }
    }
    attributes(dict_flat, FALSE) <- attributes(dict)
    return(dict_flat)
}

#' check if an object is a dictionary
#' 
#' Return \code{TRUE} if an object is a \pkg{quanteda} \link{dictionary}.
#' @param x any object
#' @export
is.dictionary <- function(x) {
    is(x, "dictionary")
}



# Import a Lexicoder dictionary
#' dict <- read_dict_lexicoder('/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015.lc3')
read_dict_lexicoder <- function(path) {
    
    lines <- stringi::stri_read_lines(path, encoding = 'utf-8')
    lines <- stringi::stri_enc_toutf8(lines)
    lines <- stringi::stri_trim_both(lines)
    lines_yaml <- ifelse(stringi::stri_detect_regex(lines, '^\\+'),
                         stringi::stri_replace_all_regex(lines, '^+(.+)$', '"$1":'),
                         stringi::stri_replace_all_regex(lines, '^(.+)$', ' - "$1"'))
    lines_yaml <- stringi::stri_replace_all_regex(lines_yaml, '[[:control:]]', '') # clean
    yaml <- paste0(lines_yaml, collapse = '\n')
    dict <- yaml::yaml.load(yaml, as.named.list = TRUE)
    return(dict)
}

# Import a Wordstat dictionary
#' dict <- read_dict_wordstat('/home/kohei/Documents/Dictionary/Wordstat/ROGET.cat', 'utf-8')
#' dict <- read_dict_wordstat('/home/kohei/Documents/Dictionary/Wordstat/WordStat Sentiments.cat', 'iso-8859-1')
read_dict_wordstat <- function(path, encoding = 'auto') {
    
    lines <- stringi::stri_read_lines(path, encoding = encoding)
    lines <- stringi::stri_enc_toutf8(lines)
    lines <- stringi::stri_trim_right(lines)
    lines_yaml <- ifelse(stringi::stri_detect_regex(lines, ' \\(\\d\\)$'),
                         stringi::stri_replace_all_regex(lines, '^(\\t*)(.+) \\(\\d\\)$', '$1- "$2"'),
                         stringi::stri_replace_all_regex(lines, '^(\\t*)(.+)$', '$1"$2": '))
    
    lines_yaml <- stringi::stri_replace_all_regex(lines_yaml, '\t', ' ')
    lines_yaml <- stringi::stri_replace_all_regex(lines_yaml, '[[:control:]]', '') # clean
    yaml <- paste0(lines_yaml, collapse = '\n')
    dict <- yaml::yaml.load(yaml, as.named.list = TRUE)
    return(dict)
}

# Import a LIWC-formatted dictionary
# read_dict_liwc('/home/kohei/Documents/Dictionary/LIWC/LIWC2007_English.dic')
read_dict_liwc <- function(path, encoding = 'auto') {
    
    lines <- stringi::stri_read_lines(path, encoding = encoding)
    lines <- stringi::stri_enc_toutf8(lines)
    lines <- stringi::stri_trim_both(lines)
    lines <- lines[lines != '']
    
    sections <- which(lines == '%')
    print(sections)
    lines_key <- lines[(sections[1] + 1):(sections[2] - 1)]
    lines_value <- lines[(sections[2] + 1):(length(lines))]
    
    keys_id <- stringi::stri_extract_first_regex(lines_key, '[^\t]+')
    keys <- stringi::stri_extract_last_regex(lines_key, '[^\t]+')
    keys <- stringi::stri_replace_all_regex(keys, '[[:control:]]', '') # clean
    
    values <- stringi::stri_extract_first_regex(lines_value, '[^\t]+')
    values <- stringi::stri_replace_all_regex(values, '[[:control:]]', '') # clean
    lines_value <- stringi::stri_replace_all_regex(lines_value, '^(.+)?\t', '') # for safety
    values_ids <- stringi::stri_extract_all_regex(lines_value, '\\d+')
    
    dict <- split(rep(values, lengths(values_ids)), as.factor(unlist(values_ids, use.names = FALSE)))
    dict <- dict[order(as.numeric(names(dict)))]
    names(dict) <- keys[match(names(dict), keys_id)]
    return(dict)
    
}

# Import a Yoshikoder dictionary
# dict <- read_dict_yoshikoder('/home/kohei/Documents/Dictionary/Yoshikoder/laver-garry-ajps.ykd')
read_dict_yoshikoder <- function(path){
    
    xml <- XML::xmlParse(path)
    root <- XML::xpathSApply(xml, "/dictionary/cnode")
    dict <- nodes2list(root[[1]])
    return(dict)
}

# Internal function for read_dict_yoshikoder
nodes2list <- function(node, dict = list()){
    nodes <- XML::xpathSApply(node, "cnode")
    if (length(nodes)) {
        for (i in seq_along(nodes)) {
            key <- XML::xmlGetAttr(nodes[[i]], name="name")
            dict[[key]] <- nodes2list(nodes[[i]], dict[[key]])
        }
    } else {
        dict <- unname(XML::xpathSApply(node, "pnode/@name"))
    }
    return(dict)
}
