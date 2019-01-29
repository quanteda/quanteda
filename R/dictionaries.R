# class definition and class functions --------

#' @rdname dictionary-class
#' @export
#' @keywords internal dictionary
#' @slot .Data named list of mode character, where each element name is a
#'   dictionary "key" and each element is one or more dictionary entry "values"
#'   consisting of a pattern match
#' @slot concatenator character object specifying space between multi-word
#'   values
setClass("dictionary2", contains = "list",
         slots = c(concatenator = "character"),
         prototype = prototype(concatenator = " "))

setValidity("dictionary2", function(object) {
    # does every element have a name? simply needs to pass
    validate_dictionary(object)
})

# Internal function to chekc if dictionary eintries are all chracters
validate_dictionary <- function(dict){
    dict <- unclass(dict)
    if (is.null(names(dict))) {
        stop("Dictionary elements must be named: ",
             paste(unlist(dict, recursive = TRUE), collapse = " "))
    }
    if (any(names(dict) == "")) {
        unnamed <- dict[which(names(dict) == "")]
        stop("Unnamed dictionary entry: ",
             paste(unlist(unnamed, use.names = FALSE), collapse = " "))
    }
    if (is.null(dict@concatenator) || dict@concatenator == "") {
        stop("Concatenator cannot be null or an empty string")
    }
    check_entries(dict)
}

check_entries <- function (dict) {
    for (i in seq_along(dict)) {
        entry <- dict[[i]]
        is_category <- vapply(entry, is.list, logical(1))
        if (any(!is_category)) {
            word <- unlist(entry[!is_category], use.names = FALSE)
            if (any(!is.character(word))) {
                word_error <- word[!is.character(word)]
                stop("Non-character entries found: ", word_error)
            }
        }
        if (any(is_category)) {
            category <- entry[is_category]
            check_entries(category)
        }
    }
}

# CORE FUNCTIONS -----------

#' Create a dictionary
#'
#' Create a \pkg{quanteda} dictionary class object, either from a list or by
#' importing from a foreign format.  Currently supported input file formats are
#' the Wordstat, LIWC, Lexicoder v2 and v3, and Yoshikoder formats.  The import
#' using the LIWC format works with all currently available dictionary files
#' supplied as part of the LIWC 2001, 2007, and 2015 software (see References).
#' @param x a named list of character vector dictionary entries, including
#'   \link{valuetype} pattern matches, and including multi-word expressions
#'   separated by \code{concatenator}.  See examples. This argument may be
#'   omitted if the dictionary is read from \code{file}.
#' @param file file identifier for a foreign dictionary
#' @param format character identifier for the format of the foreign dictionary.
#'   If not supplied, the format is guessed from the dictionary file's
#'   extension. Available options are: \describe{
#'   \item{\code{"wordstat"}}{format used by Provalis Research's Wordstat
#'   software} \item{\code{"LIWC"}}{format used by the Linguistic Inquiry and
#'   Word Count software} \item{\code{"yoshikoder"}}{ format used by Yoshikoder
#'   software} \item{\code{"lexicoder"}}{format used by Lexicoder}
#'   \item{\code{"YAML"}}{the standard YAML format}}
#' @param separator the character in between multi-word dictionary values. This
#'   defaults to \code{" "}.
#' @param encoding additional optional encoding value for reading in imported
#'   dictionaries. This uses the \link{iconv} labels for encoding.  See the
#'   "Encoding" section of the help for \link{file}.
#' @param tolower if \code{TRUE}, convert all dictionary values to lowercase
#' @return A dictionary class object, essentially a specially classed named list
#'   of characters.
#' @details Dictionaries can be subsetted using
#'   \code{\link[=dictionary2-class]{[}} and
#'   \code{\link[=dictionary2-class]{[[}}, operating the same as the equivalent
#'   \link[=dictionary2-class]{list} operators.
#'
#'   Dictionaries can be coerced from lists using \code{\link{as.dictionary}},
#'   coerced to named lists of characters using
#'   \code{\link[=dictionary2-class]{as.list}}, and checked using
#'   \code{\link{is.dictionary}}.
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
#' @seealso \link{dfm}, \code{\link{as.dictionary}},
#'   \code{\link[=dictionary2-class]{as.list}}, \code{\link{is.dictionary}}
#' @import stringi
#' @examples
#' corp <- corpus_subset(data_corpus_inaugural, Year>1900)
#' dict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxing = "taxing",
#'                           taxation = "taxation",
#'                           taxregex = "tax*",
#'                           country = "america"))
#' head(dfm(corp, dictionary = dict))
#'
#' # subset a dictionary
#' dict[1:2]
#' dict[c("christmas", "opposition")]
#' dict[["opposition"]]
#' 
#' # combine dictionaries
#' c(dict["christmas"], dict["country"])
#'
#' \dontrun{
#' # import the Laver-Garry dictionary from Provalis Research
#' dictfile <- tempfile()
#' download.file("https://provalisresearch.com/Download/LaverGarry.zip", 
#'               dictfile, mode = "wb")
#' unzip(dictfile, exdir = (td <- tempdir()))
#' dictlg <- dictionary(file = paste(td, "LaverGarry.cat", sep = "/"))
#' head(dfm(data_corpus_inaugural, dictionary = dictlg))
#'
#' # import a LIWC formatted dictionary from http://www.moralfoundations.org
#' download.file("https://goo.gl/5gmwXq", tf <- tempfile())
#' dictliwc <- dictionary(file = tf, format = "LIWC")
#' head(dfm(data_corpus_inaugural, dictionary = dictliwc))
#' }
#' @export
dictionary <- function(x, file = NULL, format = NULL,
                       separator = " ",
                       tolower = TRUE, encoding = "auto") {
    UseMethod("dictionary")
}

# method for when x is not supplied, but file is
#' @export
dictionary.default <- function(x, file = NULL, format = NULL,
                               separator = " ",
                               tolower = TRUE, encoding = "auto") {
    if (!missing(x) & is.null(file))
        stop("x must be a list if file is not specified")

    formats <- c(cat = "wordstat",
                 dic = "LIWC",
                 ykd = "yoshikoder",
                 lcd = "yoshikoder",
                 lc3 = "lexicoder",
                 yml = "YAML")

    if (!file.exists(file))
        stop("File does not exist: ", file)

    if (is.null(format)) {
        ext <- stri_trans_tolower(tools::file_ext(file))
        if (ext %in% names(formats)) {
            format <- formats[[ext]]
        } else {
            stop("Unknown dictionary file extension: ", ext)
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
        x <- list2dictionary(x)
    }
    if (tolower) x <- lowercase_dictionary_values(x)
    x <- merge_dictionary_values(x)
    new("dictionary2", x, concatenator = " ") # keep concatenator attributes
                                              # for compatibility
}

#' @export
dictionary.list <- function(x, file = NULL, format = NULL,
                            separator = " ",
                            tolower = TRUE, encoding = "auto") {
    if (!is.null(file) | !is.null(format) | encoding != "auto")
        stop("cannot specify file, format, or encoding when x is a list")
    if (!is.character(separator) || stri_length(separator) == 0)
        stop("separator must be a non-empty character")
    x <- list2dictionary(x)
    if (tolower) x <- lowercase_dictionary_values(x)
    x <- replace_dictionary_values(x, separator, " ")
    x <- merge_dictionary_values(x)
    new("dictionary2", x, concatenator = " ") # keep concatenator attributes
                                              # for compatibility
}

#' @export
dictionary.dictionary2 <- function(x, file = NULL, format = NULL,
                                   separator = " ",
                                   tolower = TRUE, encoding = "auto") {
    dictionary(as.list(x), separator = separator, tolower = tolower,
               encoding = encoding)
}

# coercion and checking methods -----------

#' Coerce a dictionary object into a list
#' @param object the dictionary to be coerced
#' @rdname dictionary-class
#' @export
setMethod("as.list",
          signature = c("dictionary2"),
          function(x) {
              simplify_dictionary(x)
          })

#' Coercion and checking functions for dictionary objects
#' 
#' Convert a dictionary from a different format into a \pkg{quanteda} 
#' dictionary, or check to see if an object is a dictionary.  
#' @param x a dictionary-like object to be coerced or checked
#' @param format input format for the object to be coerced to a
#'   \link{dictionary}; current legal values are a data.frame with the fields
#'   \code{word} and \code{sentiment} (as per the \strong{tidytext} package)
#' @inheritParams dictionary
#' @return \code{as.dictionary} returns a \pkg{quanteda} \link{dictionary}
#'   object.  This conversion function differs from the \code{\link{dictionary}}
#'   constructor function in that it converts an existing object rather than
#'   creates one from components or from a file.
#' @export
#' @examples 
#' \dontrun{
#' data(sentiments, package = "tidytext")
#' as.dictionary(subset(sentiments, lexicon == "nrc"))
#' as.dictionary(subset(sentiments, lexicon == "bing"))
#' # to convert AFINN into polarities - adjust thresholds if desired
#' datafinn <- subset(sentiments, lexicon == "AFINN")
#' datafinn[["sentiment"]] <-
#'     with(datafinn,
#'          sentiment <- ifelse(score < 0, "negative",
#'                              ifelse(score > 0, "positive", "netural"))
#'     )
#' with(datafinn, table(score, sentiment))
#' as.dictionary(datafinn)
#' 
#' dat <- data.frame(
#'     word = c("Great", "Horrible"),
#'     sentiment = c("positive", "negative")
#'     )
#' as.dictionary(dat)
#' as.dictionary(dat, tolower = FALSE)
#' }
#' 
as.dictionary <- function(x, format = c("tidytext"), separator = " ", tolower = FALSE) {
    UseMethod("as.dictionary")
}

#' @export
as.dictionary.default <- function(x, format = c("tidytext"), separator = " ", tolower = FALSE) {
    format <- match.arg(format)
    stop(friendly_class_undefined_message(class(x), "as.dictionary"))
}

#' @noRd
#' @method as.dictionary data.frame
#' @export
as.dictionary.data.frame <- function(x, format = c("tidytext"), separator = " ", tolower = FALSE) {
    format <- match.arg(format)
    
    if (format == "tidytext") {
        if (!all(c("word", "sentiment") %in% names(x)))
            stop("data.frame must contain word and sentiment columns")
        if ("lexicon" %in% names(x) && length(unique(x[["lexicon"]])) > 1)
            warning("multiple values found in a \'lexicon\' column; ",
                    "you may be mixing different dictionaries")
        if (all(is.na(x[["sentiment"]])))
            stop("sentiment values are missing")
    }
    
    dictionary(with(x, split(as.character(word), as.character(sentiment))),
               separator = separator, tolower = tolower)
}

#' @rdname as.dictionary
#' @return \code{is.dictionary} returns \code{TRUE} if an object is a
#'   \pkg{quanteda} \link{dictionary}.
#' @export
#' @examples
#' is.dictionary(dictionary(list(key1 = c("val1", "val2"), key2 = "val3")))
#' # [1] TRUE
#' is.dictionary(list(key1 = c("val1", "val2"), key2 = "val3"))
#' # [1] FALSE
is.dictionary <- function(x) {
    is(x, "dictionary2")
}

# base method extensions ------------

#' Print a dictionary object
#' 
#' Print/show method for dictionary objects.
#' @param object the dictionary to be printed
#' @rdname dictionary-class
#' @export
setMethod("show", "dictionary2",
          function(object) {
              depth <- dictionary_depth(object)
              lev <- if (depth > 1L) " primary" else ""
              nkey <- length(names(object))
              cat("Dictionary object with ", nkey, lev, " key entr",
                  if (nkey == 1L) "y" else "ies", sep = "")
              if (lev != "") cat(" and ", depth, " nested levels", sep = "")
              cat(".\n")
              print_dictionary(object)
          })

# Internal function to print dictionary
print_dictionary <- function(entry, level = 1) {
    entry <- unclass(entry)
    if (!length(entry)) return()
    is_category <- vapply(entry, is.list, logical(1))
    category <- entry[is_category]
    word <- unlist(entry[!is_category], use.names = FALSE)
    if (length(word)) {
        cat(rep("  ", level - 1), "- ",
            paste(word, collapse = ", "), "\n", sep = "")
    }
    for (i in seq_along(category)) {
        cat(rep("  ", level - 1), "- [", names(category[i]), "]:\n", sep = "")
        print_dictionary(category[[i]], level + 1)
    }
}

#' Extractor for dictionary objects
#' @param object the dictionary to be extracted
#' @param i index for entries
#' @rdname dictionary-class
#' @export
setMethod("[",
          signature = c("dictionary2", i = "index"),
          function(x, i) {
              x <- unclass(x)
              is_category <- vapply(x[i], function(y) is.list(y), logical(1))
              new("dictionary2", x[i][is_category], concatenator = x@concatenator)
          })

#' Extractor for dictionary objects
#' @param object the dictionary to be extracted
#' @param i index for entries
#' @rdname dictionary-class
#' @export
setMethod("[[",
          signature = c("dictionary2", i = "index"),
          function(x, i) {
              x <- unclass(x)
              is_category <- vapply(x[[i]], function(y) is.list(y), logical(1))
              if (all(is_category == FALSE)) {
                  unlist(x[[i]], use.names = FALSE)
              } else {
                  new("dictionary2", x[[i]][is_category], concatenator = x@concatenator)
              }
          })

#' @rdname dictionary-class
#' @param name the dictionary key
#' @export
`$.dictionary2` <- function(x, name) {
    x[[name]]
}

#' @rdname dictionary-class
#' @param ... \link{dictionary} objects to be concatenated
#' @export
setMethod("c",
          signature = c("dictionary2"),
          function(x, ...) {
              y <- list(...)
              if (length(y) == 0)
                  return(x)
              result <- c(unclass(x), unclass(y[[1]]))
              if (length(y) > 1) {
                  for (i in 2:length(y)) {
                      result <- c(result, unclass(y[[i]]))
                  }
              }
              result <- merge_dictionary_values(result)
              return(new("dictionary2", result))
          })

# utility functions ----------

#' Internal function for special handling of multi-word dictionary values
#' @param dict a flatten dictionary
#' @param concatenator_dictionary concatenator from a dictionary object
#' @param concatenator_tokens concatenator from a tokens object
#' @keywords internal
split_values <- function(dict, concatenator_dictionary, concatenator_tokens) {

    key <- rep(names(dict), lengths(dict))
    value <- unlist(dict, use.names = FALSE)
    is_multi <- stri_detect_fixed(value, concatenator_dictionary)
    if (any(is_multi)) {
        result <- vector("list", length(value) + sum(is_multi))
        l <- !seq_along(result) %in% (seq_along(value) + cumsum(is_multi))
        result[l] <- stri_split_fixed(value[is_multi], concatenator_dictionary)
        result[!l] <- as.list(stri_replace_all_fixed(value,
                                                     concatenator_dictionary,
                                                     concatenator_tokens))
        names(result) <- key[rep(seq_along(value), 1 + is_multi)]
    } else {
        result <- as.list(stri_replace_all_fixed(value,
                                                 concatenator_dictionary,
                                                 concatenator_tokens))
        names(result) <- key
    }
    return(result)
}

#' Flatten a hierarchical dictionary into a list of character vectors
#'
#' Converts a hierarchical dictionary (a named list of named lists, ending in
#' character vectors at the lowest level) into a flat list of character
#' vectors. Works like \code{unlist(dictionary, recursive=TRUE)} except that
#' the recursion does not go to the bottom level.  Called by \code{\link{dfm}}.
#'
#' @param tree list to be flattened
#' @param levels integer vector indicating levels in the dictionary
#' @param level internal argument to pass current levels
#' @param key_tree internal argument to pass for parent keys
#' @param dict internal argument to pass flattened dictionary
#' @return A dictionary flattened to variable levels
#' @keywords internal dictionary
#' @author Kohei Watanabe
#' @export
#' @examples
#' dict1 <-
#'     dictionary(list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
#'                                "corrupt*", "propagand", "politici*", "*deceit*",
#'                                "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
#'                                "dishonest*", "establishm*", "ruling*")))
#' flatten_dictionary(dict1)
#'
#' dict2 <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
#'                             level1a2 = c("l1a21", "l1a22")),
#'              level1b = list(level1b1 = c("l1b11", "l1b12"),
#'                              level1b2 = c("l1b21", "l1b22", "l1b23")),
#'               level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
#'                              level1c1b = list(level1c1b1 = c("lowestalone"))))
#' flatten_dictionary(dict2)
#' flatten_dictionary(dict2, 2)
#' flatten_dictionary(dict2, 1:2)
flatten_dictionary <- function(dict, levels = 1:100, level = 1,
                               key_parent = "", dict_flat = list()) {
    dict <- unclass(dict)
    for (i in seq_along(dict)) {
        key <- names(dict[i])
        entry <- dict[[i]]
        if (key == "" || !length(entry)) next
        if (level %in% levels) {
            if (key_parent != "") {
                key_entry <- paste(key_parent, key, sep = ".")
            } else {
                key_entry <- key
            }
        } else {
            key_entry <- key_parent
        }
        is_category <- vapply(entry, is.list, logical(1))
        dict_flat[[key_entry]] <-
            c(dict_flat[[key_entry]],
              unlist(entry[!is_category], use.names = FALSE))
        dict_flat <- flatten_dictionary(entry[is_category], levels,
                                        level + 1, key_entry, dict_flat)
    }
    dict_flat <- dict_flat[names(dict_flat) != ""]
    attributes(dict_flat, FALSE) <- attributes(dict)
    return(dict_flat)
}

#' Internal function to lowercase dictionary values
#'
#' @param dict the dictionary whose values will be lowercased
#' @keywords dictionary internal
#' @examples
#' dict <- list(KEY1 = list(SUBKEY1 = c("A", "B"),
#'                           SUBKEY2 = c("C", "D")),
#'               KEY2 = list(SUBKEY3 = c("E", "F"),
#'                           SUBKEY4 = c("G", "F", "I")),
#'               KEY3 = list(SUBKEY5 = list(SUBKEY7 = c("J", "K")),
#'                           SUBKEY6 = list(SUBKEY8 = c("L"))))
#' quanteda:::lowercase_dictionary_values(dict)
lowercase_dictionary_values <- function(dict) {
    dict <- unclass(dict)
    for (i in seq_along(dict)) {
        if (is.list(dict[[i]])) {
            dict[[i]] <- lowercase_dictionary_values(dict[[i]])
        } else {
            if (is.character(dict[[i]])) {
                dict[[i]] <- stri_trans_tolower(dict[[i]])
            }
        }
    }
    dict
}

#' Internal function to replace dictionary values
#' @param dict a \link{dictionary} object
#' @keywords internal
#' @examples
#' dict <- list(KEY1 = list(SUBKEY1 = list("A_B"),
#'                           SUBKEY2 = list("C_D")),
#'               KEY2 = list(SUBKEY3 = list("E_F"),
#'                           SUBKEY4 = list("G_F_I")),
#'               KEY3 = list(SUBKEY5 = list(SUBKEY7 = list("J_K")),
#'                           SUBKEY6 = list(SUBKEY8 = list("L"))))
#' quanteda:::replace_dictionary_values(dict, "_", " ")
replace_dictionary_values <- function(dict, from, to) {
    dict <- unclass(dict)
    for (i in seq_along(dict)) {
        if (is.list(dict[[i]])) {
            dict[[i]] <- replace_dictionary_values(dict[[i]], from, to)
        } else {
            if (is.character(dict[[i]])) {
                dict[[i]] <- stri_replace_all_fixed(dict[[i]], from, to)
            }
        }
    }
    return(dict)
}

#' Internal function to merge values of duplicated keys
#' @param dict a dictionary object
#' @keywords internal
#' @examples
#' dict <- list("A" = list(AA = list("aaaaa"), "a"), 
#'              "B" = list("b"),
#'              "C" = list("c"),
#'              "A" = list("aa"))
#' quanteda:::merge_dictionary_values(dict)
merge_dictionary_values <- function(dict) {
    name <- names(dict)
    if (is.null(name)) return(dict)
    dict_unique <- dict[!duplicated(name)]
    for (n in unique(name)) {
        for (i in which(n == name & duplicated(name))) {
            dict_unique[[n]] <- c(dict_unique[[n]], dict[[i]])
        }
        name_sub <- names(dict_unique[[n]])
        if (is.null(name_sub)) {
            is_value <- rep(TRUE, length(dict_unique[[n]]))
        } else {
            is_value <- name_sub == ""
        }
        if (any(is_value)) {
            value <- unlist(dict_unique[[n]][is_value], use.names = FALSE)
            dict_unique[[n]][is_value] <- NULL
            dict_unique[[n]] <- c(dict_unique[[n]], list(value))
        }
        dict_unique[[n]] <- merge_dictionary_values(dict_unique[[n]])
    }
    return(dict_unique)
}

#' Internal function to convert a list to a dictionary
#' 
#' A dictionary is internally a list of list to keys and values to coexist in
#' the same level.
#' @param dict list of object
#' @keywords internal
list2dictionary <- function(dict) {
    for (i in seq_along(dict)) {
        if (is.list(dict[[i]])) {
            dict[[i]] <- list2dictionary(dict[[i]])
        } else {
            if (is.character(dict[[i]])) {
                dict[[i]] <- list(unique(stri_trim_both(stri_enc_toutf8(dict[[i]]))))
            } else {
                dict[[i]] <- list(dict[[i]])
            }
        }
    }
    return(dict)
}

# import/export functions --------------

#' Import a Lexicoder dictionary
#' 
#' @param path the path from which to read in the Lexicoder dictionary
#' @keywords dictionary internal
#' @examples
#' \dontrun{
#' # dict <- read_dict_lexicoder("/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015.lc3")
#' }
read_dict_lexicoder <- function(path) {
    lines <- stri_read_lines(path, encoding = "utf-8") # Lexicoder 3.0 is always UTF-8
    lines <- stri_trim_both(lines)
    lines_yaml <- ifelse(stri_detect_regex(lines, "^\\+"),
                         stri_replace_all_regex(lines, "^+(.+)$", '"$1":'),
                         stri_replace_all_regex(lines, "^(.+)$", ' - "$1"'))
    lines_yaml <- stri_replace_all_regex(lines_yaml, "[[:control:]]", "") # clean
    yaml <- paste0(lines_yaml, collapse = "\n")
    dict <- yaml::yaml.load(yaml, as.named.list = TRUE)
    dict <- list2dictionary(dict)
    return(dict)
}

#' Import a Wordstat dictionary
#' @param path path to file to be imported
#' @param encoding the encoding of the file to be imported
#' @keywords internal dictionary
#' @examples
#' \dontrun{
#' # dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/LaverGarry.txt", "utf-8")
#' # dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/Wordstat/ROGET.cat", "utf-8")
#' # dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/Wordstat/WordStat Sentiments.cat", 
#' #                            encoding = "iso-8859-1")
#' }
read_dict_wordstat <- function(path, encoding = "auto") {
    lines <- stri_read_lines(path, encoding = encoding, fallback_encoding = "windows-1252")
    lines <- stri_trim_right(lines)
    lines_yaml <- ifelse(stri_detect_regex(lines, " \\(\\d\\)$"),
                         stri_replace_all_regex(lines, "^(\\t*)(.+) \\(\\d\\)$", '$1- "$2"'),
                         stri_replace_all_regex(lines, "^(\\t*)(.+)$", '$1- "$2": '))

    lines_yaml <- stri_replace_all_regex(lines_yaml, "\t", "  ") # needs two spaces
    lines_yaml <- stri_replace_all_regex(lines_yaml, "[[:control:]]", "") # clean
    yaml <- paste0(lines_yaml, collapse = "\n")
    dict <- yaml::yaml.load(yaml, as.named.list = TRUE)
    dict <- list2dictionary_wordstat(dict, FALSE)
    return(dict)
}

# Internal functin for read_dict_wordstat
list2dictionary_wordstat <- function(entry, omit = TRUE, dict = list()) {
    if (omit) {
        for (i in seq_along(entry)) {
            key <- names(entry[i])
            if (is.list(entry[i])) {
                dict[[key]] <- list2dictionary_wordstat(entry[[i]], FALSE)
            }
        }
    } else {
        if (length(entry)) {
            is_category <- vapply(entry, is.list, logical(1))
            category <- entry[is_category]
            for (i in seq_along(category)) {
                dict <- list2dictionary_wordstat(category[[i]], TRUE, dict)
            }
            dict[[length(dict) + 1]] <- unlist(entry[!is_category], use.names = FALSE)
        }
    }
    return(dict)
}

#' Utility function to remove empty keys
#' @param dict a flat or hierarchical dictionary
#' @keywords internal
remove_empty_keys <- function(dict) {
    for (i in rev(seq_along(dict))) {
        if (identical(dict[[i]], list(character(0)))) {
            message("note: removing empty key: ", paste(names(dict[i]), collapse = ", "))
            dict <- dict[i * -1]
        } else {
            if (!is.character(dict[[i]])) {
                dict[[i]] <- remove_empty_keys(dict[[i]])
            }
        }
    }
    return(dict)
}

#' Utility function to generate a nested list
#' @param dict a flat dictionary
#' @param depth depths of nested element
#' @keywords internal
#' @examples
#' lis <- list("A" = c("a", "aa", "aaa"), "B" = c("b", "bb"), "C" = c("c", "cc"), "D" = c("ddd"))
#' dict <- quanteda:::list2dictionary(lis)
#' quanteda:::nest_dictionary(dict, c(1, 1, 2, 2))
#' quanteda:::nest_dictionary(dict, c(1, 2, 1, 2))
#' 
nest_dictionary <- function (dict, depth) {
    if (length(dict) != length(depth))
        stop("Depth vectot must have the same length as dictionary")
    depth_max <- max(depth)
    while (depth_max > 1) {
        i_max <- which(depth == depth_max)
        for (i in i_max){
            i_parent <- tail(which(head(depth, i - 1) < depth_max), 1)

            # remove empty character vector
            if (!length(dict[[i_parent]][[1]]))
                dict[[i_parent]][[1]] <- NULL
            dict[[i_parent]] <- c(dict[[i_parent]], dict[i])
        }
        dict <- dict[i_max * -1]
        depth <- depth[i_max * -1]
        depth_max <- max(depth)
    }
    return(dict)
}

#' Import a LIWC-formatted dictionary
#' @param path a path to LIWC-formatted dictionary file
#' @param encoding encoding of a dictionary file
#' @keywords internal
#' @examples
#' \dontrun{
#' # quanteda:::read_dict_liwc("/home/kohei/Documents/Dictionary/LIWC/LIWC2007_English.dic")
#' # quanteda:::read_dict_liwc("/home/kohei/Documents/Dictionary/LIWC/LIWC2015_English.dic")
#' 
#' # dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2007_English.dic")      # WORKS
#' # dictionary(file = "/home/kohei/Documents/Dictionary/LIWC/LIWC2015_English.dic") # WORKS
#' # dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2015_English_Flat.dic") # WORKS
#' # dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic")       # FAILS
#' # dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2007_English080730.dic") # FAILS
#' }
read_dict_liwc <- function(path, encoding = "auto") {

    line <- stri_read_lines(path, encoding = encoding, fallback_encoding = "windows-1252")
    tab <- stri_extract_first_regex(line, "^\t+")
    line <- stri_trim_both(line)
    line <- line[line != ""]

    section <- which(line == "%")

    if (length(section) < 2) {
        stop("Start and end of a category legend should be marked by '%' but none were found")
    }

    line_key <- line[(section[1] + 1):(section[2] - 1)]
    tab_key <- tab[(section[1] + 1):(section[2] - 1)]
    line_value <- line[(section[2] + 1):(length(line))]

    # remove any lines with <of>
    has_oftag <- stri_detect_fixed(line_value, "<of>")
    if (any(has_oftag)) {
        catm("note: ", sum(has_oftag), " term",
             if (sum(has_oftag) > 1L) "s" else "",
             " ignored because contains unsupported <of> tag\n", sep = "")
        line_value <- line_value[!has_oftag]
    }

    # note odd parenthetical codes
    has_paren <- stri_detect_regex(line_value, "\\(.+\\)")
    if (any(has_paren)) {
        catm("note: ignoring parenthetical expressions in lines:\n")
        for (i in which(has_paren))
            catm("  [line ", i + section[2] + 1, "] ", line_value[i], "\n", sep = "")
        line_value <- stri_replace_all_regex(line_value, "\\(.+\\)", " ")
    }

    line_key <- stri_replace_all_regex(line_key, "(\\d+)\\s+", "$1\t") # fix wrong delimter
    key_id <- as.integer(stri_extract_first_regex(line_key, "\\d+"))
    key <- stri_extract_last_regex(line_key, "[^\t]+")
    depth <- ifelse(is.na(tab_key), 0, stri_length(tab_key)) + 1

    line_value <- stri_replace_all_regex(line_value, "\\s+(\\d+)", "\t$1") # fix wrong delimter
    value <- stri_extract_first_regex(line_value, "[^\t]+")
    line_value <- stri_replace_first_regex(line_value, ".+?\t", "") # remove words

    values_id <- stri_extract_all_regex(line_value, "\\d+")
    value_id <- as.integer(unlist(values_id, use.names = FALSE))
    value_rep <- rep(value, lengths(values_id))

    key <- stri_trim_both(stri_replace_all_regex(key, "[[:control:]]", "")) # clean
    value <- stri_trim_both(stri_replace_all_regex(value, "[[:control:]]", "")) # clean
    key_match <- key[match(value_id, key_id)]

    # check if all categories are defined
    is_undef <- is.na(key_match)
    if (any(is_undef)) {
        catm("note: ignoring undefined categories:\n")
        for (i in which(is_undef))
            catm("  ", value_id[i], " for ", value_rep[i], "\n", sep = "")
    }

    dict <- split(value_rep, factor(key_match, levels = key))
    dict <- lapply(dict, function(x) sort(unique(x))) # remove duplicated and sort values
    dict <- list2dictionary(dict)

    # create hieraechical structure of the LIWC 2015 format
    if (any(depth != max(depth))) {
        dict <- nest_dictionary(dict, depth)
    }
    dict <- remove_empty_keys(dict)

    dict
}

#' Import a Yoshikoder dictionary file.
#' @param path to filename to be imported
#' @keywords internal dictionary
#' @examples
#' # dict <- read_dict_yoshikoder("/home/kohei/Documents/Dictionary/Yoshikoder/laver-garry-ajps.ykd")
read_dict_yoshikoder <- function(path) {
    xml <- xml2::read_xml(path)
    root <- xml2::xml_find_first(xml, paste0("/", "dictionary"))
    dict <- nodes2list(root)
    dict <- list2dictionary(dict)
    return(dict)
}

# Internal function for read_dict_yoshikoder
nodes2list <- function(node, dict = list()){
    nodes <- xml2::xml_find_all(node, "cnode")
    if (length(nodes)) {
        for (i in seq_along(nodes)) {
            key <- xml2::xml_attrs(nodes[[i]])["name"]
            dict[[key]] <- nodes2list(nodes[[i]], dict)
        }
    } else {
        dict <-  unname(unlist(xml2::xml_attrs(xml2::xml_find_all(node, "pnode"), "name")))
    }
    return(dict)
}

#' Convert quanteda dictionary objects to the YAML format
#' 
#' Converts a \pkg{quanteda} dictionary object constructed by the 
#' \link{dictionary} function into the YAML format. The YAML 
#' files can be edited in text editors and imported into 
#' \pkg{quanteda} again.
#' @param x a \link{dictionary} object
#' @return \code{as.yaml} a dictionary in the YAML format, as a character object
#' @export
#' @examples
#' \dontrun{
#' dict <- dictionary(list(one = c("a b", "c*"), two = c("x", "y", "z??")))
#' cat(yaml <- as.yaml(dict))
#' cat(yaml, file = (yamlfile <- paste0(tempfile(), ".yml")))
#' dictionary(file = yamlfile)
#' }
as.yaml <- function(x) {
    UseMethod("as.yaml")
}

#' @noRd
#' @method as.yaml dictionary2
#' @export
as.yaml.dictionary2 <- function(x) {
    yaml <- yaml::as.yaml(simplify_dictionary(x, TRUE), indent.mapping.sequence = TRUE)
    yaml <- stri_enc_toutf8(yaml)
    return(yaml)
}

# Internal function for as.yaml to simplify dictionary objects
simplify_dictionary <- function(entry, omit = TRUE, dict = list()) {
    entry <- unclass(entry)
    if (omit) {
        dict <- simplify_dictionary(entry, FALSE)
    } else {
        if (length(entry)) {
            is_category <- vapply(entry, is.list, logical(1))
            category <- entry[is_category]
            if (any(is_category)) {
                for (i in seq_along(category)) {
                    dict[[names(category[i])]] <-
                        simplify_dictionary(category[[i]], TRUE, dict)
                }
                dict[["__"]] <- unlist(entry[!is_category], use.names = FALSE)
            } else {
                dict <- unlist(entry, use.names = FALSE)
            }
        }
    }
    return(dict)
}

# return the nested depth of a dictionary
# a dictionary with no nesting would have a depth of 1
dictionary_depth <- function(dict, depth = -1) {
    # http://stackoverflow.com/a/13433689/1270695
    dict <- unclass(dict)
    if (is.list(dict) && length(dict) > 0) {
        return(max(unlist(lapply(dict, dictionary_depth, depth = depth + 1))))
    } else {
        return(depth)
    }
}

# check if dictionary has multi-word entries
has_multiword <- function(dict) {
    any(stri_detect_fixed(unlist(dict, use.names = FALSE), attr(dict, "concatenator")))
}
