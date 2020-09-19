# class definition and class functions --------
#' dictionary class objects and functions
#'
#' The `dictionary2` class constructed by [dictionary()], and associated core
#' class functions.
#' @rdname dictionary-class
#' @export
#' @keywords internal dictionary
#' @slot .Data named list of mode character, where each element name is a
#'   dictionary "key" and each element is one or more dictionary entry "values"
#'   consisting of a pattern match
#' @slot meta list of object metadata
setClass("dictionary2", contains = "list",
         slots = c(
             meta = "list"
         ),
         prototype = prototype(
             meta = list(system = list(),
                         object = list(),
                         user = list())
             )
)

setValidity("dictionary2", function(object) {
    # does every element have a name? simply needs to pass
    validate_dictionary(object)
})

# Internal function to check if dictionary entries are all characters
validate_dictionary <- function(dict) {
    attrs <- attributes(dict)
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
    if (field_object(attrs)[["separator"]] == "") {
        stop("Concatenator cannot be null or an empty string")
    }
    check_entries(dict)
}

check_entries <- function(dict) {
    for (i in seq_along(dict)) {
        entry <- dict[[i]]
        is_category <- vapply(entry, is.list, logical(1))
        if (any(!is_category)) {
            word <- unlist(entry[!is_category], use.names = FALSE)
            if (!is.character(word) || any(is.na(word))) {
                stop("Non-character entries found in dictionary key \'", names(dict[i]), "\'")
            }
        }
        if (any(is_category)) {
            category <- entry[is_category]
            check_entries(category)
        }
    }
}

#' Create a dictionary
#'
#' Create a \pkg{quanteda} dictionary class object, either from a list or by
#' importing from a foreign format.  Currently supported input file formats are
#' the WordStat, LIWC, Lexicoder v2 and v3, and Yoshikoder formats.  The import
#' using the LIWC format works with all currently available dictionary files
#' supplied as part of the LIWC 2001, 2007, and 2015 software (see References).
#' @param x a named list of character vector dictionary entries, including
#'   [valuetype] pattern matches, and including multi-word expressions
#'   separated by `concatenator`.  See examples. This argument may be
#'   omitted if the dictionary is read from `file`.
#' @param file file identifier for a foreign dictionary
#' @param format character identifier for the format of the foreign dictionary.
#'   If not supplied, the format is guessed from the dictionary file's
#'   extension. Available options are: \describe{
#'   \item{`"wordstat"`}{format used by Provalis Research's WordStat
#'   software} \item{`"LIWC"`}{format used by the Linguistic Inquiry and
#'   Word Count software} \item{`"yoshikoder"`}{ format used by Yoshikoder
#'   software} \item{`"lexicoder"`}{format used by Lexicoder}
#'   \item{`"YAML"`}{the standard YAML format}}
#' @param separator the character in between multi-word dictionary values. This
#'   defaults to `" "`.
#' @param encoding additional optional encoding value for reading in imported
#'   dictionaries. This uses the [iconv] labels for encoding.  See the
#'   "Encoding" section of the help for [file].
#' @param tolower if `TRUE`, convert all dictionary values to lowercase
#' @return A dictionary class object, essentially a specially classed named list
#'   of characters.
#' @details Dictionaries can be subsetted using
#'   \code{\link[=dictionary2-class]{[}} and
#'   \code{\link[=dictionary2-class]{[[}}, operating the same as the equivalent
#'   [list][dictionary2-class] operators.
#'
#'   Dictionaries can be coerced from lists using [as.dictionary()],
#'   coerced to named lists of characters using
#'   [`as.list()`][dictionary2-class], and checked using
#'   [is.dictionary()].
#' @references WordStat dictionaries page, from Provalis Research
#'   <http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/>.
#'
#'   Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J.
#'   (2007). The development and psychometric properties of LIWC2007. \[Software
#'   manual\]. Austin, TX (<https://liwc.net>).
#'
#'   Yoshikoder page, from Will Lowe
#'   <https://conjugateprior.org/software/yoshikoder/>.
#'
#'   Lexicoder format, <http://www.snsoroka.com/data-lexicoder/>
#'
#' @seealso [dfm], [as.dictionary()],
#'   [`as.list()`][dictionary2-class], [is.dictionary()]
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
#' download.file("http://bit.ly/37cV95h", tf <- tempfile())
#' dictliwc <- dictionary(file = tf, format = "LIWC")
#' head(dfm(data_corpus_inaugural, dictionary = dictliwc))
#' }
#' @export
dictionary <- function(x, file = NULL, format = NULL,
                       separator = " ",
                       tolower = TRUE, encoding = "utf-8") {
    UseMethod("dictionary")
}

# method for when x is not supplied, but file is
#' @importFrom stringi stri_trans_tolower
#' @export
dictionary.default <- function(x, file = NULL, format = NULL,
                               separator = " ",
                               tolower = TRUE, encoding = "utf-8") {
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
    build_dictionary2(x, separator = separator)
}

#' @importFrom stringi stri_length
#' @export
dictionary.list <- function(x, file = NULL, format = NULL,
                            separator = " ",
                            tolower = TRUE, encoding = "utf-8") {
    if (!is.null(file) || !is.null(format) || encoding != "utf-8")
        stop("cannot specify file, format, or encoding when x is a list")
    if (!is.character(separator) || stri_length(separator) == 0)
        stop("separator must be a non-empty character")
    x <- list2dictionary(x)
    if (tolower) x <- lowercase_dictionary_values(x)
    x <- replace_dictionary_values(x, separator, " ")
    x <- merge_dictionary_values(x)
    build_dictionary2(x, separator = separator)
}

#' @export
dictionary.dictionary2 <- function(x, file = NULL, format = NULL,
                                   separator = " ",
                                   tolower = TRUE, encoding = "utf-8") {
    x <- as.dictionary(x)
    dictionary(as.list(x), separator = separator, tolower = tolower,
               encoding = encoding)
}

# coercion and checking methods -----------

#' @param object the dictionary to be coerced
#' @param flatten flatten the nested structure if `TRUE`
#' @param levels integer vector indicating levels in the dictionary. Used only
#'   when `flatten = TRUE`.
#' @rdname dictionary-class
#' @export
setMethod("as.list",
          signature = c("dictionary2"),
          function(x, flatten = FALSE, levels = 1:100) {
              x <- as.dictionary(x)
              if (flatten) {
                  result <- flatten_dictionary(x, levels)
                  # remove added attributes
                  attributes(result)[setdiff(names(attributes(result)), "names")] <- NULL
                  return(result)
              } else {
                  simplify_dictionary(x)
              }
          })

#' Coercion and checking functions for dictionary objects
#'
#' Convert a dictionary from a different format into a \pkg{quanteda}
#' dictionary, or check to see if an object is a dictionary.
#' @param x a dictionary-like object to be coerced or checked
#' @param format input format for the object to be coerced to a
#'   [dictionary]; current legal values are a data.frame with the fields
#'   `word` and `sentiment` (as per the **tidytext** package)
#' @inheritParams dictionary
#' @return `as.dictionary` returns a \pkg{quanteda} [dictionary]
#'   object.  This conversion function differs from the [dictionary()]
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

#' @export
#' @noRd
#' @method as.dictionary dictionary2
as.dictionary.dictionary2 <- function(x, ...) {
    unused_dots(...)
    upgrade_dictionary2(x)
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
#' @return `is.dictionary` returns `TRUE` if an object is a
#'   \pkg{quanteda} [dictionary].
#' @export
#' @examples
#' is.dictionary(dictionary(list(key1 = c("val1", "val2"), key2 = "val3")))
#' # [1] TRUE
#' is.dictionary(list(key1 = c("val1", "val2"), key2 = "val3"))
#' # [1] FALSE
is.dictionary <- function(x) {
    is(x, "dictionary2")
}

#' @rdname print-quanteda
#' @aliases print.dictionary
#' @param max_nkey max number of keys to print; default is from the
#'   `print_dictionary_max_max_nkey` setting of [quanteda_options()]
#' @param max_nval max number of values to print; default is from the
#'   `print_dictionary_max_nval` setting of [quanteda_options()]
#' @export
setMethod("print", signature(x = "dictionary2"),
          function(x,
                   max_nkey = quanteda_options("print_dictionary_max_nkey"),
                   max_nval = quanteda_options("print_dictionary_max_nval"),
                   show_summary = quanteda_options("print_dictionary_summary"),
                   ...) {
              x <- as.dictionary(x)
              if (show_summary) {
                  depth <- dictionary_depth(x)
                  lev <- if (depth > 1L) " primary" else ""
                  nkey <- length(names(x))
                  cat("Dictionary object with ", nkey, lev, " key entr",
                      if (nkey == 1L) "y" else "ies", sep = "")
                  if (lev != "") cat(" and ", depth, " nested levels", sep = "")
                  cat(".\n")
              }
              invisible(print_dictionary(x, 1, max_nkey, max_nval, ...))
          })

#' @rdname print-quanteda
setMethod("show", signature(object = "dictionary2"), function(object) print(object))

# Internal function to print dictionary
print_dictionary <- function(entry, level = 1,
                             max_nkey, max_nval, show_summary, ...) {
    unused_dots(...)

    nkey <- length(entry)
    entry <- unclass(entry)
    if (max_nkey >= 0)
        entry <- head(unclass(entry), max_nkey)

    if (!length(entry)) return()
    is_category <- vapply(entry, is.list, logical(1))
    category <- entry[is_category]
    pad <- rep("  ", level - 1)
    word <- unlist(entry[!is_category], use.names = FALSE)
    if (length(word)) {
        if (max_nval < 0)
            max_nval <- length(word)
        cat(pad, "- ", paste(head(word, max_nval), collapse = ", "), sep = "")
        nval_rem <- length(word) - max_nval
        if (nval_rem > 0)
            cat(" [ ... and ",  format(nval_rem, big.mark = ","), " more ]", sep = "")
        cat("\n", sep = "")
    }
    for (i in seq_along(category)) {
            cat(pad, "- [", names(category[i]), "]:\n", sep = "")
        print_dictionary(category[[i]], level + 1, max_nkey, max_nval, show_summary)
    }
    nkey_rem <- nkey - length(entry)
    if (nkey_rem > 0) {
        cat(pad, "[ reached max_nkey ... ", format(nkey_rem, big.mark = ","), " more key",
            if (nkey_rem > 1) "s", " ]\n", sep = "")
    }
}

#' @param object the dictionary to be extracted
#' @param i index for entries
#' @rdname dictionary-class
#' @export
setMethod("[",
          signature = c("dictionary2", i = "index"),
          function(x, i) {
              x <- as.dictionary(x)
              x <- unclass(x)
              attrs <- attributes(x)
              is_category <- vapply(x[i], function(y) is.list(y), logical(1))
              result <- build_dictionary2(x[i][is_category],
                                separator = field_object(attrs, "separator"),
                                valuetype = field_object(attrs, "valuetype"))
              meta(result) <- attrs[["meta"]][["user"]]
              result@meta$object <- attrs[["meta"]][["object"]]
              result
          })

#' @param object the dictionary to be extracted
#' @param i index for entries
#' @rdname dictionary-class
#' @export
setMethod("[[",
          signature = c("dictionary2", i = "index"),
          function(x, i) {
              x <- as.dictionary(x)
              x <- unclass(x)
              attrs <- attributes(x)
              is_category <- vapply(x[[i]], function(y) is.list(y), logical(1))
              if (all(is_category == FALSE)) {
                  unlist(x[[i]], use.names = FALSE)
              } else {
                  build_dictionary2(x[[i]][is_category],
                                    separator = field_object(attrs, "separator"),
                                    valuetype = field_object(attrs, "valuetype"))
              }
          })

#' @rdname dictionary-class
#' @param name the dictionary key
#' @export
`$.dictionary2` <- function(x, name) {
    x[[name]]
}

#' @rdname dictionary-class
#' @param ... [dictionary] objects to be concatenated
#' @export
setMethod("c",
          signature = c("dictionary2"),
          function(x, ...) {
              x <- as.dictionary(x)
              attrs <- attributes(x)
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
              build_dictionary2(result,
                                valuetype = field_object(attrs, "valuetype"),
                                separator = field_object(attrs, "separator"))
          })

# utility functions ----------

#' Internal function for special handling of multi-word dictionary values
#' @param dict a flatten dictionary
#' @param concatenator_dictionary concatenator from a dictionary object
#' @param concatenator_tokens concatenator from a tokens object
#' @keywords internal
#' @importFrom stringi stri_detect_fixed stri_split_fixed stri_replace_all_fixed
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
#' vectors. Works like `unlist(dictionary, recursive = TRUE)` except that
#' the recursion does not go to the bottom level.  Called by [dfm()].
#'
#' @param dict list to be flattened
#' @param levels integer vector indicating levels in the dictionary
#' @param level internal argument to pass current levels
#' @param key_parent internal argument to pass for parent keys
#' @param dict_flat internal argument to pass flattened dictionary
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
#'                              level1a2 = c("l1a21", "l1a22")),
#'               level1b = list(level1b1 = c("l1b11", "l1b12"),
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
    attributes(dict_flat, FALSE) <- attributes(dict) # will be set_attrs()
    return(dict_flat)
}

#' Internal function to lowercase dictionary values
#'
#' @param dict the dictionary whose values will be lowercased
#' @keywords dictionary internal
#' @importFrom stringi stri_trans_tolower
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
#' @param dict a [dictionary] object
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
#' @importFrom stringi stri_trim_both stri_enc_toutf8
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

#' @title Internal functions to import dictionary files
#'
#' @description Internal functions to import dictionary files in a variety of formats
#' @name read_dict_functions
#' @return a \pkg{quanteda} [dictionary] object
#' @keywords internal
NULL

#' @rdname read_dict_functions
#' @description `read_dict_lexicoder` imports Lexicoder files in the `.lc3` format.
#' @param path the full path and filename of the dictionary file to be read
#' @importFrom stringi stri_read_lines stri_trim_both
#' @keywords dictionary internal
#' @examples
#' dict <- quanteda:::read_dict_lexicoder(
#'     system.file("extdata", "LSD2015.lc3", package = "quanteda")
#' )
#'
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

#' @rdname read_dict_functions
#' @description `read_dict_wordstat` imports WordStat files in the
#'   `.cat` format.
#' @param encoding the encoding of the file to be imported
#' @examples
#'
#' \dontrun{
#' dict <- quanteda:::read_dict_wordstat(system.file("extdata", "RID.cat", package = "quanteda"))
#' # dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/LaverGarry.txt", "utf-8")
#' # dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/Wordstat/ROGET.cat", "utf-8")
#' # dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/Wordstat/WordStat Sentiments.cat",
#' #                            encoding = "iso-8859-1")
#' }
read_dict_wordstat <- function(path, encoding = "utf-8") {
    lines <- stri_read_lines(path, encoding = encoding)
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

# Internal function for read_dict_wordstat
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
nest_dictionary <- function(dict, depth) {
    if (length(dict) != length(depth))
        stop("Depth vectot must have the same length as dictionary")
    depth_max <- max(depth)
    while (depth_max > 1) {
        i_max <- which(depth == depth_max)
        for (i in i_max) {
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

#' @rdname read_dict_functions
#' @description `read_dict_liwc` imports LIWC dictionary files in the
#'   `.dic` format.
#' @importFrom stringi stri_extract_first_regex stri_extract_last_regex stri_replace_first_regex
#' stri_read_lines stri_extract_first_regex
#' @examples
#'
#' dict <- quanteda:::read_dict_liwc(
#'     system.file("extdata", "moral_foundations_dictionary.dic", package = "quanteda")
#' )
read_dict_liwc <- function(path, encoding = "utf-8") {

    line <- stri_read_lines(path, encoding = encoding)
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

#' @rdname read_dict_functions
#' @description `read_dict_yoshikoder` imports Yoshikoder files in the
#'   `.ykd` format.
#' @examples
#'
#' dict <- quanteda:::read_dict_yoshikoder(system.file("extdata", "laver_garry.ykd",
#'                                                     package = "quanteda"))
read_dict_yoshikoder <- function(path) {
    xml <- xml2::read_xml(path)
    root <- xml2::xml_find_first(xml, paste0("/", "dictionary"))
    dict <- nodes2list(root)
    dict <- list2dictionary(dict)
    return(dict)
}

# Internal function for read_dict_yoshikoder
nodes2list <- function(node, dict = list()) {
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
#' [dictionary] function into the YAML format. The YAML
#' files can be edited in text editors and imported into
#' \pkg{quanteda} again.
#' @param x a [dictionary] object
#' @return `as.yaml` a dictionary in the YAML format, as a character object
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
