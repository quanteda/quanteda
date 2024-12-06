# man page ----------

#' quanteda tokenizers
#'
#' Internal methods for tokenization providing default and legacy methods for
#' text segmentation.
#' @name tokenize_internal
#' @rdname tokenize_internal
#' @aliases tokenize
#' @param x (named) character; input texts
#' @details
#' Each of the word tokenizers corresponds to a major version of \pkg{quanteda},
#' kept here for backward compatibility and comparison.  `tokenize_word3()` is
#' identical to `tokenize_word2()`.
#' 
#' @return a list of characters corresponding to the (most conservative)
#'   tokenization, including whitespace where applicable; except for
#'   `tokenize_word1()`, which is a special tokenizer for Internet language that
#'   includes URLs, #hashtags, @usernames, and email addresses.
#' @keywords tokens internal
#' @examples
#' \dontrun{
#' txt <- c(doc1 = "Tweet https://quanteda.io using @quantedainit and #rstats.",
#'          doc2 = "The £1,000,000 question.",
#'          doc4 = "Line 1.\nLine2\n\nLine3.",
#'          doc5 = "?",
#'          doc6 = "Self-aware machines! \U0001f600",
#'          doc7 = "Qu'est-ce que c'est?")
#' tokenize_word2(txt)
#' tokenize_word2(txt, split_hyphens = FALSE)
#' tokenize_word1(txt, split_hyphens = FALSE)
#' tokenize_word4(txt, split_hyphens = FALSE, split_elisions = TRUE)
#' tokenize_fasterword(txt)
#' tokenize_fastestword(txt)
#' tokenize_sentence(txt)
#' tokenize_character(txt[2])
#' }
NULL

# default v2 and v3 tokenizer ----------

#' @rdname tokenize_internal
#' @importFrom stringi stri_replace_all_regex stri_detect_fixed stri_split_boundaries
#' @export
tokenize_word2 <- function(x, split_hyphens = FALSE, verbose = quanteda_options("verbose"), ...) {

    if (verbose) catm(" ...segmenting into words\n")
    m <- names(x)
    x[is.na(x)] <- "" # make NAs ""

    # this will not be needed if we can modify the ICU type rules to protect them
    # remove variant selector & whitespace with diacritical marks
    x <- stri_replace_all_regex(x, c("[\uFE00-\uFE0F]", "\\s[\u0300-\u036F]"), "",
                                vectorize_all = FALSE)

    structure(stri_split_boundaries(x, type = "word"), names = m)
}

#' @rdname tokenize_internal
#' @export
tokenize_word3 <- tokenize_word2

preserve_special <- function(x, split_hyphens = TRUE, split_tags = TRUE, verbose = FALSE) {

    name <- names(x)
    x <- as.character(x)

    hyphen <- "[\\p{Pd}]"
    username <- quanteda_options("pattern_username")
    hashtag <- quanteda_options("pattern_hashtag")
    # preserves web and email address
    address <- "(https?:\\/\\/(www\\.)?|@)[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
    
    regex <- address
    if (!split_hyphens) {
        if (verbose) catm(" ...preserving hyphens\n")
        regex <- c(regex, hyphen)
    }
    if (!split_tags) {
        if (verbose) catm(" ...preserving social media tags (#, @)\n")
        regex <- c(regex, username, hashtag)
    }

    s <- stri_extract_all_regex(x, paste(regex, collapse = "|"),  omit_no_match = TRUE)
    r <- lengths(s)
    s <- unlist_character(s, use.names = FALSE)

    # index specials
    u <- unique(s)
    u <- u[order(stri_length(u), decreasing = TRUE)] # substitute longer match first
    index <- split(rep(seq_along(x), r), factor(s, levels = u))
    if (length(index)) {
        special <- paste0("\u100000", seq_along(index), "\u100001")
        names(special) <- names(index)
        for (i in seq_along(index)) {
            x[index[[i]]] <- stri_replace_all_fixed(
                x[index[[i]]],
                names(special)[i],
                special[i],
                vectorize_all = FALSE
            )
        }
    } else {
        special <- character()
    }
    structure(x, names = name, special = special)
}

restore_special <- function(x, special, recompile = TRUE) {

    if (!length(special))
        return(x)

    type <- get_types(x)
    # extract all placeholders
    d <- stri_extract_all_regex(type, "\u100000\\d+\u100001", omit_no_match = TRUE)
    r <- lengths(d)
    d <- unlist_character(d, use.names = FALSE)

    # index placeholders
    index <- split(rep(seq_along(type), r), factor(d, levels = unique(d)))
    if (length(index)) {
        pos <- fastmatch::fmatch(names(index), special)
        for (i in seq_along(index)) {
            type[index[[i]]] <- stri_replace_all_fixed(
                type[index[[i]]],
                special[pos[i]],
                names(special)[pos[i]],
                vectorize_all = FALSE
            )
        }
    }
    if (!identical(type, get_types(x))) {
        set_types(x) <- type
    }
    return(x)
}

# v4 tokenizer ---------

#' @rdname tokenize_internal
#' @export
tokenize_word4 <- function(x, split_hyphens = FALSE, split_tags = FALSE, split_elisions = FALSE,
                           verbose = quanteda_options("verbose"), ...) {
    
    rules <- breakrules_get("word")
    if (!split_hyphens) {
        if (verbose) catm(" ...preserving hyphens\n")
    } else {
        rules[["keep_hyphens"]] <- NULL
    }
    if (!split_elisions) {
        # NOTE: consider splitting in tokens.tokens()
        if (verbose) catm(" ...preserving elisions\n")
        rules[["split_elisions"]] <- NULL
    }

    username <- quanteda_options("pattern_username")
    hashtag <- quanteda_options("pattern_hashtag")
    
    url <- "([-a-zA-Z0-9+.]{2,20}://|www\\.)[-+a-zA-Z0-9@#:.%~=_&/]+(\\.[-a-zA-Z0-9]+)+([/?#][-+a-zA-Z0-9@#:.%~=_&]+)*[/?#]?"
    email <- "[-+a-zA-Z0-9_.]+@[-a-zA-Z0-9]+(\\.[-a-zA-Z0-9]+)*\\.[a-z]+"
    regex <- c(email, url)
    if (!split_tags) {
        if (verbose) catm(" ...preserving social media tags (#, @)\n")
        regex <- c(regex, username, hashtag)
        x <- stri_replace_all_regex(x, paste(regex, collapse = "|"), "\uE001$0\uE002")
        rules[["split_tags"]] <- NULL
    }
    # if (!split_tags) {
    #     if (verbose) catm(" ...preserving social media tags (#, @)\n")
    #     # NOTE: default patterns are protected by keep_tags
    #     rules[["keep_usernames"]] <- paste0(stri_replace_all_fixed(quanteda_options("pattern_username"), "@", "\\@"), ";")
    #     rules[["keep_hashtags"]] <- paste0(stri_replace_all_fixed(quanteda_options("pattern_hashtag"), "#", "\\#"), ";")
    # } else {
    #     rules[["keep_tags"]] <- NULL
    # }
    tokenize_custom(x, rules)
}

#' Customizable tokenizer
#'
#' Allows users to tokenize texts using customized boundary rules. See the [ICU
#' website](https://unicode-org.github.io/icu/userguide/boundaryanalysis/break-rules.html)
#' for how to define boundary rules.
#' @param x character vector for texts to tokenize
#' @param rules a list of rules for rule-based boundary detection
#' @details The package contains internal sets of rules for word and sentence 
#' breaks, which are lists
#'   of rules for word and sentence boundary detection. `base` is copied from
#'   the ICU library. Other rules are created by the package maintainers in 
#'   `system.file("breakrules/breakrules_custom.yml")`.
#'   
#'   This function allows modification of those rules, and applies them as a new
#'   tokenizer.
#'   
#'   Custom word rules:
#'   \describe{
#'   \item{`base`}{ICU's rules for detecting word/sentence boundaries}
#'   \item{`keep_hyphens`}{quanteda's rule for preserving hyphens}
#'   \item{`keep_url`}{quanteda's rule for preserving URLs}
#'   \item{`keep_email`}{quanteda's rule for preserving emails}
#'   \item{`keep_tags`}{quanteda's rule for preserving tags}
#'   \item{`split_elisions`}{quanteda's rule for splitting elisions}
#'   \item{`split_tags`}{quanteda's rule for splitting tags}
#'   }
#'
#' @return `tokenize_custom()` returns a list of characters containing tokens.
#' @importFrom stringi stri_split_boundaries
#' @source
#' <https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt>
#' 
#' <https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt>
#' @examples
#' lis <- tokenize_custom("a well-known http://example.com", rules = breakrules_get("word"))
#' tokens(lis, remove_separators = TRUE)
#' @export
tokenize_custom <- function(x, rules) {
    x[is.na(x)] <- ""
    rule <- paste0(unlist(rules), collapse = "\n")
    structure(stri_split_boundaries(x, type = rule), names = names(x))
}

#' @rdname tokenize_custom
#' @description
#' Tools for custom word and sentence breakrules, to retrieve, set, or reset
#' them to package defaults.
#' @keywords internal
#' @export
#' @param what character; which set of rules to return, one of `"word"` or
#'   `"sentence"`
#' @returns `breakrules_get()` returns the existing break rules as a list.
#' @examples
#' breakrules_get("word")
#' breakrules_get("sentence")
#' 
breakrules_get <- function(what = c("word", "sentence")) {
    what <- match.arg(what)
    if (what == "word") {
        global$breakrules_word
    } else if (what == "sentence") {
        global$breakrules_sentence
    }
}

#' @rdname tokenize_custom
#' @keywords internal
#' @export
#' @returns `breakrules_set()` returns nothing but reassigns the global
#'   breakrules to `x`.
#' @examples
#' brw <- breakrules_get("word")
#' brw$keep_email <- "@[a-zA-Z0-9_]+"
#' breakrules_set(brw, what = "word")
breakrules_set <- function(x, what = c("word", "sentence")) {
    what <- match.arg(what)
    if (what == "word") {
        global$breakrules_word <- x
    } else if (what == "sentence") {
        global$breakrules_sentence <- x
    }
}

#' @rdname tokenize_custom
#' @keywords internal
#' @export
#' @returns `breakrules_reset()` returns nothing but reassigns the global
#'   breakrules to the system defaults.  These rules are defined in
#'   `system.file("breakrules/")`.
#' @examples
#' breakrules_reset("sentence")
#' breakrules_reset("word")
breakrules_reset <- function(what = c("word", "sentence")) {
    what <- match.arg(what)
    if (what == "word") {
        global$breakrules_word <-
            c(list(base = paste0(readLines(system.file("breakrules/word.txt", package = "quanteda")),
                                 collapse = "\n")),
              yaml::read_yaml(system.file("breakrules/custom.yml", package = "quanteda")))
    } else if (what == "sentence") {
        global$breakrules_sentence <-
            list(base = paste0(readLines(system.file("breakrules/sent.txt", package = "quanteda")),
                               collapse = "\n"))
    }
}


# legacy tokenizers ----------

#' @rdname tokenize_internal
#' @inheritParams tokens
#' @importFrom stringi stri_detect_regex stri_detect_charclass
#'   stri_replace_all_regex stri_detect_fixed stri_replace_all_fixed
#' @export
tokenize_word1 <- function(x, split_hyphens = FALSE, verbose = quanteda_options("verbose"), ...) {

    m <- names(x)
    x[is.na(x)] <- "" # make NAs ""

    # remove variant selector & whitespace with diacritical marks
    x <- stri_replace_all_regex(x, c("[\uFE00-\uFE0F]", "\\s[\u0300-\u036F]"), "",
                                vectorize_all = FALSE)

    if (verbose) catm(" ...segmenting into words\n")
    x <- stri_split_boundaries(x, type = "word", locale = quanteda_options("tokens_locale"))
    structure(x, names = m)
}

# substitutions to preserve hyphens and tags
preserve_special1 <- function(x, split_hyphens = TRUE, split_tags = TRUE, verbose = FALSE) {
    if (!split_hyphens) {
        if (verbose) catm(" ...preserving hyphens\n")
        x <- stri_replace_all_regex(x, "(\\w)\\p{Pd}+", "$1_hy_")
    }
    if (!split_tags) {
        if (verbose) catm(" ...preserving social media tags (#, @)\n")
        x <- stri_replace_all_fixed(x, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
    }
    return(x)
}

# re-substitute the replacement hyphens and tags
restore_special1 <- function(x, split_hyphens = TRUE, split_tags = TRUE) {
    type <- get_types(x)
    if (!split_hyphens)
        type <- stri_replace_all_fixed(type, "_hy_", "-")
    if (!split_tags)
        type <- stri_replace_all_fixed(type, c("_ht_", "_as_"), c("#", "@"),
                                       vectorize_all = FALSE)
    if (!identical(type, get_types(x))) {
        set_types(x) <- type
    }
    return(x)
}

#' @rdname tokenize_internal
#' @importFrom stringi stri_split_boundaries
#' @export
tokenize_character <- function(x, ...) {
    stri_split_boundaries(x, type = "character", simplify = FALSE)
}

#' @rdname tokenize_internal
#' @importFrom stringi stri_replace_all_regex stri_replace_all_fixed
#'   stri_split_boundaries stri_trim_right
#' @export
tokenize_sentence <- function(x, verbose = FALSE, ...) {
    if (verbose) catm(" ...segmenting into sentences\n")
    m <- names(x)
    x <- stri_replace_all_fixed(x, "\n", " ") # TODO consider removing
    x <- stri_split_boundaries(x, type = "sentence", locale = quanteda_options("tokens_locale"))
    x <- lapply(x, function(y) if (length(y)) stri_trim_right(y) else "")
    structure(x, names = m)
}

#' @rdname tokenize_internal
#' @importFrom stringi stri_split_regex
#' @export
tokenize_fasterword <- function(x, ...) {
    stri_split_regex(x, "[\\p{Z}\\p{C}]+")
}

#' @rdname tokenize_internal
#' @importFrom stringi stri_split_regex
#' @export
tokenize_fastestword <- function(x, ...) {
    stri_split_regex(x, " ")
}


normalize_characters <- function(x) {
    # convert the dreaded "curly quotes" to ASCII equivalents
    x <- stri_replace_all_fixed(x,
                                c("\u201C", "\u201D", "\u201F",
                                  "\u2018", "\u201B", "\u2019"),
                                c("\"", "\"", "\"",
                                  "\'", "\'", "\'"), vectorize_all = FALSE)

    # replace all hyphens with simple hyphen
    x <- stri_replace_all_fixed(x, c("\u2012", "\u2013", "\u2014", "\u2015", "\u2053"), "--",
                                vectorize_all = FALSE)
    x <- stri_replace_all_regex(x, c("\\p{Pd}", "\\p{Pd}{2,}"), c("-", " - "),
                                vectorize_all = FALSE)

    return(x)
}
