# tokens() functions -----------

#' Tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @rdname tokens
#' @param x a character, \link{corpus}, or \link{tokens} object to be tokenized
#' @keywords tokens
#' @export
#' @param what the unit for splitting the text, available alternatives are:
#'   \describe{ \item{\code{"word"}}{(recommended default) smartest, but
#'   slowest, word tokenization method; see
#'   \link[stringi]{stringi-search-boundaries} for details.}
#'   \item{\code{"fasterword"}}{dumber, but faster, word tokenization method,
#'   uses \code{\link[stringi]{stri_split_charclass}(x,
#'   "[\\\\p{Z}\\\\p{C}]+")}} \item{\code{"fastestword"}}{dumbest, but fastest,
#'   word tokenization method, calls \code{\link[stringi]{stri_split_fixed}(x, "
#'   ")}} \item{\code{"character"}}{tokenization into individual characters}
#'   \item{\code{"sentence"}}{sentence segmenter, smart enough to handle some
#'   exceptions in English such as "Prof. Plum killed Mrs. Peacock." (but far
#'   from perfect).} }
#' @param remove_numbers logical; if \code{TRUE} remove tokens that consist only
#'   of numbers, but not words that start with digits, e.g. \code{2day}
#' @param remove_punct logical; if \code{TRUE} remove all characters in the
#'   Unicode "Punctuation" [P] class
#' @param remove_symbols logical; if \code{TRUE} remove all characters in the
#'   Unicode "Symbol" [S] class
#' @param remove_twitter logical; if \code{TRUE} remove Twitter characters
#'   \code{@@} and \code{#}; set to \code{TRUE} if you wish to eliminate these.
#'   Note that this will always be set to \code{FALSE} if \code{remove_punct =
#'   FALSE}.
#' @param remove_url logical; if \code{TRUE} find and eliminate URLs beginning
#'   with http(s) -- see section "Dealing with URLs".
#' @param remove_hyphens logical; if \code{TRUE} split words that are connected
#'   by hyphenation and hyphenation-like characters in between words, e.g.
#'   \code{"self-storage"} becomes \code{c("self", "storage")}.  Default is
#'   \code{FALSE} to preserve such words as is, with the hyphens.  Only applies
#'   if \code{what = "word"} or \code{what = "fasterword"}.
#' @param remove_separators logical; if \code{TRUE} remove separators and
#'   separator characters (Unicode "Separator" [Z] and "Control [C]"
#'   categories). Only applicable for \code{what = "character"} (when you
#'   probably want it to be \code{FALSE}) and for \code{what = "word"} (when you
#'   probably want it to be \code{TRUE}).
#' @param ngrams integer vector of the \emph{n} for \emph{n}-grams, defaulting
#'   to \code{1} (unigrams). For bigrams, for instance, use \code{2}; for
#'   bigrams and unigrams, use \code{1:2}.  You can even include irregular
#'   sequences such as \code{2:3} for bigrams and trigrams only.  See
#'   \code{\link{tokens_ngrams}}.
#' @param skip integer vector specifying the skips for skip-grams, default is 0
#'   for only immediately neighbouring words. Only applies if \code{ngrams} is
#'   different from the default of 1.  See \code{\link{tokens_skipgrams}}.
#' @param concatenator character to use in concatenating \emph{n}-grams, default
#'   is "\code{_}", which is recommended since this is included in the regular
#'   expression and Unicode definitions of "word" characters
#' @param verbose if \code{TRUE}, print timing messages to the console; off by
#'   default
#' @param include_docvars if \code{TRUE}, pass docvars and metadoc fields
#'   through to the tokens object.  Only applies when tokenizing \link{corpus}
#'   objects.
#' @param ... additional arguments not used
#' @import stringi
#' @details The tokenizer is designed to be fast and flexible as well as to
#'   handle Unicode correctly. Most of the time, users will construct \link{dfm}
#'   objects from texts or a corpus, without calling \code{tokens()} as an
#'   intermediate step.  Since \code{tokens()} is most likely to be used by more
#'   technical users, we have set its options to default to minimal
#'   intervention. This means that punctuation is tokenized as well, and that
#'   nothing is removed by default from the text being tokenized except
#'   inter-word spacing and equivalent characters.
#'
#'   Note that a \code{tokens} constructor also works on \link{tokens} objects,
#'   which allows setting additional options that will modify the original
#'   object. It is not possible, however, to change a setting to "un-remove"
#'   something that was removed from the input \link{tokens} object, however.
#'   For instance, \code{tokens(tokens("Ha!", remove_punct = TRUE), remove_punct
#'   = FALSE)} will not restore the \code{"!"} token.  No warning is currently
#'   issued about this, so the user should use \code{tokens.tokens()} with
#'   caution.
#'
#' @section Dealing with URLs: URLs are tricky to tokenize, because they contain
#'   a number of symbols and punctuation characters.  If you wish to remove
#'   these, as most people do, and your text contains URLs, then you should set
#'   \code{what = "fasterword"} and \code{remove_url = TRUE}.  If you wish to
#'   keep the URLs, but do not want them mangled, then your options are more
#'   limited, since removing punctuation and symbols will also remove them from
#'   URLs.  We are working on improving this behaviour.
#'
#'   See the examples below.
#' @return \pkg{quanteda} \code{tokens} class object, by default a serialized
#'   list of integers corresponding to a vector of types.
#' @seealso \code{\link{tokens_ngrams}}, \code{\link{tokens_skipgrams}},
#'   \code{\link{as.list.tokens}}
#' @keywords tokens
#' @examples
#' txt1 <- c(doc1 = "This is a sample: of tokens.",
#'          doc2 = "Another sentence, to demonstrate how tokens works.")
#' tokens(txt1)
#' # removing punctuation marks and lowecasing texts
#' tokens(char_tolower(txt1), remove_punct = TRUE)
#' # keeping versus removing hyphens
#' tokens("quanteda data objects are auto-loading.", remove_punct = TRUE)
#' tokens("quanteda data objects are auto-loading.", remove_punct = TRUE, remove_hyphens = TRUE)
#' # keeping versus removing symbols
#' tokens("<tags> and other + symbols.", remove_symbols = FALSE)
#' tokens("<tags> and other + symbols.", remove_symbols = TRUE)
#' tokens("<tags> and other + symbols.", remove_symbols = FALSE, what = "fasterword")
#' tokens("<tags> and other + symbols.", remove_symbols = TRUE, what = "fasterword")
#'
#' # examples with URLs - hardly perfect!
#' txt2 <- "Repo https://githib.com/quanteda/quanteda, and www.stackoverflow.com."
#' tokens(txt2, remove_url = TRUE, remove_punct = TRUE)
#' tokens(txt2, remove_url = FALSE, remove_punct = TRUE)
#' tokens(txt2, remove_url = FALSE, remove_punct = TRUE, what = "fasterword")
#' tokens(txt2, remove_url = FALSE, remove_punct = FALSE, what = "fasterword")
#'
#'
#' ## MORE COMPARISONS
#' txt3 <- "#textanalysis is MY <3 4U @@myhandle gr8 #stuff :-)"
#' tokens(txt3, remove_punct = TRUE)
#' tokens(txt3, remove_punct = TRUE, remove_twitter = TRUE)
#' tokens("great website http://textasdata.com", remove_url = FALSE)
#' tokens("great website http://textasdata.com", remove_url = TRUE)
#'
#' txt4 <- c(text1="This is $10 in 999 different ways,\n up and down; left and right!",
#'          text2="@@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
#' tokens(txt4, verbose = TRUE)
#' tokens(txt4, remove_numbers = TRUE, remove_punct = TRUE)
#' tokens(txt4, remove_numbers = FALSE, remove_punct = TRUE)
#' tokens(txt4, remove_numbers = TRUE, remove_punct = FALSE)
#' tokens(txt4, remove_numbers = FALSE, remove_punct = FALSE)
#' tokens(txt4, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)
#' tokens(txt4, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE)
#'
#' # character level
#' tokens("Great website: http://textasdata.com?page=123.", what = "character")
#' tokens("Great website: http://textasdata.com?page=123.", what = "character",
#'          remove_separators = FALSE)
#'
#' # sentence level
#' tokens(c("Kurt Vongeut said; only assholes use semi-colons.",
#'            "Today is Thursday in Canberra:  It is yesterday in London.",
#'            "Today is Thursday in Canberra:  \nIt is yesterday in London.",
#'            "To be?  Or\nnot to be?"),
#'           what = "sentence")
#' tokens(data_corpus_inaugural[c(2,40)], what = "sentence")
#'
#' # removing features (stopwords) from tokenized texts
#' txt5 <- char_tolower(c(mytext1 = "This is a short test sentence.",
#'                       mytext2 = "Short.",
#'                       mytext3 = "Short, shorter, and shortest."))
#' tokens(txt5, remove_punct = TRUE)
#' tokens_remove(tokens(txt5, remove_punct = TRUE), stopwords("english"))
#'
#' # ngram tokenization
#' tokens(txt5, remove_punct = TRUE, ngrams = 2)
#' tokens(txt5, remove_punct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
#' tokens(txt5, remove_punct = TRUE, ngrams = 1:2)
#' # removing features from ngram tokens
#' tokens_remove(tokens(txt5, remove_punct = TRUE, ngrams = 1:2), stopwords("english"))
tokens <-  function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
                    remove_numbers = FALSE,
                    remove_punct = FALSE,
                    remove_symbols = FALSE,
                    remove_separators = TRUE,
                    remove_twitter = FALSE,
                    remove_hyphens = FALSE,
                    remove_url = FALSE,
                    ngrams = 1L,
                    skip = 0L,
                    concatenator = "_",
                    verbose = quanteda_options("verbose"),
                    include_docvars = TRUE,
                    ...) {
    UseMethod("tokens")
}

#' @export
tokens.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "tokens"))
}

#' @rdname tokens
#' @noRd
#' @export
tokens.character <- function(x, ...) {
    tokens(corpus(x), ...)
}


#' @rdname tokens
#' @export
#' @noRd
tokens.corpus <- function(x, ..., include_docvars = TRUE) {
    result <- tokens_internal(texts(x), ...)
    if (include_docvars) {
        docvars(result) <- documents(x)[, which(names(documents(x)) != "texts"), drop = FALSE]
    } else {
        docvars(result) <- data.frame(row.names = docnames(x))
    }
    result
}

#' @rdname tokens
#' @export
#' @noRd
tokens.tokens <-  function(x,
                           what = c("word", "sentence", "character", "fastestword", "fasterword"),
                           remove_numbers = FALSE,
                           remove_punct = FALSE,
                           remove_symbols = FALSE,
                           remove_separators = TRUE,
                           remove_twitter = FALSE,
                           remove_hyphens = FALSE,
                           remove_url = FALSE,
                           ngrams = 1L,
                           skip = 0L,
                           concatenator = "_",
                           verbose = quanteda_options("verbose"),
                           include_docvars = TRUE,
                           ...) {

    if (remove_hyphens)
        x <- tokens_split(x, separator = "\\p{Pd}", valuetype = "regex", remove_separator = FALSE)

    if (remove_twitter)
        x <- tokens_replace(x, types(x), stri_replace_first_regex(types(x), "^(@|#)", ""))

    regex <- c()
    if (remove_numbers)
        regex <- c(regex, "^[\\p{N}]+$")
    if (remove_punct)
        regex <- c(regex, "^[\\p{P}\\p{S}]+$")
    if (remove_symbols)
        regex <- c(regex, "^[\\p{S}]+$")
    if (remove_separators)
        regex <- c(regex, "^[\uFE00-\uFE0F\\p{Z}\\p{C}]+$")
    if (remove_url)
        regex <- c(regex, "^https?")

    if (length(regex))
        x <- tokens_remove(x, paste(regex, collapse = "|"), valuetype = "regex", padding = FALSE)
    if (!identical(ngrams, 1L) || !identical(skip, 0L))
        x <- tokens_ngrams(x, n = ngrams, skip = skip, concatenator = concatenator)
    if (!include_docvars)
        docvars(x) <- data.frame(row.names = docnames(x))
    return(x)
}


# coercion and checking functions -----------

#' Coercion, checking, and combining functions for tokens objects
#'
#' Coercion functions to and from \link{tokens} objects, checks for whether an
#' object is a \link{tokens} object, and functions to combine \link{tokens}
#' objects.
#' @param x object to be coerced or checked
#' @param concatenator character between multi-word expressions, default is the
#'   underscore character.  See Details.
#' @param ... additional arguments used by specific methods.  For
#'   \link{c.tokens}, these are the \link{tokens} objects to be concatenated.
#' @return \code{as.tokens} returns a quanteda \link{tokens} object.
#' @details The \code{concatenator} is used to automatically generate dictionary
#'   values for multi-word expressions in \code{\link{tokens_lookup}} and
#'   \code{\link{dfm_lookup}}. The underscore character is commonly used to join
#'   elements of multi-word expressions (e.g. "piece_of_cake", "New_York"), but
#'   other characters (e.g. whitespace " " or a hyphen "-") can also be used.
#'   In those cases, users have to tell the system what is the concatenator in
#'   your tokens so that the conversion knows to treat this character as the
#'   inter-word delimiter, when reading in the elements that will become the
#'   tokens.
#' @export
#' @rdname as.tokens
#' @examples
#'
#' # create tokens object from list of characters with custom concatenator
#' dict <- dictionary(list(country = "United States",
#'                    sea = c("Atlantic Ocean", "Pacific Ocean")))
#' lis <- list(c("The", "United-States", "has", "the", "Atlantic-Ocean",
#'               "and", "the", "Pacific-Ocean", "."))
#' toks <- as.tokens(lis, concatenator = "-")
#' tokens_lookup(toks, dict)
#'
as.tokens <- function(x, concatenator = "_", ...) {
    UseMethod("as.tokens")
}

#' @export
as.tokens.default <- function(x, concatenator = "", ...) {
    stop(friendly_class_undefined_message(class(x), "as.tokens"))
}

#' @rdname as.tokens
#' @export
as.tokens.list <- function(x, concatenator = "_", ...) {
    result <- structure(tokens_serialize(x),
                        class = "tokens",
                        names = docnames(x),
                        what = "word",
                        ngrams = 1L,
                        skip = 0L,
                        concatenator = concatenator,
                        padding = FALSE)
    docvars(result) <- data.frame(row.names = docnames(x))
    return(result)
}

#' @rdname as.tokens
#' @param use_lemma logical; if \code{TRUE}, use the lemma rather than the raw
#'   token
#' @param include_pos character; whether and which part-of-speech tag to use:
#'   \code{"none"} do not use any part of speech indicator, \code{"pos"} use the
#'   \code{pos} variable, \code{"tag"} use the \code{tag} variable.  The POS
#'   will be added to the token after \code{"concatenator"}.
#' @export
as.tokens.spacyr_parsed <- function(x, concatenator = "/",
                                    include_pos = c("none", "pos", "tag"),
                                    use_lemma = FALSE, ...) {
    token_index <-  if (use_lemma) "lemma" else "token"

    include_pos <- match.arg(include_pos)
    if (include_pos != "none") {
        x[[token_index]] <-
            paste(x[[token_index]], x[[include_pos]], sep = concatenator)
    }

    as.tokens(base::split(x[[token_index]],
                          factor(x[["doc_id"]], levels = unique(x[["doc_id"]]))))
}

#' @rdname as.tokens
#' @return \code{as.list} returns a simple list of characters from a
#'   \link{tokens} object.
#' @method as.list tokens
#' @export
as.list.tokens <- function(x, ...) {
    types <- c("", types(x))
    result <- lapply(unclass(x), function(y) types[y + 1]) # shift index to show padding
    attributes(result) <- NULL
    names(result) <- names(x)
    return(result)
}

#' @rdname as.tokens
#' @param use.names logical; preserve names if \code{TRUE}.  For
#'   \code{as.character} and \code{unlist} only.
#' @return \code{as.character} returns a character vector from a
#'   \link{tokens} object.
#' @export
as.character.tokens <- function(x, use.names = FALSE, ...) {
    unlist(as.list(x), use.names = use.names)
}

#' @rdname as.tokens
#' @export
#' @return \code{is.tokens} returns \code{TRUE} if the object is of class
#'   tokens, \code{FALSE} otherwise.
is.tokens <- function(x) "tokens" %in% class(x)

# extension of generics for tokens -----------

#' @rdname as.tokens
#' @return \code{unlist} returns a simple vector of characters from a
#'   \link{tokens} object.
#' @param recursive a required argument for \link{unlist} but inapplicable to
#'   \link{tokens} objects
#' @method unlist tokens
#' @export
unlist.tokens <- function(x, recursive = FALSE, use.names = TRUE) {
    unlist(as.list(x), use.names = use.names)
}

#' print a tokens objects
#' print method for a tokens object
#' @param x a tokens object created by \code{\link{tokens}}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokens
#' @noRd
print.tokens <- function(x, ...) {
    cat(class(x)[1], " from ", ndoc(x), " document",
        if (ndoc(x) > 1L) "s" else "", ".\n", sep = "")
    types <- c("", types(x))
    x <- lapply(unclass(x), function(y) types[y + 1]) # shift index to show padding
    class(x) <- "listof"
    print(x, ...)
}


#' @method "[" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[c(1,3)]
"[.tokens" <- function(x, i, ...) {

    if (length(x) == 1 && is.null(x[[1]])) return(x)

    error <- FALSE
    if (is.character(i) && any(!i %in% names(x))) error <- TRUE
    if (is.numeric(i) && any(i > length(x))) error <- TRUE
    if (error) stop("Subscript out of bounds")

    attrs <- attributes(x)
    x <- unclass(x)[i]
    if (is.data.frame(attrs$docvars)) {
        attrs$docvars <- attrs$docvars[i, , drop = FALSE]
    }
    attributes(x, FALSE) <- attrs
    tokens_recompile(x)
}

#' @method "[[" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[[2]]
"[[.tokens" <- function(x, i, ...) {
    types <- c("", types(x))
    types[unclass(x)[[i]] + 1] # shift index to show padding
}

#' @method "$" tokens
#' @export
#' @noRd
#' @examples
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks$d3
"$.tokens" <- function(x, i, ...) {
    x[[i]]
}

#' @method "[<-" tokens
#' @export
#' @noRd
"[<-.tokens" <- function(x, i, value) {
    stop("assignment to tokens objects is not allowed", call. = FALSE)
}

#' @method "[[<-" tokens
#' @export
#' @noRd
"[[<-.tokens" <- function(x, i, value) {
    stop("assignment to tokens objects is not allowed", call. = FALSE)
}

#' @method lengths tokens
#' @noRd
#' @export
lengths.tokens <- function(x, use.names = TRUE) {
    NextMethod()
}

#' @rdname as.tokens
#' @param t1 tokens one to be added
#' @param t2 tokens two to be added
#' @return \code{c(...)} and \code{+} return a tokens object whose documents
#'   have been added as a single sequence of documents.
#' @examples
#' # combining tokens
#' toks1 <- tokens(c(doc1 = "a b c d e", doc2 = "f g h"))
#' toks2 <- tokens(c(doc3 = "1 2 3"))
#' toks1 + toks2
#' c(toks1, toks2)
#'
#' @export
`+.tokens` <- function(t1, t2) {
    if (length(intersect(docnames(t1), docnames(t2))))
        stop("Cannot combine tokens with duplicated document names")
    if (!identical(attr(t1, "what"), attr(t2, "what")))
        stop("Cannot combine tokens in different units")
    if (!identical(attr(t1, "concatenator"), attr(t2, "concatenator")))
        stop("Cannot combine tokens with different concatenators")

    attrs <- list(what = attr(t1, "what"),
                  ngrams = sort(unique(c(attr(t1, "ngrams"), attr(t2, "ngrams")))),
                  skip = sort(unique(c(attr(t1, "skip"), attr(t2, "skip")))),
                  concatenator = attr(t1, "concatenator"),
                  docvars = data.frame(row.names = c(docnames(t1), docnames(t2))))

    docvars(t1) <- docvars(t2) <- NULL
    types2 <- types(t2)
    types1 <- types(t1)
    t2 <- unclass(t2)
    t1 <- unclass(t1)
    t2 <- lapply(t2, function(x, y) x + y, length(types1)) # shift IDs
    t1 <- c(t1, t2)
    class(t1) <- "tokens"
    types(t1) <- c(types1, types2)
    t1 <- tokens_recompile(t1)
    attributes(t1, FALSE) <- attrs
    return(t1)
}

#' @rdname as.tokens
#' @export
c.tokens <- function(...) {
    x <- list(...)
    if (length(x) == 1) return(x[[1]])
    result <- x[[1]] + x[[2]]
    if (length(x) == 2) return(result)
    for (i in seq(3, length(x)))
        result <- result + x[[i]]
    return(result)
}

# quanteda methods for tokens ---------

#' @noRd
#' @export
docnames.tokens <- function(x) {
    if (is.null(names(x))) {
        paste0("text", seq_along(x))
    } else {
        names(x)
    }
}

#' @noRd
#' @export
docnames.list <- function(x) {
    if (is.null(names(x))) {
        paste0("text", seq_along(x))
    } else {
        names(x)
    }
}



# ============== INTERNAL FUNCTIONS =======================================

# TODO we can be rename this "tokenize" once quanteda::tokenize has gone
tokens_internal <- function(x,
                            what = c("word", "sentence", "character", "fastestword", "fasterword"),
                            remove_numbers = FALSE,
                            remove_punct = FALSE,
                            remove_symbols = FALSE,
                            remove_separators = TRUE,
                            remove_twitter = FALSE,
                            remove_hyphens = FALSE,
                            remove_url = FALSE,
                            ngrams = 1L,
                            skip = 0L,
                            concatenator = "_",
                            verbose = getOption("verbose"),
                            include_docvars = TRUE,
                            ...) {


    check_dots(list(...), names(formals("tokens")))

    what <- match.arg(what)
    attrs <- attributes(x)

    # disable remove_twitter if remove_punct = FALSE
    if (!remove_punct & remove_twitter) {
        remove_twitter <- FALSE
        warning("remove_twitter reset to FALSE when remove_punct = FALSE")
    }

    if (!remove_separators && what %in% c("fasterword", "fastestword"))
        warning("remove_separators = FALSE has no effect when what = fasterword or fastestword",
                call. = FALSE, noBreaks. = TRUE)

    if (!is.integer(ngrams)) ngrams <- as.integer(ngrams)

    if (verbose) catm("Starting tokenization...\n")

    time_start <- proc.time()

    # Split x into smaller blocks to reducre peak memory consumption
    x <- split(x, ceiling(seq_along(x) / 10000))
    for (i in seq_along(x)) {

        if (verbose) catm("...tokenizing", i, "of", length(x), "blocks\n")
        if (what %in% c("word", "fasterword")) {
            temp <- preserve_special(x[[i]], remove_hyphens, remove_url, remove_twitter, verbose)
            temp <- tokens_word(temp, what, remove_numbers, remove_punct, remove_symbols,
                                remove_separators, verbose)
        } else if (what == "fastestword") {
            temp <- tokens_word(x[[i]], what, FALSE, FALSE, FALSE, FALSE, verbose)
        } else if (what == "character") {
            temp <- tokens_character(x[[i]], remove_punct, remove_symbols,
                                     remove_separators, verbose)
        } else if (what == "sentence") {
            temp <- tokens_sentence(x[[i]], verbose)
        } else {
            stop(what, " not implemented in tokens().")
        }

        if (verbose) catm("...serializing tokens ")
        if (i == 1) {
            x[[i]] <- tokens_serialize(temp)
        } else {
            x[[i]] <- tokens_serialize(temp, attr(x[[i - 1]], "types"))
        }
        if (verbose) catm(length(attr(x[[i]], "types")), "unique types\n")

    }

    x <- structure(unlist(x, recursive = FALSE), # put all the blocked results togather
                   class = "tokens",
                   names = attrs$names,
                   what = what,
                   ngrams = ngrams,
                   skip = skip,
                   concatenator = concatenator,
                   padding = FALSE,
                   types = attr(x[[length(x)]], "types") # last block has all the types
                   )
    if (what %in% c("word", "fasterword")) {

        types <- types(x)
        if (!remove_punct || remove_punct)
            types <- stri_replace_all_fixed(types, "_hy_", "-") # run this always
        if (!remove_twitter)
            types <- stri_replace_all_fixed(types, c("_ht_", "_as_"), c("#", "@"),
                                            vectorize_all = FALSE)
        if (!identical(types, types(x)))
            types(x) <- types
            x <- tokens_recompile(x)

        regex <- c()
        if (remove_numbers)
            regex <- c(regex, "^[\\p{N}]+$")
        if (remove_punct)
            regex <- c(regex, "^[\\p{P}\\p{S}]+$")
        if (remove_symbols)
            regex <- c(regex, "^[\\p{S}]+$")
        if (remove_separators)
            regex <- c(regex, "^[\\p{Z}\\p{C}]+$")
        if (remove_punct & !remove_twitter)
            regex <- c(regex, "^#+$|^@+$") # remove @ # only if not part of Twitter names
        if (length(regex))
            x <- tokens_remove(x, paste(regex, collapse = "|"), valuetype = "regex")
    }

    if (!identical(ngrams, 1L)) {
        if (verbose) catm("...creating ngrams\n")
        x <- tokens_ngrams(x, n = ngrams, skip = skip, concatenator = concatenator)
    }

    if (verbose){
        catm("...total elapsed: ", (proc.time() - time_start)[3], "seconds.\n")
        catm("Finished tokenizing and cleaning", format(length(x), big.mark = ","), "texts.\n")
    }

    return(x)
}

tokens_word <- function(txt,
                        what = "word",
                        remove_numbers = FALSE,
                        remove_punct = FALSE,
                        remove_symbols = FALSE,
                        remove_separators = TRUE,
                        verbose = FALSE){

    if (what == "fastestword") {
        tok <- stri_split_fixed(txt, " ")
    } else if (what == "fasterword") {
        tok <- stri_split_regex(txt, "[\\p{Z}\\p{C}]+")
    } else {
        # remove variant selector
        txt <- stri_replace_all_regex(txt, "[\uFE00-\uFE0F]", "")
        # remove whitespace with diacritical marks
        txt <- stri_replace_all_regex(txt, "\\s[\u0300-\u036F]", "")
        tok <- stri_split_boundaries(txt, type = "word",
                                     # this is what kills currency symbols, Twitter tags, URLs
                                     skip_word_none = remove_punct && remove_separators,
                                     # but does not remove 4u, 2day, etc.
                                     skip_word_number = remove_numbers)

    }
    return(tok)
}

preserve_special <- function(txt, remove_hyphens, remove_url, remove_twitter, verbose) {

    if (remove_hyphens) {
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1 _hy_ $2")
    } else {
        if (verbose) catm("...preserving hyphens\n")
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1_hy_$2")
    }
    if (remove_url) {
        if (verbose & remove_url) catm("...removing URLs\n")
        regex_url <- "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
        txt <- stri_replace_all_regex(txt, regex_url, "")
    }
    if (remove_twitter == FALSE) {
        if (verbose) catm("...preserving Twitter characters (#, @)\n")
        txt <- stri_replace_all_fixed(txt, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
    }
    return(txt)
}

tokens_sentence <- function(txt, verbose = FALSE){

    if (verbose) catm("...separating into sentences.\n")

    # Replace . delimiter from common title abbreviations, with _pd_
    exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph.D", "M", "MM", "St", "etc")
    findregex <- paste0("\\b(", exceptions, ")\\.")
    txt <- stri_replace_all_regex(txt, findregex, "$1_pd_", vectorize_all = FALSE)

    ## Remove newline chars
    txt <- lapply(txt, stri_replace_all_fixed, "\n", " ")

    ## Perform the tokenization
    tok <- stri_split_boundaries(txt, type = "sentence")

    ## Cleaning
    tok <- lapply(tok, function(x){
        x <- x[which(x != "")] # remove any "sentences" that were completely blanked out
        x <- stri_trim_right(x) # trim trailing spaces
        x <- stri_replace_all_fixed(x, "_pd_", ".") # replace the non-full-stop "." characters
        return(x)
    })

    return(tok)
}

tokens_character <- function(txt,
                             remove_punct = FALSE,
                             remove_symbols = FALSE,
                             remove_separators = FALSE,
                             verbose = FALSE){

    # note: does not implement remove_numbers
    tok <- stri_split_boundaries(txt, type = "character")
    if (remove_punct) {
        if (verbose) catm("...removing punctuation.\n")
        tok <- lapply(tok, function(x){
            x <- stri_replace_all_charclass(x, "[\\p{P}]", "")
            x <- x[which(x != "")]
            return(x)
        })
    }
    if (remove_symbols) {
        if (verbose) catm("...removing symbols.\n")
        tok <- lapply(tok, function(x){
            x <- stri_replace_all_charclass(x, "[\\p{S}]", "")
            x <- x[which(x != "")]
            return(x)
        })
    }
    if (remove_separators) {
        if (verbose) catm("...removing separators.\n")
        tok <- lapply(tok, function(x){
            x <- stri_subset_regex(x, "^\\p{Z}$", negate = TRUE)
            x <- x[which(x != "")]
            return(x)
        })
    }
    return(tok)
}

#' Function to serialized list-of-character tokens
#'
#' Creates a serialized object of tokens, called by \code{\link{tokens}}.
#' @param x a list of character vectors
#' @param types_reserved optional pre-existing types for mapping of tokens
#' @param ... additional arguments
#' @return a list the serialized tokens found in each text
#' @importFrom fastmatch fmatch
#' @keywords internal tokens
tokens_serialize <- function(x, types_reserved = NULL, ...) {

    attrs <- attributes(x)
    types <- unique(unlist(x, use.names = FALSE))
    types <- types[nzchar(types)]  # remove empty types
    types <- union(types_reserved, types) # prepend new types
    
    x <- lapply(x, function(x) {
        id <- fastmatch::fmatch(x, types)
        is_na <- is.na(id)
        if (length(is_na) > 0) {
            id[!is_na]
        } else {
            integer()
        }
    })

    attributes(x) <- attrs
    attr(x, "types") <- stri_trans_nfc(types) # unicode normalization
    class(x) <- "tokens"
    return(x)
}

#' recompile a serialized tokens object
#'
#' This function recompiles a serialized tokens object when the vocabulary has
#' been changed in a way that makes some of its types identical, such as
#' lowercasing when a lowercased version of the type already exists in the type
#' table, or introduces gaps in the integer map of the types.  It also
#' re-indexes the types attribute to account for types that may have become
#' duplicates, through a procedure such as stemming or lowercasing; or the
#' addition of new tokens through compounding.
#' @param x the \link{tokens} object to be recompiled
#' @param gap if \code{TRUE}, remove gaps between token IDs
#' @param dup if \code{TRUE}, merge duplicated token types into the same ID
#' @param method \code{"C++"} for C++ implementation or \code{"R"} for an older
#'   R-based method
#' @examples
#' # lowercasing
#' toks1 <- tokens(c(one = "a b c d A B C D",
#'                  two = "A B C d"))
#' attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
#' unclass(toks1)
#' unclass(quanteda:::tokens_recompile(toks1))
#'
#' # stemming
#' toks2 <- tokens("Stemming stemmed many word stems.")
#' unclass(toks2)
#' unclass(quanteda:::tokens_recompile(tokens_wordstem(toks2)))
#'
#' # compounding
#' toks3 <- tokens("One two three four.")
#' unclass(toks3)
#' unclass(tokens_compound(toks3, "two three"))
#'
#' # lookup
#' dict <- dictionary(list(test = c("one", "three")))
#' unclass(tokens_lookup(toks3, dict))
#'
#' # empty pads
#' unclass(tokens_select(toks3, dict))
#' unclass(tokens_select(toks3, dict, pad = TRUE))
#'
#' # ngrams
#' unclass(tokens_ngrams(toks3, n = 2:3))
#'
#' @keywords internal tokens
#' @author Kenneth Benoit and Kohei Watanabe
tokens_recompile <- function(x, method = c("C++", "R"), gap = TRUE, dup = TRUE) {

    method <- match.arg(method)
    attrs <- attributes(x)

    if (method == "C++") {
        x <- qatd_cpp_tokens_recompile(x, types(x), gap, dup)
        attributes(x, FALSE) <- attrs
        return(x)
    }

    # Check for padding
    index_unique <- unique(unlist(unclass(x), use.names = FALSE))
    padding <- (index_unique == 0)
    attrs$padding <- any(padding) # add padding flag
    index_unique <- index_unique[!padding] # exclude padding

    if (!gap && !dup) {
        attributes(x) <- attrs
        return(x)
    }

    # Remove gaps in the type index, if any, remap index
    if (gap) {
        if (any(is.na(match(seq_len(length(types(x))), index_unique)))) {
            types_new <- types(x)[index_unique]
            index_new <- c(0, seq_along(index_unique)) # padding index is zero but not in types
            index_unique <- c(0, index_unique) # padding index is zero but not in types
            x <- lapply(unclass(x), function(y) index_new[fastmatch::fmatch(y, index_unique)])
            attributes(x) <- attrs
            types(x) <- types_new
        }
    }

    # Reindex duplicates, if any
    if (dup) {
        if (any(duplicated(types(x)))) {
            types <- types(x)
            types_unique <- unique(types)
            index_mapping <- match(types, types_unique)
            index_mapping <- c(0, index_mapping) # padding index is zero but not in types
            x <- lapply(unclass(x), function(y) index_mapping[y + 1]) # shift index for padding
            attributes(x) <- attrs
            types(x) <- types_unique
        }
    }
    Encoding(types(x)) <- "UTF-8"
    return(x)
}

get_tokens <- function(x) {
    UseMethod("get_tokens")
}

get_tokens.tokens <- function(x) {
    as.list(x)
}

#' Get word types from a tokens object
#'
#' Get unique types of tokens from a \link{tokens} object.
#' @param x a tokens object
#' @export
#' @seealso \link{featnames}
#' @examples
#' toks <- tokens(data_corpus_inaugural)
#' types(toks)
types <- function(x) {
    UseMethod("types")
}

#' @export
types.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "types"))
}

#' @export
types.tokens <- function(x) {
    attr(x, "types")
}

"types<-" <- function(x, value) {
    UseMethod("types<-")
}

"types<-.tokens" <- function(x, value) {
    if (!is.character(value))
        stop("replacement value must be character")
    attr(x, "types") <- value
    return(x)
}
