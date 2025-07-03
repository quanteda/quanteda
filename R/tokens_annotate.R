#' Annotate a tokens object using a dictionary
#'
#' Insert tags to matched tokens using a dictionary object.
#' @param marker characters that are added before and after the dictionary keys.
#' @inheritParams tokens_lookup
#' @inheritParams apply_if
#' @keywords tokens
#' @seealso tokens_lookup
#' @examples
#' toks1 <- tokens(data_corpus_inaugural)
#' dict1 <- dictionary(list(country = "united states",
#'                    law=c("law*", "constitution"),
#'                    freedom=c("free*", "libert*")))
#' dfm(tokens_annotate(toks1, dict1, valuetype = "glob", verbose = TRUE))
#'
#' dict2 <- dictionary(list(country = "united states",
#'                        law = c("law", "constitution"),
#'                        freedom = c("freedom", "liberty")))
#' # dfm(applyDictionary(toks1, dict2, valuetype = "fixed"))
#' dfm(tokens_annotate(toks1, dict2, valuetype = "fixed"))
#'
#' # hierarchical dictionary example
#' txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
#'          d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
#' toks2 <- tokens(txt)
#' dict3 <- dictionary(list(US = list(Countries = c("States"),
#'                                   oceans = c("Atlantic", "Pacific")),
#'                         Europe = list(Countries = c("Britain", "Ireland"),
#'                                       oceans = list(west = "Irish Sea",
#'                                                     east = "English Channel"))))
#' tokens_annotate(toks2, dict3, levels = 1)
#' tokens_annotate(toks2, dict3, levels = 2)
#' tokens_annotate(toks2, dict3, levels = 1:2)
#' tokens_annotate(toks2, dict3, levels = 3)
#' tokens_annotate(toks2, dict3, levels = c(1,3))
#' tokens_annotate(toks2, dict3, levels = c(2,3))
#'
#' # nested matching differences
#' dict4 <- dictionary(list(paper = "New York Times", city = "New York"))
#' toks4 <- tokens("The New York Times is a New York paper.")
#' tokens_annotate(toks4, dict4, nested_scope = "key", exclusive = FALSE)
#' tokens_annotate(toks4, dict4, nested_scope = "dictionary", exclusive = FALSE)
#'
#' @export
tokens_annotate <- function(x, dictionary, levels = 1:5,
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            marker = "#",
                            capkeys = TRUE,
                            nested_scope = c("key", "dictionary"),
                            apply_if = NULL,
                            verbose = quanteda_options("verbose")) {
    UseMethod("tokens_annotate")
}

#' @export
tokens_annotate.default <- function(x, dictionary, levels = 1:5,
                                    valuetype = c("glob", "regex", "fixed"),
                                    case_insensitive = TRUE,
                                    marker = "#",
                                    capkeys = TRUE,
                                    nested_scope = c("key", "dictionary"),
                                    apply_if = NULL,
                                    verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_annotate")
}

#' @export
tokens_annotate.tokens_xptr <- function(x, dictionary, levels = 1:5,
                                        valuetype = c("glob", "regex", "fixed"),
                                        case_insensitive = TRUE,
                                        marker = "#",
                                        capkeys = TRUE,
                                        nested_scope = c("key", "dictionary"),
                                        apply_if = NULL,
                                        verbose = quanteda_options("verbose")) {

    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    levels <- check_integer(levels, min = 1, max_len = Inf)
    valuetype <- match.arg(valuetype)
    capkeys <- check_logical(capkeys)
    marker <- check_character(marker, max_len = 2)
    nested_scope <- match.arg(nested_scope)
    apply_if <- check_logical(apply_if, min_len = ndoc(x), max_len = ndoc(x),
                               allow_null = TRUE, allow_na = TRUE)
    verbose <- check_logical(verbose)
        
    attrs <- attributes(x)
    type <- get_types(x)
    ids <- object2id(dictionary, type, valuetype, case_insensitive,
                     concatenator = field_object(attrs, "concatenator"), 
                     levels = levels)
    overlap <- match(nested_scope, c("key", "dictionary"))
    if (is.null(apply_if))
        apply_if <- rep(TRUE, length.out = ndoc(x))
    
    key <- attr(ids, "key")
    id_key <- match(names(ids), key)
    if (capkeys)
        key <- stri_trans_toupper(key)
    if (length(marker) == 1)
        marker <- rep(marker, 2)
    key <- paste0(marker[1], key, marker[2])
    
    if (verbose)
        before <- stats_tokens(x)
    
    id_used <- unique(id_key)
    result <- cpp_tokens_lookup(x, ids, match(id_key, id_used), key[id_used], overlap, 3,
                                !apply_if, get_threads())
        
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_annotate()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_annotate.tokens <- function(x, ...) {
    as.tokens(tokens_annotate(as.tokens_xptr(x), ...))
}


