
#' working prototype for faster selectFeatures.tokenizedTexts
#' 
#' Calls C++ for super-fast selection or removal of features from a 
#' set of tokens.
#' @inheritParams selectFeatures
#' @export
selectFeatures2 <- function(x, ...) UseMethod("selectFeatures2")

#' @rdname selectFeatures2
#' @param padding (only for \code{tokenizedTexts} objects) if \code{TRUE}, leave
#'   an empty string where the removed tokens previously existed.  This is
#'   useful if a positional match is needed between the pre- and post-selected
#'   features, for instance if a window of adjacency needs to be computed.
#' @param indexing use dfm-based index to efficiently process large tokenizedTexts object
#' @export
#' @examples 
#' \dontrun{## performance comparisons
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(SOTUCorpus, removePunct = TRUE)
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' # head to head, old v. new
#' system.time(selectFeatures2(toks, stopwords("english"), "remove", verbose = FALSE))
#' system.time(selectFeatures(toks, stopwords("english"), "remove", verbose = FALSE))
#' system.time(selectFeatures2(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"))
#' system.time(selectFeatures(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"))
#' microbenchmark::microbenchmark(
#'     new = selectFeatures2(toks, stopwords("english"), "remove", verbose = FALSE),
#'     old = selectFeatures(toks, stopwords("english"), "remove", verbose = FALSE),
#'     times = 5, unit = "relative")
#' microbenchmark::microbenchmark(
#'     new = selectFeatures2(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"),
#'     old = selectFeatures(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"),
#'     times = 2, unit = "relative")
#'     
#' types <- unique(unlist(toks))
#' numbers <- types[stringi::stri_detect_regex(types, '[0-9]')]
#' microbenchmark::microbenchmark(
#'     new = selectFeatures2(toks, numbers, "remove", verbose = FALSE, valuetype = "fixed"),
#'     old = selectFeatures(toks, numbers, "remove", verbose = FALSE, valuetype = "fixed"),
#'     times = 2, unit = "s")  
#'     
#' # removing tokens before dfm, versus after
#' microbenchmark::microbenchmark(
#'     pre = dfm(selectFeatures2(toks, stopwords("english"), "remove"), verbose = FALSE),
#'     post = dfm(toks, ignoredFeatures = stopwords("english"), verbose = FALSE),
#'     times = 5, unit = "relative")
#' }
#' 
#' ## with simple examples
#' toks <- tokenize(c("This is a sentence.", "This is a second sentence."), 
#'                  removePunct = TRUE)
#' selectFeatures2(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' 
#' # how case_insensitive works
#' selectFeatures2(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "fixed", padding = TRUE, case_insensitive = FALSE)
#' selectFeatures2(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' selectFeatures2(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "glob", padding = TRUE, case_insensitive = TRUE)
#' selectFeatures2(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "glob", padding = TRUE, case_insensitive = FALSE)
#' 
#' # with longer texts
#' txts <- c(exampleString, inaugTexts[2])
#' toks <- tokenize(txts)
#' selectFeatures2(toks, stopwords("english"), "remove")
#' selectFeatures2(toks, stopwords("english"), "keep")
#' selectFeatures2(toks, stopwords("english"), "remove", padding = TRUE)
#' selectFeatures2(toks, stopwords("english"), "keep", padding = TRUE)
#' selectFeatures2(tokenize(encodedTexts[1]), stopwords("english"), "remove", padding = TRUE)
selectFeatures2.tokenizedTexts <- function(x, features, selection = c("keep", "remove"), 
                                          valuetype = c("glob", "regex", "fixed"),
                                          case_insensitive = TRUE, padding = FALSE, indexing = FALSE,
                                          verbose = TRUE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    originalvaluetype <- valuetype
    features <- unique(unlist(features, use.names=FALSE))  # to convert any dictionaries
    y <- deepcopy(x) # copy x to y to prevent changes in x
    n <- length(y)
    
    if(indexing){
      if(verbose) cat("Indexing tokens...\n")
      index <- dfm(y, verbose = FALSE)
      index_binary <- as(index, 'nMatrix')
      types <- colnames(index_binary)
    }else{
      types <- unique(unlist(y, use.names=FALSE))
      flag <- rep(TRUE, n)
    }

    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "fixed") {

        if (case_insensitive) {
            #types <- unique(unlist(y, use.names=FALSE))
            types_match <- types[toLower(types) %in% toLower(features)]
        } else {
            types_match <- features
        }
        if(indexing) flag <- Matrix::rowSums(index_binary[,types_match]) > 0 # identify texts where types match appear
        if(verbose) cat(sprintf("Scanning %.2f%% of texts...\n", 100 * sum(flag) / n))
        if(selection == "remove"){
            select_tokens_cppl(y, flag, types_match, TRUE, padding)
        }else{ 
            select_tokens_cppl(y, flag, types_match, FALSE, padding)
        }
    } else if (valuetype == "regex") {
        if(verbose) cat("Converting regex to fixed...\n")
        types_match <- regex2fixed(features, types) # get all the unique types that match regex
        if(indexing) flag <- Matrix::rowSums(index_binary[,types_match]) > 0 # identify texts where types match appear
        if(verbose) cat(sprintf("Scanning %.2f%% of texts...\n", 100 * sum(flag) / n))
        if (selection == "remove") {
            select_tokens_cppl(y, flag, types_match, TRUE, padding)  # search as fixed
        } else {
            select_tokens_cppl(y, flag, types_match, FALSE, padding) # search as fixed
        }
    }
    
    class(y) <- c("tokenizedTexts", class(x))
    attributes(y) <- attributes(x)
    return(y)
}

#' Convert regex to fixed
#' @param regex regular expression
#' @param types unique types of tokens
#' @export
regex2fixed <- function(regex, types){
  flag <- stringi::stri_startswith_fixed(regex, '^') & stringi::stri_endswith_fixed(regex, '$') # detect fixed patterns
  types_fixed <- stringi::stri_sub(regex[flag], 2, -2) # remove regex operators
  regex <- regex[!flag] # only non-fixed patterns
  types_match <- types[types %in% types_fixed | stringi::stri_detect_regex(types, paste0(regex, collapse='|'))]
  #cat('types_fixed -------------------\n')
  #print(types_fixed)
  #cat('-------------------\n')
  #types_match <- types[types %in% types_fixed]
  return(types_match)
}
