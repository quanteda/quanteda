# main functions ========

#' Calculate lexical diversity
#'
#' Calculate the lexical diversity of text(s).
#' @details `textstat_lexdiv` calculates the lexical diversity of documents
#'   using a variety of indices.
#'
#' @details In the following formulas, \eqn{N} refers to the total number of
#'   tokens, \eqn{V} to the number of types, and \eqn{f_v(i, N)} to the numbers
#'   of types occurring \eqn{i} times in a sample of length \eqn{N}.
#'   \describe{
#'
#'   \item{`"TTR"`:}{The ordinary *Type-Token Ratio*: \deqn{TTR =
#'   \frac{V}{N}}{TTR =  V / N}}
#'
#'   \item{`"C"`:}{Herdan's *C* (Herdan, 1960, as cited in Tweedie &
#'   Baayen, 1998; sometimes referred to as *LogTTR*): \deqn{C =
#'   \frac{\log{V}}{\log{N}}}{C = log(V) / log(N)}}
#'
#'   \item{`"R"`:}{Guiraud's *Root TTR* (Guiraud, 1954, as cited in
#'   Tweedie & Baayen, 1998): \deqn{R = \frac{V}{\sqrt{N}}}{R = V / sqrt(N)}}
#'
#'   \item{`"CTTR"`:}{Carroll's *Corrected TTR*: \deqn{CTTR =
#'   \frac{V}{\sqrt{2N}}}{CTTR = V / sqrt(2N)}}
#'
#'   \item{`"U"`:}{Dugast's *Uber Index*  (Dugast, 1978, as cited in
#'   Tweedie & Baayen, 1998): \deqn{U = \frac{(\log{N})^2}{\log{N} - \log{V}}}{U
#'   = log(N)^2 / log(N) - log(V)}}
#'
#'   \item{`"S"`:}{Summer's index: \deqn{S =
#'   \frac{\log{\log{V}}}{\log{\log{N}}}}{S = log(log(V)) / log(log(N))}}
#'
#'   \item{`"K"`:}{Yule's *K*  (Yule, 1944, as presented in Tweedie &
#'   Baayen, 1998, Eq. 16) is calculated by: \deqn{K = 10^4 \times
#'   \left[ -\frac{1}{N} + \sum_{i=1}^{V} f_v(i, N) \left( \frac{i}{N} \right)^2 \right] }}
#'
#'   \item{`"I"`:}{Yule's *I*  (Yule, 1944) is calculated by: \deqn{I = \frac{V^2}{M_2 - V}}
#'   \deqn{M_2 = \sum_{i=1}^{V} i^2 * f_v(i, N)}}
#'
#'   \item{`"D"`:}{Simpson's *D*  (Simpson 1949, as presented in
#'   Tweedie & Baayen, 1998, Eq. 17) is calculated by:
#'   \deqn{D = \sum_{i=1}^{V} f_v(i, N) \frac{i}{N} \frac{i-1}{N-1}}}
#'
#'   \item{`"Vm"`:}{Herdan's \eqn{V_m}  (Herdan 1955, as presented in
#'   Tweedie & Baayen, 1998, Eq. 18) is calculated by:
#'   \deqn{V_m = \sqrt{ \sum_{i=1}^{V} f_v(i, N) (i/N)^2 - \frac{i}{V} }}}
#'
#'   \item{`"Maas"`:}{Maas' indices (\eqn{a}, \eqn{\log{V_0}} &
#'   \eqn{\log{}_{e}{V_0}}): \deqn{a^2 = \frac{\log{N} -
#'   \log{V}}{\log{N}^2}}{a^2 = log(N) - log(V) / log(N)^2} \deqn{\log{V_0} =
#'   \frac{\log{V}}{\sqrt{1 - \frac{\log{V}}{\log{N}}^2}}}{log(V0) = log(V) /
#'   sqrt(1 - (log(V) / log(N)^2))} The measure was derived from a formula by
#'   Mueller (1969, as cited in Maas, 1972). \eqn{\log{}_{e}{V_0}} is equivalent
#'   to \eqn{\log{V_0}}, only with \eqn{e} as the base for the logarithms. Also
#'   calculated are \eqn{a}, \eqn{\log{V_0}} (both not the same as before) and
#'   \eqn{V'} as measures of relative vocabulary growth while the text
#'   progresses. To calculate these measures, the first half of the text and the
#'   full text will be examined (see Maas, 1972, p. 67 ff. for details).  Note:
#'   for the current method (for a dfm) there is no computation on separate
#'   halves of the text.}
#'
#'   \item{`"MATTR"`:}{The Moving-Average Type-Token Ratio (Covington &
#'   McFall, 2010) calculates TTRs for a moving window of tokens from the first
#'   to the last token, computing a TTR for each window. The MATTR is the mean
#'   of the TTRs of each window.}
#'
#'   \item{`"MSTTR"`:}{Mean Segmental Type-Token Ratio (sometimes referred
#'   to as *Split TTR*) splits the tokens into segments of the given size,
#'   TTR for each segment is calculated and the mean of these values returned.
#'   When this value is < 1.0, it splits the tokens into equal, non-overlapping
#'   sections of that size.  When this value is > 1, it defines the segments as
#'   windows of that size. Tokens at the end which do not make a full segment
#'   are ignored.}
#'   }
#'
#' @param x an [dfm] or [tokens] input object for whose documents
#'   lexical diversity will be computed
#' @param measure a character vector defining the measure to compute
#' @param remove_numbers logical; if `TRUE` remove features or tokens that
#'   consist only of numerals (the Unicode "Number" `[N]` class)
#' @param remove_punct logical; if `TRUE` remove all features or tokens
#'   that consist only of the Unicode "Punctuation" `[P]` class)
#' @param remove_symbols logical; if `TRUE` remove all features or tokens
#'   that consist only of the Unicode "Punctuation" `[S]` class)
#' @param remove_hyphens logical; if `TRUE` split words that are connected
#'   by hyphenation and hyphenation-like characters in between words, e.g.
#'   "self-storage" becomes two features or tokens "self" and "storage". Default
#'   is FALSE to preserve such words as is, with the hyphens.
#' @param log.base a numeric value defining the base of the logarithm (for
#'   measures using logarithms)
#' @param MATTR_window a numeric value defining the size of the moving window
#'   for computation of the Moving-Average Type-Token Ratio (Covington & McFall, 2010)
#' @param MSTTR_segment a numeric value defining the size of the each segment
#'   for the computation of the the Mean Segmental Type-Token Ratio (Johnson, 1944)
#' @param ... for passing arguments to other methods
#' @author Kenneth Benoit and Jiong Wei Lua. Many of the formulas have been
#'   reimplemented from functions written by Meik Michalke in the \pkg{koRpus}
#'   package.
#' @references
#'   Covington, M.A. & McFall, J.D. (2010). [Cutting the Gordian Knot: The
#'   Moving-Average Type-Token Ratio
#'   (MATTR)](https://doi.org/10.1080/09296171003643098). *Journal of
#'   Quantitative Linguistics*, 17(2), 94--100.
#'
#'   Herdan, G. (1955). [A New Derivation and Interpretation of Yule's
#'   'Characteristic' *K*](https://doi.org/10.1007/BF01587632). *Zeitschrift für
#'   angewandte Mathematik und Physik*, 6(4): 332--334.
#'
#'   Maas, H.D. (1972). Über den Zusammenhang zwischen Wortschatzumfang und
#'   Länge eines Textes. *Zeitschrift für Literaturwissenschaft und Linguistik*,
#'   2(8), 73--96.
#'
#'   McCarthy, P.M. &  Jarvis, S. (2007). [vocd: A Theoretical and Empirical
#'   Evaluation](https://doi.org/10.1177/0265532207080767). *Language Testing*,
#'   24(4), 459--488.
#'
#'   McCarthy, P.M. & Jarvis, S. (2010). [MTLD, vocd-D, and HD-D: A Validation
#'   Study of Sophisticated Approaches to Lexical Diversity
#'   Assessment](https://doi.org/10.3758/BRM.42.2.381). *Behaviour Research
#'   Methods*, 42(2), 381--392.
#'
#'   Michalke, M. (2014) *koRpus: An R Package for Text Analysis*. R package
#'   version 0.05-5. <https://reaktanz.de/?c=hacking&s=koRpus>
#'
#'   Simpson, E.H. (1949). [Measurement of
#'   Diversity](https://doi.org/10.1038/163688a0). *Nature*, 163: 688.
#'
#'   Tweedie. F.J. and Baayen, R.H. (1998). [How Variable May a Constant Be?
#'   Measures of Lexical Richness in
#'   Perspective](https://doi.org/10.1023/A:1001749303137). *Computers and the
#'   Humanities*, 32(5), 323--352.
#'
#'   Yule, G. U. (1944)  *The Statistical Study of Literary Vocabulary.*
#'   Cambridge: Cambridge University Press.
#'
#' @return A data.frame of documents and their lexical diversity scores.
#' @export
#' @examples
#' txt <- c("Anyway, like I was sayin', shrimp is the fruit of the sea. You can
#'           barbecue it, boil it, broil it, bake it, saute it.",
#'          "There's shrimp-kabobs,
#'           shrimp creole, shrimp gumbo. Pan fried, deep fried, stir-fried. There's
#'           pineapple shrimp, lemon shrimp, coconut shrimp, pepper shrimp, shrimp soup,
#'           shrimp stew, shrimp salad, shrimp and potatoes, shrimp burger, shrimp
#'           sandwich.")
#' tokens(txt) %>%
#'     textstat_lexdiv(measure = c("TTR", "CTTR", "K"))
#' dfm(txt) %>%
#'     textstat_lexdiv(measure = c("TTR", "CTTR", "K"))
#'
#' toks <- tokens(corpus_subset(data_corpus_inaugural, Year > 2000))
#' textstat_lexdiv(toks, c("CTTR", "TTR", "MATTR"), MATTR_window = 100)
textstat_lexdiv <- function(x,
                            measure = c("TTR", "C", "R", "CTTR", "U", "S", "K", "I", "D",
                                        "Vm", "Maas", "MATTR", "MSTTR", "all"),
                            remove_numbers = TRUE, remove_punct = TRUE,
                            remove_symbols = TRUE, remove_hyphens = FALSE,
                            log.base = 10,
                            MATTR_window = 100L,
                            MSTTR_segment = 100L,
                            ...) {
    UseMethod("textstat_lexdiv")
}

#' @export
textstat_lexdiv.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_lexdiv"))
}

#' @export
textstat_lexdiv.dfm <- function(x,
                                measure = c("TTR", "C", "R", "CTTR", "U", "S", "K", "I", "D",
                                            "Vm", "Maas", "all"),
                                remove_numbers = TRUE, remove_punct = TRUE,
                                remove_symbols = TRUE, remove_hyphens = FALSE,
                                log.base = 10,
                                ...) {

    unused_dots(...)
    tokens_only_measures <-  c("MATTR", "MSTTR")

    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))

    # special character handling
    # splitting hyphens
    if (remove_hyphens)
        x <- dfm_split_hyphenated_features(x)
    # other removals
    removals <- removals_regex(separators = FALSE,
                               punct = remove_punct,
                               symbols = remove_symbols,
                               numbers = remove_numbers,
                               url = TRUE)
    if (length(removals)) {
        x <- dfm_remove(x, paste(unlist(removals), collapse = "|"),
                           valuetype = "regex")
    }

    if (!sum(x))
        stop(message_error("dfm_empty after removal of numbers, symbols, punctuations, hyphens"))

    # check that no moving average methods have been requested
    if (any(tokens_only_measures %in% measure))
        stop("average-based measures are only available for tokens inputs")
    # match the measures from the function signature
    available_measures <- as.character(formals()$measure)[-1]
    # this ensures that a default will choose only the first option
    measure <- match.arg(measure, choices = available_measures,
                         several.ok = !missing(measure))
    # get all measures except "all" if "all" is specified
    if ("all" %in% measure)
        measure <- available_measures[!available_measures %in% "all"]

    compute_lexdiv_dfm_stats(x, measure = measure, log.base = log.base)
}

#' @export
textstat_lexdiv.tokens <-
    function(x,
             measure = c("TTR", "C", "R", "CTTR", "U", "S", "K", "I", "D",
                         "Vm", "Maas", "MATTR", "MSTTR", "all"),
             remove_numbers = TRUE, remove_punct = TRUE,
             remove_symbols = TRUE, remove_hyphens = FALSE,
             log.base = 10,
             MATTR_window = 100L,
             MSTTR_segment = 100L,
             ...) {

    unused_dots(...)
    tokens_only_measures <-  c("MATTR", "MSTTR")

    # additional token handling
    x <- tokens(x, split_hyphens = remove_hyphens,
                remove_numbers = remove_numbers,
                remove_symbols = remove_symbols,
                remove_punct = remove_punct,
                remove_url = TRUE)

    # get and validate measures
    available_measures <- as.character(formals()$measure)[-1]
    # this ensures that a default will choose only the first option
    measure <- match.arg(measure, choices = available_measures,
                         several.ok = !missing(measure))
    # get all measures except "all" if "all" is specified
    if ("all" %in% measure)
        measure <- available_measures[!available_measures %in% "all"]

    # which, if any, are tokens-only measures
    tokens_only_measure_index <- which(measure %in% tokens_only_measures)

    # compute all results - returns NAs for tokens-only measures
    result <- compute_lexdiv_dfm_stats(dfm(x), measure = measure, log.base = log.base)

    # if any tokens-only measures exist, compute and replace NAs with the reusults
    # removes the first column which is "documents"
    if (length(tokens_only_measure_index)) {
        result[, tokens_only_measure_index + 1] <- # add 1 because of documents column
            compute_lexdiv_tokens_stats(x,
                                        measure = measure[tokens_only_measure_index],
                                        MATTR_window = MATTR_window,
                                        MSTTR_segment = MSTTR_segment)[, -1]
    }

    return(result)

}

# internal functions to handle lexdiv statistics for dfm and tokens -------

#' @name compute_lexdiv_stats
#' @title Compute lexical diversity from a dfm or tokens
#' @description
#' Internal functions used in [textstat_lexdiv()], for computing
#' lexical diversity measures on dfms or tokens objects
#' @param x a [dfm] object
#' @param measure a list of lexical diversity measures.
#' @return a `data.frame` with a `document` column containing the
#'   input document name, followed by columns with the lexical diversity
#'   statistic, in the order in which they were supplied as the `measure`
#'   argument.
#' @keywords internal textstat
NULL

#' @rdname compute_lexdiv_stats
#' @param log.base a numeric value defining the base of the logarithm (for
#'   measures using logs)
#' @details `compute_lexdiv_dfm_stats` in an internal function that
#'   computes the lexical diversity measures from a [dfm] input.
#' @importFrom data.table :=
compute_lexdiv_dfm_stats <- function(x, measure = NULL, log.base = 10) {

    n_tokens <- n_types <- TTR <- C <- R <- CTTR <- U <- S <- Maas <-
        lgV0 <- lgeV0 <- K <- D <- Vm <- I <- NULL
    temp <- data.table(n_tokens = ntoken(x), n_types = ntype(x))

    if ("TTR" %in% measure)
        temp[, TTR := n_types / n_tokens]

    if ("C" %in% measure)
        temp[, C := log(n_types, base = log.base) / log(n_tokens, base = log.base)]

    if ("R" %in% measure)
        temp[, R := n_types / sqrt(n_tokens)]

    if ("CTTR" %in% measure)
        temp[, CTTR := n_types / sqrt(2 * n_tokens)]

    if ("U" %in% measure)
        temp[, U := log(n_tokens, base = log.base) ^ 2 /
                    (log(n_tokens, base = log.base) - log(n_types, base = log.base))]

    if ("S" %in% measure)
        temp[, S := log(log(n_types, base = log.base), base = log.base) /
                    log(log(n_tokens, base = log.base), base = log.base)]

    # computations for K, D, Vm, I
    # produces a list of data.frames that will be used for computing the measures
    if (length(intersect(c("K", "D", "Vm", "I"), measure))) {
        ViN <- lapply(docnames(x), function(y) {
            result <- as.data.frame(table(colSums(x[y, ])), stringsAsFactors = FALSE)
            names(result) <- c("i", "ViN")
            result[["i"]] <- as.integer(result[["i"]])
            result[["n_tokens"]] <- ntoken(x)[y]
            result[["n_types"]] <- ntype(x)[y]
            subset(result, result$i > 0)
      })
    }

    if ("K" %in% measure)
        temp[, K := 10 ^ 4 * vapply(ViN, function(y) sum(y$ViN * (y$i / y$n_tokens) ^ 2), numeric(1))]
    if ("I" %in% measure) {
        M_2 <- vapply(ViN, function(y) sum(y$ViN * y$i^2), numeric(1))
        M_1 <- temp$n_types
        yule_i <- (M_1 ^ 2) / (M_2 - M_1)
        yule_i[is.infinite(yule_i)] <- 0
        temp[, I := yule_i]
    }

    if ("D" %in% measure)
        temp[, D := vapply(ViN,
                           function(y) sum(y$ViN * (y$i / y$n_tokens) * ((y$i - 1) / (y$n_tokens - 1))),
                           numeric(1))]

    if ("Vm" %in% measure)
        temp[, Vm := vapply(ViN,
                            function(y) sqrt(sum(y$ViN * (y$i / y$n_tokens) ^ 2) - 1 / y$n_types[1]),
                            numeric(1))]

    if ("Maas" %in% measure) {
        measure <- c(measure, "lgV0", "lgeV0")
        temp[, Maas := sqrt((log(n_tokens, base = log.base) - log(n_types, base = log.base)) /
                             log(n_tokens, base = log.base) ^ 2)]
        temp[, lgV0 := log10(n_types) / sqrt(1 - (log10(n_types) / (log10(n_tokens) + 0)) ^ 2)]
        temp[, lgeV0 := log(n_types) / sqrt(1 - (log(n_types) / (log(n_tokens) + 0)) ^ 2)]
    }

    # return missings for tokens-only measures
    MATTR <- MSTTR <- NULL
    if ("MATTR" %in% measure) temp[, MATTR := NA]
    if ("MSTTR" %in% measure) temp[, MSTTR := NA]

    result <- data.frame(document = docnames(x), stringsAsFactors = FALSE)
    if (length(measure))
        result <- cbind(result, as.data.frame(temp[, measure, with = FALSE]))
    result[is.na(result)] <- NA
    class(result) <- c("lexdiv", "textstat", "data.frame")
    return(result)
}

#' @rdname compute_lexdiv_stats
#' @details `compute_lexdiv_tokens_stats` in an internal function that
#'   computes the lexical diversity measures from a [dfm] input.
#' @param MATTR_window a numeric value defining the size of the moving window
#'   for computation of the Moving-Average Type-Token Ratio (Covington & McFall, 2010)
#' @param MSTTR_segment a numeric value defining the size of the each segment
#'   for the computation of the the Mean Segmental Type-Token Ratio (Johnson, 1944)
compute_lexdiv_tokens_stats <- function(x, measure = c("MATTR", "MSTTR"),
                                     MATTR_window, MSTTR_segment) {
    measure <- match.arg(measure, several.ok = TRUE)
    result <- data.frame(document = docnames(x), stringsAsFactors = FALSE)

    if ("MATTR" %in% measure)
        result[["MATTR"]] <- compute_mattr(x, MATTR_window = MATTR_window)

    if ("MSTTR" %in% measure)
        result[["MSTTR"]] <- compute_msttr(x, MSTTR_segment = MSTTR_segment)

    # reorder output as originally supplied
    result <- result[, c("document", measure), drop = FALSE]
    result[is.na(result)] <- NA
    class(result) <- c("lexdiv", "textstat", "data.frame")
    return(result)
}

# specific functions for tokens-only moving average measures -----------

#' Compute the Moving-Average Type-Token Ratio (MATTR)
#'
#' From a tokens object, computes the Moving-Average Type-Token Ratio (MATTR)
#' from Covington & McFall (2010), averaging all of the sequential moving
#' windows of tokens of size `MATTR_window` across the text, returning the
#' average as the MATTR.
#' @param x a [tokens] object
#' @param MATTR_window integer; the size of the moving window for computation of
#'   TTR, between 1 and the number of tokens of the document
#' @keywords internal textstat lexdiv
compute_mattr <- function(x, MATTR_window = 100L) {

    if (MATTR_window < 1)
        stop("MATTR_window must be positive")
    if (any(ntoken(x) < MATTR_window)) {
        MATTR_window <- max(ntoken(x))
        warning("MATTR_window exceeds some documents' token lengths, resetting to ",
                MATTR_window)
    }

    # create each document window as an "ngram"
    x <- tokens_ngrams(x, n = MATTR_window, concatenator = " ")

    # get a list of TTRs by document
    temp <- lapply(as.list(x), function(y) textstat_lexdiv(dfm(y), "TTR")[["TTR"]])
    result <- unlist(lapply(temp, mean))
    return(result)
}

#' Compute the Mean Segmental Type-Token Ratio (MSTTR)
#'
#' Compute the Mean Segmental Type-Token Ratio (Johnson 1944) for a tokens input.
#' @param x input [tokens]
#' @param segment_size the size of the segment
#' @keywords internal textstat lexdiv
compute_msttr <- function(x, MSTTR_segment) {
    if (MSTTR_segment < 1)
        stop("MSTTR_segment must be positive")
    if (any(ntoken(x) < MSTTR_segment)) {
        MSTTR_segment <- max(ntoken(x))
        warning("MSTTR_segment exceeds some documents' token lengths, resetting to ",
                MSTTR_segment)
    }

    x <- tokens_chunk(x, MSTTR_segment)
    # drop remainder documents shorter than MSTTR_segment
    x <- x[lengths(x) >= MSTTR_segment]

    temp <- split(textstat_lexdiv(x, measure = "TTR")[["TTR"]],
                  attr(x, "docvars")[["docid_"]])
    result <- unlist(lapply(temp, mean))
    return(result)
}


# additional utility functions ------------

#' Split a dfm's hyphenated features into constituent parts
#'
#' Takes a dfm that contains features with hyphenated words, such as
#' "split-second" and turns them into features that split the elements
#' in the same was as `tokens(x, remove_hyphens = TRUE)` would have done.
#' @param x input [dfm]
#' @keywords internal dfm
#' @examples
#' (dfmat <- dfm("One-two one two three."))
#' quanteda:::dfm_split_hyphenated_features(dfmat)
dfm_split_hyphenated_features <- function(x) {
    # the global for matching the hyphens and similar characters
    hyphen_regex <- "^.+\\p{Pd}.+$"

    # figure out where the hyphens are
    hyphenated_index <- which(stringi::stri_detect_regex(featnames(x), hyphen_regex))

    # return dfm unmodified if no hyphenated features are found
    if (length(hyphenated_index) == 0) return(x)

    # split the hyphenated feature names into a list of components
    splitfeatures <- as.list(tokens(featnames(x)[hyphenated_index], split_hyphens = TRUE))

    # efficiently create a new dfm from hyphenated feature name components
    splitdfm <- x[, rep(hyphenated_index, times = lengths(splitfeatures))]
    colnames(splitdfm) <- unlist(splitfeatures, use.names = FALSE)

    # combine dfms and suppress duplicated feature name warning
    result <- suppressWarnings(cbind(x[, -hyphenated_index], splitdfm))

    # compress features to combine same-named features
    dfm_compress(result, margin = "features")
}
