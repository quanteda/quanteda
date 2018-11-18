#' Calculate lexical diversity
#' 
#' Calculate the lexical diversity or complexity of text(s).
#' @details \code{textstat_lexdiv} calculates a variety of proposed indices for
#'   lexical diversity. In the following formulas, \eqn{N} refers to the total
#'   number of tokens, \eqn{V} to the number of types, and \eqn{f_v(i, N)} to the
#'   numbers of types occurring \eqn{i} times in a sample of length \eqn{N}.
#'   \describe{
#'      
#'   \item{\code{"TTR"}:}{The ordinary \emph{Type-Token Ratio}: \deqn{TTR =
#'   \frac{V}{N}}{TTR =  V / N}}
#'   
#'   \item{\code{"C"}:}{Herdan's \emph{C} (Herdan, 1960, as cited in Tweedie & 
#'   Baayen, 1998; sometimes referred to as \emph{LogTTR}): \deqn{C = 
#'   \frac{\log{V}}{\log{N}}}{C = log(V) / log(N)}}
#'   
#'   \item{\code{"R"}:}{Guiraud's \emph{Root TTR} (Guiraud, 1954, as cited in 
#'   Tweedie & Baayen, 1998): \deqn{R = \frac{V}{\sqrt{N}}}{R = V / sqrt(N)}}
#'   
#'   \item{\code{"CTTR"}:}{Carroll's \emph{Corrected TTR}: \deqn{CTTR = 
#'   \frac{V}{\sqrt{2N}}}{CTTR = V / sqrt(2N)}}
#'   
#'   \item{\code{"U"}:}{Dugast's \emph{Uber Index}  (Dugast, 1978, as cited in 
#'   Tweedie & Baayen, 1998): \deqn{U = \frac{(\log{N})^2}{\log{N} - \log{V}}}{U
#'   = log(N)^2 / log(N) - log(V)}}
#'   
#'   \item{\code{"S"}:}{Summer's index: \deqn{S = 
#'   \frac{\log{\log{V}}}{\log{\log{N}}}}{S = log(log(V)) / log(log(N))}}
#'   
#'   \item{\code{"K"}:}{Yule's \emph{K}  (Yule, 1944, as presented in Tweedie &
#'   Baayen, 1998, Eq. 16) is calculated by: \deqn{K = 10^4 \times
#'   \left[ -\frac{1}{N} + \sum_{i=1}^{V} f_v(i, N) \left( \frac{i}{N} \right)^2 \right] }}
#'   
#'   \item{\code{"D"}:}{Simpson's \emph{D}  (Simpson 1949, as presented in
#'   Tweedie & Baayen, 1998, Eq. 17) is calculated by:
#'   \deqn{D = \sum_{i=1}^{V} f_v(i, N) \frac{i}{N} \frac{i-1}{N-1}}}
#'
#'   \item{\code{"Vm"}:}{Herdan's \eqn{V_m}  (Herdan 1955, as presented in
#'   Tweedie & Baayen, 1998, Eq. 18) is calculated by:
#'   \deqn{V_m = \sqrt{ \sum_{i=1}^{V} f_v(i, N) (i/N)^2 - \frac{i}{V} }}}
#'
#'   \item{\code{"Maas"}:}{Maas' indices (\eqn{a}, \eqn{\log{V_0}} & 
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
#'   }
#'   
#' @param x an input \link{dfm} or \link{tokens}
#' @param measure a character vector defining the measure to calculate.
#' @param log.base a numeric value defining the base of the logarithm (for
#'   measures using logs)
#' @param remove_numbers default = TRUE. removes regex "\\p{N}"
#' @param remove_punct default = TRUE. removes regex "\\p{P}"
#' @param remove_symbols default = TRUE. removes regex "\\p{S}"
#' @param remove_hyphens default = FALSE. splits hyphenated tokens by the hyphen and produces two tokens.
#' @param ... not used
#' @author Kenneth Benoit, adapted from the S4 class implementation written by 
#'   Meik Michalke in the \pkg{koRpus} package.
#' @note This implements only the static measures of lexical diversity, not more
#'   complex measures based on windows of text such as the Mean Segmental
#'   Type-Token Ratio, the Moving-Average Type-Token Ratio (Covington & McFall,
#'   2010), the MLTD or MLTD-MA (Moving-Average Measure of Textual Lexical
#'   Diversity) proposed by McCarthy & Jarvis (2010) or Jarvis (no year), or the
#'   HD-D version of vocd-D (see McCarthy & Jarvis, 2007).  These are available
#'   from the package \pkg{korRpus}.
#' @references 
#'   Covington, M.A. & McFall, J.D. (2010). "Cutting the Gordian Knot: 
#'   The Moving-Average Type-Token Ratio (MATTR)". \emph{Journal of Quantitative 
#'   Linguistics} 17(2), 94--100.
#'   
#'   Herdan, Gustav. 1955. "A New Derivation and Interpretation of Yule's
#'   'Characteristic' \emph{K}." \emph{Zeitschrift für angewandte Mathematik und
#'   Physik} 6(4): 332--34.
#'   
#'   Maas, H.-D., (1972). "\"Uber den Zusammenhang zwischen Wortschatzumfang und 
#'   L\"ange eines Textes". \emph{Zeitschrift f\"ur Literaturwissenschaft und 
#'   Linguistik} 2(8), 73--96.
#'   
#'   McCarthy, P.M. & Jarvis, S. (2007). "vocd: A theoretical and empirical 
#'   evaluation". \emph{Language Testing} 24(4), 459--488.
#'   
#'   McCarthy, P.M. & Jarvis, S. (2010). "MTLD, vocd-D, and HD-D: A validation 
#'   study of sophisticated approaches to lexical diversity assessment". 
#'   \emph{Behaviour Research Methods} 42(2), 381--392.
#'   
#'   Michalke, Meik.  (2014) \emph{koRpus: An R Package for Text Analysis}. 
#'   Version 0.05-5. \url{http://reaktanz.de/?c=hacking&s=koRpus}
#'   
#'   Simpson, Edward H. 1949. "Measurement of Diversity." \emph{Nature} 163: 688.
#'   
#'   Tweedie. F.J. & Baayen, R.H. (1998). "How Variable May a Constant Be? 
#'   Measures of Lexical Richness in Perspective". \emph{Computers and the 
#'   Humanities} 32(5), 323--352.
#'   
#' @return \code{textstat_lexdiv} returns a data.frame of documents and
#'   their lexical diversity scores.
#' @export
#' @examples
#' mydfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), verbose = FALSE)
#' (result <- textstat_lexdiv(mydfm, c("CTTR", "TTR", "U")))
#' cor(textstat_lexdiv(mydfm, "all")[,-1])
#' 
textstat_lexdiv <- function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas"), 
                            log.base = 10, remove_numbers = TRUE, remove_punct = TRUE,
                            remove_symbols = TRUE, remove_hyphens = FALSE) {
    UseMethod("textstat_lexdiv")
}


#' @export
textstat_lexdiv.default <- function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas"), 
                                log.base = 10, remove_numbers = TRUE, remove_punct = TRUE,
                                remove_symbols = TRUE, remove_hyphens = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textstat_lexdiv"))
}

#' @export
textstat_lexdiv.dfm <- function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas"), 
                                log.base = 10, remove_numbers = TRUE, remove_punct = TRUE,
                                remove_symbols = TRUE, remove_hyphens = FALSE) {
    
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    if (remove_numbers) {x <- dfm_remove(x, "\\p{N}", valuetype = "regex")}
    if (remove_punct) {x <- dfm_remove(x, "\\p{Po}", valuetype = "regex")}
    if (remove_symbols) {x <- dfm_remove(x, "\\p{S}", valuetype = "regex")}
    if (remove_hyphens){
        features <- featnames(x)
        features_with_hyphens <- features[stringi::stri_detect_fixed(features, '-')]
        dfm_nohyphen <- dfm_select(x, features_with_hyphens, selection ="remove")
        # Create separate dataframe for hyphenated features
        temp <- dfm_select(x, features_with_hyphens)
        temp <- convert(temp, to = 'data.frame')
        for (i in features_with_hyphens){
          split = stri_split_fixed(i, '-', tokens_only=TRUE) 
          temp[split[[1]]] <- temp[i]
        }
        
        dfm_hyphen <- as.dfm(temp) 
        # WARNING: Coercion creates unwanted column "document" and docnames are not updated properly
        # Remove hyphenated features
        dfm_hyphen_removed <- dfm_hyphen %>% 
          dfm_remove('document') %>% 
          dfm_remove(features_with_hyphens)
        
        # Add checks to deal with case when hyphenated words contain duplicated tokens in dfm_nohyphen
        overlap_condition = (featnames(dfm_hyphen_removed) %in% featnames(dfm_nohyphen))
        if (any(overlap_condition) == TRUE) {
            overlap = featnames(dfm_hyphen_removed)[overlap_condition]
            df_keep_overlap <- convert(dfm_hyphen_removed, to = 'data.frame')
            df_drop_overlap <- convert(dfm_nohyphen, to = 'data.frame')
            df_keep_overlap[overlap] = df_keep_overlap[overlap] + df_drop_overlap[overlap]
            df_drop_overlap = df_drop_overlap[,!names(df_drop_overlap) %in% overlap]
            dfm_recombine <- cbind(as.dfm(df_keep_overlap) %>% dfm_remove('document'), 
                                   as.dfm(df_drop_overlap) %>% dfm_remove('document'))
            docnames(dfm_recombine) <- docnames(x)
            x <- dfm_recombine
          
        } else {
          #Update docnames
          docnames(dfm_hyphen_removed) <- docnames(x)
          x <- cbind(dfm_nohyphen, dfm_hyphen_removed)
        }
        
    }
    
    if (!sum(x)) stop(message_error("dfm_empty after removal of numbers, symbols, punctuations, hyphens"))
    
    
    measure_option <- c("TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas")
    if (measure[1] == 'all') {
        measure <- measure_option
    } else {
        is_valid <- measure %in% measure_option
        if (!all(is_valid))
            stop("Invalid measure(s): ", measure[!is_valid])
    }
    
    result <- compute_lexdiv_stats(x, measure, log.base)
    return(result)
}

#' @export
textstat_lexdiv.tokens <- function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas"),
                                   log.base = 10, remove_numbers = TRUE, remove_punct = TRUE,
                                   remove_symbols = TRUE, remove_hyphens = FALSE) {
  
    # Check if input is a token and coerce. This would support lists
    if (!(is.tokens(x))) {x <- as.tokens(x)}
  
    # Convert x into dfm to reuse computation for K, D, Vm
    if (is.tokens(x)) {
        if (remove_numbers) {x <- tokens_remove(x, "\\p{N}", valuetype = "regex")}
        if (remove_punct) {x <- tokens_remove(x, "\\p{Po}", valuetype = "regex")}
        if (remove_symbols) {x <- tokens_remove(x, "\\p{S}", valuetype = "regex")}
    }
    
    result <- dfm(x) %>% textstat_lexdiv.dfm(measure = measure, log.base = log.base)
  
    return(result)
  
}

#' Compute lexdiv (internal functions)
#' 
#' Internal function used in textstat_lexdiv 
#' @name lexdiv
#' @param x a \link{dfm} object
#' @param measure a list of lexical diversity measures.
#' @param log.base a numeric value defining the base of the logarithm (for
#'   measures using logs)
#' @return returns a data.frame of documents and their lexical diversity scores.

compute_lexdiv_stats <- function(x, measure, log.base){
    
    n_tokens <- n_types <- TTR <- C <- R <- CTTR <- U <- S <- Maas <- lgV0 <- lgeV0 <- K <- D <- Vm <- NULL
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
  
    # computations for K, D, Vm
    # produces a list of data.frames that will be used for computing the measures
    if (length(intersect(c("K", "D", "Vm"), measure))) {
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
        temp[, K := 10^4 * vapply(ViN, function(y) sum(y$ViN * (y$i / y$n_tokens)^2), numeric(1))]
    
    if ("D" %in% measure)
        temp[, D := vapply(ViN, 
                           function(y) sum(y$ViN * (y$i / y$n_tokens) * ((y$i - 1) / (y$n_tokens - 1))), 
                           numeric(1))]
    
    if ("Vm" %in% measure) 
        temp[, Vm := vapply(ViN, 
                            function(y) sqrt( sum(y$ViN * (y$i / y$n_tokens)^2) - 1 / y$n_types[1] ),
                            numeric(1))]
    
    if ("Maas" %in% measure) {
        measure <- c(measure, "lgV0", "lgeV0")
        temp[, Maas := sqrt((log(n_tokens, base = log.base) - log(n_types, base = log.base)) / 
                             log(n_tokens, base = log.base) ^ 2)]
        temp[, lgV0 := log10(n_types) / sqrt(1 - (log10(n_types) / (log10(n_tokens) + 0)) ^ 2)]
        temp[, lgeV0 := log(n_types) / sqrt(1 - (log(n_types) / (log(n_tokens) + 0)) ^ 2)]
    }
    
    result <- data.frame(document = docnames(x), stringsAsFactors = FALSE)
    result <- cbind(result, as.data.frame(temp[,measure, with = FALSE]))
    class(result) <- c("lexdiv", "textstat", "data.frame")
    rownames(result) <- as.character(seq_len(nrow(result)))
    return(result)
}


