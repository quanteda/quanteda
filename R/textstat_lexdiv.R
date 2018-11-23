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
#' @param x an input \link{dfm} or \link{tokens} input object
#' @param measure a character vector defining the measure to calculate
#' @param log.base a numeric value defining the base of the logarithm (for
#'   measures using logs)
#' @param remove_numbers logical; if \code{TRUE} remove features or tokens that
#'   consist only of numerals (the Unicode "Number" [N] class)
#' @param remove_punct logical; if \code{TRUE} remove all features or tokens
#'   that consist only of the Unicode "Punctuation" [P] class)
#' @param remove_symbols logical; if \code{TRUE} remove all features or tokens
#'   that consist only of the Unicode "Punctuation" [S] class)
#' @param remove_hyphens logical; if \code{TRUE} split words that are connected
#'   by hyphenation and hyphenation-like characters in between words, e.g.
#'   "self-storage" becomes two features or tokens "self" and "storage". Default
#'   is FALSE to preserve such words as is, with the hyphens.
#' @author Kenneth Benoit and Jiang Wei Lua.  Many of the formulas have been
#'   reimplemented from functions written by Meik Michalke in the \pkg{koRpus}
#'   package.
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
textstat_lexdiv.dfm <- 
    function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas"), 
             log.base = 10, remove_numbers = TRUE, remove_punct = TRUE,
             remove_symbols = TRUE, remove_hyphens = FALSE) {
        
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    if (remove_hyphens) 
        x <- dfm_split_hyphenated_features(x)
    if (remove_numbers) 
        x <- dfm_remove(x, "^\\p{N}+$", valuetype = "regex")
    if (remove_punct) 
        x <- dfm_remove(x, "^\\p{P}+$", valuetype = "regex")
    if (remove_symbols) 
        x <- dfm_remove(x, "^\\p{S}+$", valuetype = "regex")
    
    if (!sum(x)) 
        stop(message_error("dfm_empty after removal of numbers, symbols, punctuations, hyphens"))

    measure_option <- c("TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas")
    if (measure[1] == "all") {
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
textstat_lexdiv.tokens <- 
    function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "K", "D", "Vm", "Maas"),
             log.base = 10, remove_numbers = TRUE, remove_punct = TRUE,
             remove_symbols = TRUE, remove_hyphens = FALSE) {
        
    if (remove_hyphens)
        x <- tokens(x, remove_hyphens = TRUE)
    if (remove_numbers) 
        x <- tokens(x, remove_numbers = TRUE)
    if (remove_symbols)
        x <- tokens(x, remove_symbols = TRUE)
    if (remove_punct) {
        # this will be replaced with 
        # x <- tokens(x, remove_punct = TRUE)
        # when we resolve #1445
        x <- tokens_remove(x, "^\\p{P}+$", valuetype = "regex")
    }

    textstat_lexdiv.dfm(dfm(x), measure = measure, log.base = log.base)
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

#' Split a dfm's hyphenated features into constituent parts
#' 
#' Takes a dfm that contains features with hyphenated words, such as
#' "split-second" and turns them into features that split the elements
#' in the same was as `tokens(x, remove_hyphens = TRUE)` would have done.
#' @param x input \link{dfm}
#' @keywords internal dfm
#' @examples
#' (tmp <- dfm("One-two one two three."))
#' quanteda:::dfm_split_hyphenated_features(tmp)
dfm_split_hyphenated_features <- function(x) {
    # the global for matching the hyphens and similar characters
    hyphen_regex <- "^.+\\p{Pd}.+$"
    
    # figure out where the hyphens are
    hyphenated_index <- which(stringi::stri_detect_regex(featnames(x), hyphen_regex))
    
    # return dfm unmodified if no hyphenated features are found
    if (length(hyphenated_index) == 0) return(x)
    
    # split the hyphenated feature names into a list of components
    splitfeatures <- as.list(tokens(featnames(x)[hyphenated_index], remove_hyphens = TRUE))
    
    # efficiently create a new dfm from hyphenated feature name components
    splitdfm <- x[, rep(hyphenated_index, times = lengths(splitfeatures))]
    colnames(splitdfm) <- unlist(splitfeatures, use.names = FALSE)
    
    # combine dfms and suppress duplicated feature name warning
    result <- suppressWarnings(cbind(x[, -hyphenated_index], splitdfm))
    # compress features to combine same-named features
    result <- dfm_compress(result, margin = "features")

    result
} 

#' Computes the Moving-Average Type-Token Ratio (Covington & McFall, 2010) for a Tokens Object
#' 
#' Takes a tokens object which should have been preprocessed - removal of punctuation etc.
#' @param x input \link{tokens}
#' @param window_size input: the size of the moving window for computation of TTR
#' @param all_windows default = FALSE. If TRUE, returns a vector with the MATTR for each window
#' @param mean_mattr default = TRUE. Returns the mean MATTR, given the window size, for the tokens object. 
#' @return return either a vector with the MATTR for each window, or the mean MATTR for the whole tokens object
#' @keywords internal tokens

compute_mattr <- function(x, window_size = NULL, all_windows = FALSE, mean_mattr= TRUE){
    # Error Checks
    if (!is.tokens(x)) stop("x must be a tokens object")
    if (is.null(window_size)) stop('window_size must be specified')
    # Get number of tokens across all documents
    num_tokens <- sum(ntoken(x))
    if (window_size > num_tokens) stop('window_size must be smaller than total ntokens across all documents')
    if ((all_windows == FALSE) && (mean_mattr == FALSE)) stop('at least one MATTR value type to be returned')
    
    # List to Store MATTR Values for each Window 
    mattr_list <- list()
    
    # Initializers
    start = 1
    end = window_size 
    
    all_tokens <- unlist(x)
    temp_ls <- all_tokens[start:end]
    
    while (end <= num_tokens){
        # Each MATTR value is named with the start token number and end token number
        window_name <- paste0('MATTR_tokens',start, '_',end)
        temp_toks <- tokens(paste(unlist(temp_ls), collapse = ' '))
        typecount <- ntype(temp_toks)[[1]]
        tokcount <- ntoken(temp_toks)[[1]]
        mattr_list[[window_name]] <- typecount/tokcount
        start = start + 1
        end = end + 1
        overlap <- temp_ls[2:length(temp_ls)]
        if (end <= num_tokens) {temp_ls <- c(overlap, all_tokens[end])}
    }
    
    mattr_list <- unlist(mattr_list)
    ## Checks
    if (length(mattr_list) != (num_tokens - window_size + 1)) {
        stop('Internal error within compute_mattr')}
    else {
        if ((all_windows == FALSE) && (mean_mattr == TRUE)) return(mean(mattr_list))
        if ((all_windows == TRUE) && (mean_mattr == FALSE)) return(mattr_list)
        if ((all_windows == TRUE) && (mean_mattr == TRUE)) return(mean(mattr_list), mattr_list)
    }
}



#' Computes the Mean Segmental Type-Token Ratio (Johnson, 1944) for a Tokens Object
#' 
#' Takes a tokens object which should have been preprocessed - removal of punctuation etc.
#' @param x input \link{tokens}
#' @param segment_size input: the size of the segment. 
#' @param discard_remainder default = TRUE. If TRUE, the final segment of the document not divisible by segment_size is dropped. 
#' @param all_segments default = FALSE. If TRUE, returns a vector with the TTR for each segment. 
#' @param mean_sttr default = TRUE. Returns the Mean TTR across Segments for the tokens object. 
#' @return returns a vector with the MSSTR for each segment
#' @keywords internal tokens


compute_msttr <- function(x, segment_size = NULL, discard_remainder = TRUE, all_segments = FALSE, mean_sttr = TRUE){
    # Error Checks
    if (!is.tokens(x)) stop("x must be a tokens object")
    if (is.null(segment_size)) stop('segment_size must be specified')
    if ((all_segments == FALSE) && (mean_sttr == FALSE)) stop('at least one MSTTR value type to be returned')
    
    
    # Get number of tokens across all documents
    num_tokens <- sum(ntoken(x))
    if (segment_size > num_tokens) stop('window_size must be smaller than total ntokens across all documents')
    
    # Checks for divisibility of the tokens object by segment_size
    remainder = num_tokens %% segment_size
    n_segments = num_tokens %/% segment_size
    # Warning raiser if not perfectly divisible
    if (remainder != 0) warning(paste('ntokens =', num_tokens, 'not perfectly divisible by segment_size.', 
                                      n_segments, 'segments of size', segment_size, 
                                      'and last segment of size', remainder))
    
    # Warning of discard of remainder
    if (discard_remainder == TRUE) warning(paste('Last segment of size ', remainder, 'will be discarded'))
    
    # List to Store MSTTR Values for each Window 
    msttr_list <- list()
    
    # Initializers
    start = 1
    end = segment_size 
    
    all_tokens <- unlist(x)
    temp_ls <- all_tokens[start:end]
    
    while (start <= num_tokens){
        # Each MSSTR segment is named with the start token number and end token number
        segment_name <- paste0('MSTTR_tokens',start, '_',end)
        temp_toks <- tokens(paste(unlist(temp_ls), collapse = ' '))
        typecount <- ntype(temp_toks)[[1]]
        tokcount <- ntoken(temp_toks)[[1]]
        msttr_list[[segment_name]] <- typecount/tokcount
        start = start + segment_size
        end = end + segment_size
        if (end <= num_tokens) {temp_ls <- all_tokens[start:end]}
        else {
            if ((start < num_tokens) && (end > num_tokens)) {
                if (discard_remainder == FALSE){
                    end = num_tokens
                    temp_ls <- all_tokens[start:end]
                } else if (discard_remainder == TRUE){
                    break
                }
            }
        }
    }
    
    msttr_list <- unlist(msttr_list)
    
    
    if ((remainder ==0) & (length(msttr_list) != n_segments)){
        stop('Internal error within compute_msttr')}
    
    
    if ((remainder != 0)  & (discard_remainder == TRUE) & length(msttr_list) != (n_segments)) {
        stop('Internal error within compute_msttr')}
    
    if ((remainder != 0)  & (discard_remainder == FALSE) & length(msttr_list) != (n_segments + 1)) {
        stop('Internal error within compute_msttr')}
    
    if ((all_segments == FALSE) && (mean_sttr == TRUE)) return(mean(msttr_list))
    if ((all_segments == TRUE) && (mean_sttr == FALSE)) return(msttr_list)
    if ((all_segments == TRUE) && (mean_sttr == TRUE)) return(mean(mattr), msttr_list)
    
}



#' Computes the Measure of Textual Lexical Diversity (MTLD) (McCarthy & Jarvis, 2010) for a Tokens Object
#' MTLD is defined as the mean length of sequential word strings in a text that maintain a given TTR value
#' Takes a tokens object which should have been preprocessed - removal of punctuation etc.
#' Support for partial factors not yet implemented
#' @param x input \link{tokens}
#' @param ttr_threshold the threshold below which TTR for a sequential string of words cannot fall. 
#' If TTR falls below this value, the factor count increases by a value of 1, and the TTR evaluations are reset.
#' @return Average of MTLD (Forward Pass and Backward Pass)
#' @keywords internal tokens


compute_mtld <- function(x, ttr_threshold = 0.720){
    # Error Checks 
    if (!is.tokens(x)) stop("x must be a tokens object")
    if ((ttr_threshold > 1) | (ttr_threshold < 0)) stop("TTR threshold must be between 0 and 1")
    if (is.null(ttr_threshold)) stop('TTR threshold cannot be NULL')
    
    # Internal Function to compute MTLD for both forward and backward passes
    one_pass <- function(x, backward = FALSE){
        # Initialize Counters
        start = 1
        counter = 1
        end = sum(ntoken(x))
        factor = 1
        
        list_of_tokens <- unlist(x)
        
        if (backward == TRUE){list_of_tokens <- rev(list_of_tokens)}
        
        temp_ls <- list_of_tokens[start:counter]
        
        while (counter <= end){
            print(paste(start, counter, temp_ls))
            temp_toks <- tokens(paste(unlist(temp_ls), collapse = ' '))
            typecount <- ntype(temp_toks)[[1]]
            tokcount <- ntoken(temp_toks)[[1]]
            temp_TTR <- typecount/tokcount
            if (temp_TTR <= ttr_threshold){
                factor = factor + 1
                counter = counter + 1
                start = counter
                temp_ls <- list_of_tokens[start:counter]
            } else if (temp_TTR > ttr_threshold){
                counter = counter + 1
                if (counter <= end){
                    temp_ls <- c(temp_ls, list_of_tokens[counter])
                } else {
                    break
                }
            }
        }
        mtld_value = (end / factor)
        return(mtld_value)
    }
    
    mtld_forward_pass <- one_pass(x)
    mtld_backward_pass <- one_pass(x, backward = TRUE)
    mtld_average <- ((mtld_forward_pass + mtld_backward_pass) /2)
    return (mtld_average)
}






