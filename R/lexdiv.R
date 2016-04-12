#' calculate lexical diversity
#' 
#' Calculate the lexical diversity or complexity of text(s).
#' @export
lexdiv <- function(x, ...) {
    UseMethod("lexdiv")
}
    
    
#' @rdname lexdiv
#' @details \code{lexdiv} calculates a variety of proposed indices for lexical
#'   diversity. In the following formulae, \eqn{N} refers to the total number of
#'   tokens, and \eqn{V} to the number of types: \describe{
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
#'   \item{\code{"K"}:}{Yule's \emph{K}  (Yule, 1944, as cited in Tweedie &
#'   Baayen, 1998) is calculated by: \deqn{K = 10^4 \times
#'   \frac{(\sum_{X=1}^{X}{{f_X}X^2}) - N}{N^2}}{K = 10^4 * (sum(fX*X^2) - N) /
#'   N^2} where \eqn{N} is the number of tokens, \eqn{X} is a vector with the
#'   frequencies of each type, and \eqn{f_X}{fX} is the frequencies for each X.}
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
#' @param x an input object, such as a \link[=dfm]{document-feature matrix}
#'   object
#' @param measure a character vector defining the measure to calculate.
#' @param log.base a numeric value defining the base of the logarithm (for
#'   measures using logs)
#' @param drop if \code{TRUE}, the result is returned as a numeric vector if 
#'   only a single measure is requested; otherwise, a data.frame is returned 
#'   with each column consisting of a requested measure.
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
#' @references Covington, M.A. & McFall, J.D. (2010). Cutting the Gordian Knot: 
#'   The Moving-Average Type-Token Ratio (MATTR). \emph{Journal of Quantitative 
#'   Linguistics}, 17(2), 94--100.
#'   
#'   Maas, H.-D., (1972). \"Uber den Zusammenhang zwischen Wortschatzumfang und 
#'   L\"ange eines Textes. \emph{Zeitschrift f\"ur Literaturwissenschaft und 
#'   Linguistik}, 2(8), 73--96.
#'   
#'   McCarthy, P.M. & Jarvis, S. (2007). vocd: A theoretical and empirical 
#'   evaluation. \emph{Language Testing}, 24(4), 459--488.
#'   
#'   McCarthy, P.M. & Jarvis, S. (2010). MTLD, vocd-D, and HD-D: A validation 
#'   study of sophisticated approaces to lexical diversity assessment. 
#'   \emph{Behaviour Research Methods}, 42(2), 381--392.
#'   
#'   Michalke, Meik.  (2014) \emph{koRpus: An R Package for Text Analysis}. 
#'   Version 0.05-5. \url{http://reaktanz.de/?c=hacking&s=koRpus}
#'   
#'   Tweedie. F.J. & Baayen, R.H. (1998). How Variable May a Constant Be? 
#'   Measures of Lexical Richness in Perspective. \emph{Computers and the 
#'   Humanities}, 32(5), 323--352.
#' @return a data.frame or vector of lexical diversity statistics, each row or
#'   vector element corresponding to an input document
#' @export
#' @examples
#' mydfm <- dfm(subset(inaugCorpus, Year > 1980), verbose = FALSE)
#' (results <- lexdiv(mydfm, c("CTTR", "TTR", "U")))
#' cor(lexdiv(mydfm, "all"))
#' 
#' # with different settings of drop
#' lexdiv(mydfm, "TTR", drop = TRUE)
#' lexdiv(mydfm, "TTR", drop = FALSE)
lexdiv.dfm <- function(x, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "Maas"), 
                           log.base = 10, drop = TRUE, ...) {

    addedArgs <- names(list(...))
    if (length(addedArgs))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), addedArgs, " not used.", sep = "", noBreaks. = TRUE)

    TTR <- C <- R <- CTTR <- U <- S <- Maas <- lgV0 <- lgeV0 <- NULL
    
    
    # check that all measures are legal values
    validMeasures <- c("all", "TTR", "C", "R", "CTTR", "U", "S", "Maas")
    checkMeasure <- measure %in% validMeasures
    if (!all(checkMeasure))
        stop("Invalid measure(s): ", measure[!checkMeasure])
    if ("all" %in% measure) 
        measure <- validMeasures[-1]

    # nTokens <- ntoken(x)
    # nTypes <- ntype(x)
    
    # set up the data.table for results
    results <- data.table(textID = docnames(x), nTokens <- ntoken(x), nTypes <- ntype(x))
                               
    if ("TTR" %in% measure) {
        results[, TTR := nTypes / nTokens]
    }
    
    if ("C" %in% measure) {
        results[, C := log(nTypes, base = log.base) / log(nTokens, base = log.base)]
    }

    if ("R" %in% measure) {
        results[, R := nTypes / sqrt(nTokens)]
    }
    
    if ("CTTR" %in% measure) {
        results[, CTTR := nTypes / sqrt(2 * nTokens)]
    }

    if ("U" %in% measure) 
        results[, U := log(nTokens, base = log.base)^2 / (log(nTokens, base = log.base) - log(nTypes, base = log.base))]

    if ("S" %in% measure) 
        results[, S := log(log(nTypes, base = log.base), base = log.base) / 
                       log(log(nTokens, base = log.base), base = log.base)]
    
    if ("Maas" %in% measure) {
        measure <- c(measure, "lgV0", "lgeV0")
        results[, Maas := sqrt((log(nTokens, base = log.base) - log(nTypes, base = log.base)) / 
                               log(nTokens, base = log.base)^2)]
        results[, lgV0 := log10(nTypes) / sqrt(1 - (log10(nTypes) / (log10(nTokens) + 0))^2)]
        results[, lgeV0 := log(nTypes) / sqrt(1 - (log(nTypes) / (log(nTokens) + 0))^2)]
    }

    # return a data.frame of the measures
    results <- as.data.frame(results)
    results <- results[, measure, drop = drop]
    if (!is.vector(results) & !("all" %in% measure)) {
        row.names(results) <- docnames(x)
    } else {
        names(results) <- docnames(x)
    }
    results
}

