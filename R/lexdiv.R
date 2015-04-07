#' calculate lexical diversity
#' 
#' Calculate the lexical diversity or complexity of text(s).
#' @export
lexdiv <- function(x, ...) {
    UseMethod("lexdiv")
}
    
    
#' @rdname lexdiv
#' @details \code{lexdiv} calculates a variety of proposed indices for 
#'   lexical diversity. In the following formulae, \eqn{N} refers to the total 
#'   number of tokens, and \eqn{V} to the number of types: 
#'   \describe{ 
#'   
#'   \item{\code{"TTR"}:}{The ordinary \emph{Type-Token Ratio}: 
#'   \deqn{TTR = \frac{V}{N}}{TTR =  V / N}}
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
#'   Tweedie & Baayen, 1998): \deqn{U = \frac{(\log{N})^2}{\log{N} - \log{V}}}{U = 
#'   log(N)^2 / log(N) - log(V)}}
#'   
#'   \item{\code{"S"}:}{Summer's index: \deqn{S = 
#'   \frac{\log{\log{V}}}{\log{\log{N}}}}{S = log(log(V)) / log(log(N))}}
#'   
#'   \item{\code{"K"}:}{Yule's
#'   \emph{K}  (Yule, 1944, as cited in Tweedie & Baayen, 1998) is calculated 
#'   by: \deqn{K = 10^4 \times \frac{(\sum_{X=1}^{X}{{f_X}X^2}) - N}{N^2}}{K = 
#'   10^4 * (sum(fX*X^2) - N) / N^2} where \eqn{N} is the number of tokens, 
#'   \eqn{X} is a vector with the frequencies of each type, and \eqn{f_X}{fX} is
#'   the frequencies for each X.}
#'   
#'   \item{\code{"Maas"}:}{Maas' indices (\eqn{a}, \eqn{\log{V_0}} & 
#'   \eqn{\log{}_{e}{V_0}}): \deqn{a^2 = \frac{\log{N} - \log{V}}{\log{N}^2}}{a^2 = 
#'   log(N) - log(V) / log(N)^2} \deqn{\log{V_0} = \frac{\log{V}}{\sqrt{1 - 
#'   \frac{\log{V}}{\log{N}}^2}}}{log(V0) = log(V) / sqrt(1 - (log(V) / log(N)^2))} 
#'   The measure was derived from a formula by Mueller (1969, as cited
#'   in Maas, 1972). \eqn{\log{}_{e}{V_0}} is equivalent to \eqn{\log{V_0}}, only 
#'   with \eqn{e} as the base for the logarithms. Also calculated are \eqn{a}, 
#'   \eqn{\log{V_0}} (both not the same as before) and \eqn{V'} as measures of 
#'   relative vocabulary growth while the text progresses. To calculate these 
#'   measures, the first half of the text and the full text will be examined 
#'   (see Maas, 1972, p. 67 ff. for details).  Note: for the current method (for a dfm)
#'   there is no computation on separate halves of the text.}
#'   
#'   }
#'   
#' @param x a \link[=dfm]{document-feature matrix object}
#' @param measure A character vector defining the measure to calculate.
#' @param log.base A numeric value defining the base of the logarithm. 
# See \code{\link[base:log]{log}} for details.
#' @param ... additional arguments
#' @author Kenneth Benoit, adapted from the S4 class implementation written by 
#'   Meik Michalke in the \pkg{koRpus} package.
#' @note This implements only the static measures of lexical diversity, not more 
#'   complex measures based on windows of text such as the Mean Segmental Type-Token Ratio,
#'   the Moving-Average Type-Token Ratio (Covington & McFall, 2010), the MLTD or MLTD-MA 
#'   (Moving-Average Measure of Textual Lexical Diversity) proposed by McCarthy 
#'   & Jarvis (2010) or Jarvis (no year), or the HD-D version of vocd-D 
#'   (see McCarthy & Jarvis, 2007).  These are available from the package \pkg{korRpus}.
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
#'   Michalke, Meik.  (2014) \emph{koRpus: An R Package for Text Analysis}.  Version 0.05-5.
#'   \url{http://reaktanz.de/?c=hacking&s=koRpus}   
#'   
#'   Tweedie. F.J. & Baayen, R.H. (1998). How Variable May a Constant Be? 
#'   Measures of Lexical Richness in Perspective. \emph{Computers and the 
#'   Humanities}, 32(5), 323--352.
#' @return a vector of lexical diversity statistics, each corresponding to an 
#'   input document
#' @export
#' @examples
#' mydfm <- dfm(subset(inaugCorpus, Year>1980))
#' mydfmSW <- dfm(subset(inaugCorpus, Year>1980), ignoredFeatures=stopwords("english"))
#' results <- data.frame(TTR = lexdiv(mydfm, "TTR"),
#'                       CTTR = lexdiv(mydfm, "CTTR"), 
#'                       U = lexdiv(mydfm, "U"),
#'                       TTRs = lexdiv(mydfmSW, "TTR"),
#'                       CTTRs = lexdiv(mydfmSW, "CTTR"), 
#'                       Us = lexdiv(mydfmSW, "U"))
#' results
#' cor(results)
#' t(lexdiv(mydfmSW, "Maas"))
lexdiv.dfm <- function(x, measure=c("TTR", "C", "R", "CTTR", "U", "S", "Maas"), 
                           log.base=10, ...) {
    measure <- match.arg(measure)
    apply(x, 1, lexdiv.numeric, measure, log.base, ...)
}


#' @rdname lexdiv
#' @export
lexdiv.numeric <- function(x, measure=c("TTR", "C", "R", "CTTR", "U", "S", "Maas"), 
                               log.base=10, ...) {
    
    measure <- match.arg(measure)
    num.tokens <- sum(x)
    num.types <- sum(x>0)
    
    ## calculate TTR
    if (measure=="TTR") 
        return(ttr.calc(num.tokens=num.tokens, num.types=num.types, type="TTR"))

    ## calculate Herdan's C: log(types) / log(tokens)
    if (measure=="C") 
        return(ttr.calc(num.tokens=num.tokens, num.types=num.types, type="C", log.base=log.base))
    
    ## calculate Guiraud's R: types / sqrt(tokens)
    if (measure=="R")
        return(ttr.calc(num.tokens=num.tokens, num.types=num.types, type="R"))
    
    ## calculate Carroll's CTTR: types / 2*sqrt(tokens)
    if (measure=="CTTR")
        return(ttr.calc(num.tokens=num.tokens, num.types=num.types, type="CTTR"))
    
    ## calculate Uber Index U: (log(tokens))^2 / (log(tokens) - log(types))
    if (measure=="U") 
        return(ttr.calc(num.tokens=num.tokens, num.types=num.types, type="U", log.base=log.base))
    
    ## calculate Summer's S: LogLog(types) / LogLog(tokens)
    if (measure=="S") 
        return(ttr.calc(num.tokens=num.tokens, num.types=num.types, type="S", log.base=log.base))
    
    ## calculate Maas' a^2 and lgV0 indices
    if (measure=="Maas") 
        return(c(Maas=ttr.calc(num.tokens=num.tokens, num.types=num.types, type="Maas", log.base=log.base),
                 lgV0=lgV0.calc(num.tokens=num.tokens, num.types=num.types, x=0, log.base=10),
                 lgeV0=lgV0.calc(num.tokens=num.tokens, num.types=num.types, x=0, log.base=exp(1))))
}


## function ttr.calc()
# this helper function will be used for nearly all TTR calculations
ttr.calc <- function(txt.tokens=NULL, txt.types=NULL, 
                     num.tokens=NULL, num.types=NULL, type="TTR", log.base=10) {
  if(is.null(c(txt.tokens, txt.types, num.tokens, num.types))) {
    stop(simpleError("Internal function ttr.calc() called without any data!"))
  } else {
    if(is.null(num.tokens)){
      stopifnot(!is.null(txt.tokens))
      num.tokens <-  length(txt.tokens)
    } else {}
    if(is.null(num.types)){
      if(is.null(txt.types)){
        stopifnot(!is.null(txt.tokens))
        txt.types <- unique(txt.tokens)
      } else {}
      num.types <- length(txt.types)
    } else {}
  }
  if(identical(type, "TTR")){
    result <- num.types / num.tokens
  } else {}
  if(identical(type, "C")){
    result <- log(num.types, base=log.base) / log(num.tokens, base=log.base)
  } else {}
  if(identical(type, "R")){
    result <- num.types / sqrt(num.tokens)
  } else {}
  if(identical(type, "CTTR")){
    result <- num.types / sqrt(2 * num.tokens)
  } else {}
  if(identical(type, "U")){
    result <- log(num.tokens, base=log.base)^2 / (log(num.tokens, base=log.base) - log(num.types, base=log.base))
  } else {}
  if(identical(type, "S")){
    result <-  log(log(num.types, base=log.base), base=log.base) / log(log(num.tokens, base=log.base), base=log.base)
  } else {}
  if(identical(type, "Maas")){
    result <- sqrt((log(num.tokens, base=log.base) - log(num.types, base=log.base)) / log(num.tokens, base=log.base)^2)
  } else {}
  return(result)
} ## end function ttr.calc()

## function lgV0.calc()
# function to calculate Maas' lgV0 index
## TODO: estimate geschwurbel index x for correction
lgV0.calc <- function(txt.tokens=NULL, txt.types=NULL, 
                      num.tokens=NULL, num.types=NULL, x=0, log.base=10) {
    if (is.null(c(txt.tokens, txt.types, num.tokens, num.types))) {
        stop(simpleError("Internal function lgV0.calc() called without any data!"))
    } else {
        if (is.null(num.tokens)) {
            stopifnot(!is.null(txt.tokens))
            num.tokens <- length(txt.tokens)
        } else {}
        if (is.null(num.types)) {
            if (is.null(txt.types)) {
                stopifnot(!is.null(txt.tokens))
                txt.types <- unique(txt.tokens)
            } else {}
            num.types <- length(txt.types)
        } else {}
    }
    maas.index <- log(num.types, base=log.base) / sqrt(1 - (log(num.types, base=log.base) / (log(num.tokens, base=log.base) + x))^2)
    return(maas.index)
} 
