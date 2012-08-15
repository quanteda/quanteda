\name{translate}
\alias{translate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
translate(sourceText, sourceLanguage, targetLanguage, key = NULL, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sourceText}{
%%     ~~Describe \code{sourceText} here~~
}
  \item{sourceLanguage}{
%%     ~~Describe \code{sourceLanguage} here~~
}
  \item{targetLanguage}{
%%     ~~Describe \code{targetLanguage} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sourceText, sourceLanguage, targetLanguage, key = NULL, 
    verbose = FALSE) 
{
    a <- strsplit(sourceText, split = "[\\.]")
    sentences <- unlist(a)
    chunk <- ""
    translatedText <- ""
    for (i in 1:length(sentences)) {
        s <- sentences[i]
        if (nchar(s) < 2) {
            if (verbose) 
                print("empty sentence")
            next
        }
        s <- curlEscape(s)
        if (nchar(s) >= 1000) {
            if (verbose) 
                print("in the 1000 case")
            start <- 1
            end <- 1000
            while ((nchar(s) - start) > 1000) {
                chunk <- substr(s, start, end)
                translatedText <- paste(translatedText, translateChunk(chunk, 
                  sourceLanguage, targetLanguage), sep = ". ")
                start <- start + 1000
                end <- end + 1000
            }
            chunk <- substr(s, start, nchar(s))
            translatedText <- paste(translatedText, translateChunk(chunk, 
                sourceLanguage, targetLanguage), sep = "")
            chunk <- ""
        }
        else {
            if (i == length(sentences)) {
                if (verbose) 
                  print("one")
                if (nchar(chunk) > 5) {
                  translatedText <- paste(translatedText, translateChunk(chunk, 
                    sourceLanguage, targetLanguage), sep = ". ")
                }
                else {
                  if (verbose) 
                    print("empty chunk")
                }
                if (nchar(s) > 5) {
                  translatedText <- paste(translatedText, translateChunk(s, 
                    sourceLanguage, targetLanguage), sep = ". ")
                }
                else {
                  if (verbose) 
                    print("empty sentence")
                }
                chunk <- ""
            }
            else if ((nchar(chunk) + nchar(s) >= 1000)) {
                if (verbose) 
                  print("two")
                translatedText <- paste(translatedText, translateChunk(chunk, 
                  sourceLanguage, targetLanguage), sep = ". ")
                chunk <- paste(s, ".\%20", sep = "")
            }
            else {
                if (verbose) 
                  print("three")
                chunk <- paste(chunk, s, sep = ".\%20")
            }
        }
    }
    translatedText <- curlUnescape(translatedText)
    if (verbose) 
        cat("****************", translatedText, "********************", 
            nchar(translatedText), "\n")
    if (verbose) 
        cat("\n")
    return(translatedText)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
