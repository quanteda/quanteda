\name{translateChunk}
\alias{translateChunk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
translateChunk(sourceText, sourceLanguage, targetLanguage, key = NULL, verbose = TRUE)
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
    verbose = TRUE) 
{
    if (is.null(key)) {
        key <- "DQAAALoAAABQS8Lok-tdR8rU1ewhKf1o7IJxS0m_X63cVuDI3ETGyg8rgWhgYTyaXBDdqIe1TUSlCzTbUi70iYQ5bsTOznfk_W9yXNG68iKxExrSxyy5iT5nXbRn3dXONOCcqkNHmJJ-zQAmwP4Gw3uyFEx2A_JES5Xru_Kaq2aJ9hOfRae8h4bqN_PKe7T_HRTg0xhwaNVWGno_tctoe5zXOHcRbEeRyFG-TTrD45ceJMSat7NPF2n7noIPFvL9SNpD026RSPM"
    }
    if (verbose) {
        cat("Making call to Google Translate..., with string of length: ", 
            nchar(sourceText), "\n")
        print(sourceText)
    }
    baseUrl <- "http://translate.google.com/researchapi/translate?"
    params <- paste("sl=", sourceLanguage, "&tl=", targetLanguage, 
        "&q=", sourceText, sep = "")
    url <- paste(baseUrl, params, sep = "")
    header <- paste("Authorization: GoogleLogin auth=", key, 
        sep = "")
    if (verbose) 
        print(url)
    curl <- getCurlHandle()
    response <- getURL(url, httpheader = header, curl = curl)
    code <- getCurlInfo(curl, which = "response.code")
    print(code)
    rm(curl)
    Sys.sleep(1)
    doc <- xmlTreeParse(response, getDTD = F)
    r <- xmlRoot(doc)
    translation <- xmlValue(r["entry"][[1]][[5]])
    return(translation)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
