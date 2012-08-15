\name{translate.corpus}
\alias{translate.corpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
translate.corpus(corpus, targetlanguageString, textvar = "texts", languagevar = "language")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
%%     ~~Describe \code{corpus} here~~
}
  \item{targetlanguageString}{
%%     ~~Describe \code{targetlanguageString} here~~
}
  \item{textvar}{
%%     ~~Describe \code{textvar} here~~
}
  \item{languagevar}{
%%     ~~Describe \code{languagevar} here~~
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
function (corpus, targetlanguageString, textvar = "texts", languagevar = "language") 
{
    translatedTextVector <- rep(NA, nrow(corpus$attribs))
    for (i in 1:nrow(corpus$attribs)) {
        if (corpus$attribs[i, textvar] == "" | is.na(corpus$attribs[i, 
            textvar])) 
            next
        if (corpus$attribs[i, languagevar] == targetlanguageString) 
            next
        translatedTextVector[i] <- translate(corpus$attribs[i, 
            textvar], corpus$attribs[i, languagevar], targetlanguageString)
    }
    return(translatedTextVector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
