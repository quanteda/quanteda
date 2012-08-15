\name{summary.corpus}
\alias{summary.corpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.corpus(corpus, texts = "texts", subset = NULL, select = NULL, drop = FALSE, output = TRUE, nmax = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
%%     ~~Describe \code{corpus} here~~
}
  \item{texts}{
%%     ~~Describe \code{texts} here~~
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
}
  \item{select}{
%%     ~~Describe \code{select} here~~
}
  \item{drop}{
%%     ~~Describe \code{drop} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{nmax}{
%%     ~~Describe \code{nmax} here~~
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
function (corpus, texts = "texts", subset = NULL, select = NULL, 
    drop = FALSE, output = TRUE, nmax = 100) 
{
    corpus <- corpus.subset.inner(corpus, substitute(subset), 
        substitute(select))
    cat("Corpus object contains", nrow(corpus$attribs), "texts.\n\n")
    texts <- corpus$attribs[, texts]
    attribs <- as.data.frame(corpus$attribs[, -1])
    if (ncol(attribs) == 1) 
        names(attribs) <- as.character(substitute(select))[2]
    names(texts) <- rownames(corpus$attribs)
    print(head(cbind((dtexts <- describetexts(texts, output = FALSE)), 
        attribs), nmax))
    cat("\nSource:  ", corpus$metadata["source"], ".\n", sep = "")
    cat("Created: ", corpus$metadata["created"], ".\n", sep = "")
    cat("Notes:   ", corpus$metadata["notes"], ".\n\n", sep = "")
    return(invisible(dtexts))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
