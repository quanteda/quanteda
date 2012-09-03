\name{corpus.subset.inner}
\alias{corpus.subset.inner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corpus.subset.inner(corpus, subsetExpr = NULL, selectExpr = NULL, drop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
%%     ~~Describe \code{corpus} here~~
}
  \item{subsetExpr}{
%%     ~~Describe \code{subsetExpr} here~~
}
  \item{selectExpr}{
%%     ~~Describe \code{selectExpr} here~~
}
  \item{drop}{
%%     ~~Describe \code{drop} here~~
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
function (corpus, subsetExpr = NULL, selectExpr = NULL, drop = FALSE) 
{
    if (is.null(subsetExpr)) 
        rows <- TRUE
    else {
        rows <- eval(subsetExpr, corpus$attribs, parent.frame())
        if (!is.logical(rows)) 
            stop("'subset' must evaluate to logical")
        rows <- rows & !is.na(rows)
    }
    if (is.null(selectExpr)) 
        vars <- TRUE
    else {
        nl <- as.list(seq_along(corpus$attribs))
        names(nl) <- names(corpus$attribs)
        vars <- c(1, eval(selectExpr, nl, parent.frame()))
    }
    corpus$attribs <- corpus$attribs[rows, vars, drop = drop]
    return(corpus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
