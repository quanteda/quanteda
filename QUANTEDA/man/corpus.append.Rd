\name{corpus.append}
\alias{corpus.append}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corpus.append(corpus1, newtexts, newattribs, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus1}{
%%     ~~Describe \code{corpus1} here~~
}
  \item{newtexts}{
%%     ~~Describe \code{newtexts} here~~
}
  \item{newattribs}{
%%     ~~Describe \code{newattribs} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (corpus1, newtexts, newattribs, ...) 
{
    tempcorpus <- corpus.create(newtexts, attribs = newattribs)
    corpus1$attribs <- rbind(tempcorpus$attribs, corpus1$attribs)
    return(corpus1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
