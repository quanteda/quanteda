\name{corpus.create}
\alias{corpus.create}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corpus.create(texts, textnames = NULL, attribs = NULL, source = NULL, notes = NULL, attribs.labels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{
%%     ~~Describe \code{texts} here~~
}
  \item{textnames}{
%%     ~~Describe \code{textnames} here~~
}
  \item{attribs}{
%%     ~~Describe \code{attribs} here~~
}
  \item{source}{
%%     ~~Describe \code{source} here~~
}
  \item{notes}{
%%     ~~Describe \code{notes} here~~
}
  \item{attribs.labels}{
%%     ~~Describe \code{attribs.labels} here~~
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
function (texts, textnames = NULL, attribs = NULL, source = NULL, 
    notes = NULL, attribs.labels = NULL) 
{
    if (is.null(names(texts))) 
        names(texts) <- paste("text", 1:length(texts), sep = "")
    if (is.null(source)) 
        source <- paste(getwd(), "/* ", "on ", Sys.info()["machine"], 
            " by ", Sys.info()["user"], sep = "")
    created <- date()
    metadata <- c(source = source, created = created, notes = notes)
    if (!is.null(attribs)) {
        attribs <- data.frame(texts = texts, attribs, row.names = names(texts), 
            check.rows = TRUE, stringsAsFactors = FALSE)
    }
    if (!is.null(attribs) & is.null(attribs.labels)) {
        attribs.labels <- c("Original texts", rep(NULL, length(attribs) - 
            1))
    }
    temp.corpus <- list(attribs = attribs, attribs.labels = attribs.labels, 
        metadata = metadata)
    class(temp.corpus) <- list("corpus", class(temp.corpus))
    return(temp.corpus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
