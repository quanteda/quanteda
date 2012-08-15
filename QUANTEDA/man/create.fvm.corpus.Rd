\name{create.fvm.corpus}
\alias{create.fvm.corpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.fvm.corpus(corpus, feature = c("word"), groups = NULL, subset = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
%%     ~~Describe \code{corpus} here~~
}
  \item{feature}{
%%     ~~Describe \code{feature} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
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
function (corpus, feature = c("word"), groups = NULL, subset = NULL, 
    verbose = TRUE) 
{
    if (verbose) 
        cat("Creating fvm:\n")
    corpus <- corpus.subset.inner(corpus, substitute(subset))
    if (!is.null(groups)) {
        if (verbose) 
            cat("  Now aggregating by group: ", groups, "...", 
                sep = "")
        texts <- split(corpus$attribs$texts, as.factor(corpus$attribs[, 
            groups]))
        texts <- sapply(texts, paste)
        if (verbose) 
            cat("complete.\n")
    }
    else {
        texts <- corpus$attribs$texts
        names(texts) <- rownames(corpus$attribs)
    }
    textnames <- names(texts)
    fvm <- data.frame(feature = NA)
    progress.threshold <- 0.1
    if (verbose) 
        cat("  Progress (\%): [0")
    for (i in 1:length(texts)) {
        if (i/length(texts) > progress.threshold) {
            if (verbose) 
                cat(paste("...", progress.threshold * 100, sep = ""))
            progress.threshold <- progress.threshold + 0.1
        }
        temp.fvm <- tokenize(texts[i], textnames[i])
        fvm <- merge(fvm, temp.fvm, by = "feature", all = TRUE)
    }
    fvm[is.na(fvm)] <- 0
    fvm <- fvm[-which(apply(fvm[, -1], 1, sum) == 0), ]
    if (verbose) 
        cat("]\n")
    rownames(fvm) <- fvm[, "feature"]
    fvm <- fvm[, -1]
    if (verbose) 
        cat("\n")
    return(fvm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
