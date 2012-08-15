\name{tokenize}
\alias{tokenize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tokenize(input.text, input.text.name = "count")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.text}{
%%     ~~Describe \code{input.text} here~~
}
  \item{input.text.name}{
%%     ~~Describe \code{input.text.name} here~~
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
function (input.text, input.text.name = "count") 
{
    clean.txt <- gsub("[[:punct:][:digit:]]", "", input.text)
    input.text <- gsub("l'", "l ", input.text)
    clean.txt <- gsub("\303\237", "ss", clean.txt)
    clean.txt <- tolower(clean.txt)
    tokenized.txt <- scan(what = "char", text = clean.txt, quiet = TRUE)
    tokenized.txt <- tokenized.txt[tokenized.txt != ""]
    wf.list <- as.data.frame(table(tokenized.txt))
    names(wf.list) <- c("feature", input.text.name)
    return(wf.list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
