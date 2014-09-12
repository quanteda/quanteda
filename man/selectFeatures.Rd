\name{selectFeatures}
\alias{selectFeatures}
\title{extract feature words
This function takes type of feature extractor and a word freaquency matrix
with binary class (1/0) to select features in class one. 'wsll' and
'wschisq' replicates of 'Keyness' of Wordsmith Tools.}
\usage{
selectFeatures(extractor, dfm, class, smooth = 1, show = 10)

selectFeatures(extractor, dfm, class, smooth = 1, show = 10)
}
\arguments{
  \item{extractor}{Type of feature extractor}

  \item{dfm}{Word frequency matrix}

  \item{class}{Biarny class}

  \item{smooth}{Smoothing constant}

  \item{show}{Number of features shown}

  \item{extractor}{Type of feature extractor}

  \item{dfm}{Word frequency matrix}

  \item{class}{Biarny class}

  \item{smooth}{Smoothing constant}

  \item{show}{Number of features shown}
}
\value{
data frame of feature words

data frame of feature words
}
\description{
extract feature words This function takes type of feature
extractor and a word freaquency matrix with binary class
(1/0) to select features in class one. 'wsll' and 'wschisq'
replicates of 'Keyness' of Wordsmith Tools.

extract feature words This function takes type of feature
extractor and a word freaquency matrix with binary class
(1/0) to select features in class one. 'wsll' and 'wschisq'
replicates of 'Keyness' of Wordsmith Tools.
}
\examples{
\dontrun{
texts <- getTextDir("/home/kohei/Documents/budget_2010/")
class  <- rep(0, length(texts))
class[grep("_LAB", names(texts))] <- 1
class[grep("_FF", names(texts))] <- 0
corpus <- corpusCreate(texts, attribs=list(class=class))
dfm <- dfm(corpus)
features <- selectFeatures('ll', dfm, corpus$attribs$class, smooth=1)
}
\dontrun{
texts <- getTextDir("/home/kohei/Documents/budget_2010/")
class  <- rep(0, length(texts))
class[grep("_LAB", names(texts))] <- 1
class[grep("_FF", names(texts))] <- 0
corpus <- corpusCreate(texts, attribs=list(class=class))
dfm <- dfm(corpus)
features <- selectFeatures('ll', dfm, corpus$attribs$class, smooth=1)
}
}
\author{
Kohei Watanabe

Kohei Watanabe
}

