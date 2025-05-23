% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokens_lookup.R
\name{tokens_lookup}
\alias{tokens_lookup}
\title{Apply a dictionary to a tokens object}
\usage{
tokens_lookup(
  x,
  dictionary,
  levels = 1:5,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  capkeys = !exclusive,
  exclusive = TRUE,
  nomatch = NULL,
  append_key = FALSE,
  separator = "/",
  concatenator = concat(x),
  nested_scope = c("key", "dictionary"),
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)
}
\arguments{
\item{x}{the \link{tokens} object to which the dictionary will be applied}

\item{dictionary}{the \link{dictionary}-class object that will be applied to
\code{x}}

\item{levels}{integers specifying the levels of entries in a hierarchical
dictionary that will be applied.  The top level is 1, and subsequent levels
describe lower nesting levels.  Values may be combined, even if these
levels are not contiguous, e.g. \code{levels = c(1:3)} will collapse the second
level into the first, but record the third level (if present) collapsed
below the first (see examples).}

\item{valuetype}{the type of pattern matching: \code{"glob"} for "glob"-style
wildcard expressions; \code{"regex"} for regular expressions; or \code{"fixed"} for
exact matching. See \link{valuetype} for details.}

\item{case_insensitive}{logical; if \code{TRUE}, ignore case when matching a
\code{pattern} or \link{dictionary} values}

\item{capkeys}{if \code{TRUE}, convert dictionary keys to uppercase to distinguish
them from unmatched tokens.}

\item{exclusive}{if \code{TRUE}, remove all features not in dictionary,
otherwise, replace values in dictionary with keys while leaving other
features unaffected.}

\item{nomatch}{an optional character naming a new key for tokens that do not
matched to a dictionary values  If \code{NULL} (default), do not record
unmatched tokens.}

\item{append_key}{if \code{TRUE}, annotate matched tokens with keys.}

\item{separator}{a character to separate tokens and keys when \code{append_key = TRUE}.}

\item{concatenator}{the concatenation character that will connect the words
making up the multi-word sequences.}

\item{nested_scope}{how to treat matches from different dictionary keys that
are nested.  When one value is nested within another, such as "a b" being
nested within "a b c", then \code{tokens_lookup()} will match the longer.  When
\code{nested_scope = "key"}, this longer-match priority is applied only
within the key, while \code{"dictionary"} applies it across keys, matching only
the key with the longer pattern, not the matches nested within that longer
pattern from other keys.  See Details.}

\item{apply_if}{logical vector of length \code{ndoc(x)}; documents are modified
only when corresponding values are \code{TRUE}, others are left unchanged.}

\item{verbose}{if \code{TRUE} print the number of tokens and documents before and
after the function is applied. The number of tokens does not include paddings.}
}
\description{
Convert tokens into equivalence classes defined by values of a dictionary
object.
}
\details{
Dictionary values may consist of sequences, and there are different
methods of counting key matches based on values that are nested or that
overlap.

When two different keys in a dictionary are nested matches of one another,
the \code{nested_scope} options provide the choice of matching each key's
values independently (the \code{"key"}) option, or just counting the
longest match (the \code{"dictionary"} option).  Values that are nested
\emph{within} the same key are always counted as a single match.  See the
last example below comparing the \emph{New York} and \emph{New York Times}
for these two different behaviours.

\emph{Overlapping values}, such as \code{"a b"} and \code{"b a"} are
currently always considered as separate matches if they are in different
keys, or as one match if the overlap is within the same key.

Note: \code{apply_if} This applies the dictionary lookup only to documents that
match the logical condition.  When \code{exclusive = TRUE} (the default),
however, this means that empty documents will be returned for those not
meeting the condition, since no lookup will be applied and hence no tokens
replaced by matching keys.
}
\examples{
toks1 <- tokens(data_corpus_inaugural)
dict1 <- dictionary(list(country = "united states",
                   law=c("law*", "constitution"),
                   freedom=c("free*", "libert*")))
dfm(tokens_lookup(toks1, dict1, valuetype = "glob", verbose = TRUE))
dfm(tokens_lookup(toks1, dict1, valuetype = "glob", verbose = TRUE, nomatch = "NONE"))

dict2 <- dictionary(list(country = "united states",
                       law = c("law", "constitution"),
                       freedom = c("freedom", "liberty")))
# dfm(applyDictionary(toks1, dict2, valuetype = "fixed"))
dfm(tokens_lookup(toks1, dict2, valuetype = "fixed"))

# hierarchical dictionary example
txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
         d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
toks2 <- tokens(txt)
dict3 <- dictionary(list(US = list(Countries = c("States"),
                                  oceans = c("Atlantic", "Pacific")),
                        Europe = list(Countries = c("Britain", "Ireland"),
                                      oceans = list(west = "Irish Sea",
                                                    east = "English Channel"))))
tokens_lookup(toks2, dict3, levels = 1)
tokens_lookup(toks2, dict3, levels = 2)
tokens_lookup(toks2, dict3, levels = 1:2)
tokens_lookup(toks2, dict3, levels = 3)
tokens_lookup(toks2, dict3, levels = c(1,3))
tokens_lookup(toks2, dict3, levels = c(2,3))

# show unmatched tokens
tokens_lookup(toks2, dict3, nomatch = "_UNMATCHED")

# nested matching differences
dict4 <- dictionary(list(paper = "New York Times", city = "New York"))
toks4 <- tokens("The New York Times is a New York paper.")
tokens_lookup(toks4, dict4, nested_scope = "key", exclusive = FALSE)
tokens_lookup(toks4, dict4, nested_scope = "dictionary", exclusive = FALSE)

}
\seealso{
tokens_replace
}
\keyword{tokens}
