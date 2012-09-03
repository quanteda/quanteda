corpus.subset <-
function(corpus, subset=NULL, select=NULL) {
  tempcorp <- corpus.subset.inner(corpus, substitute(subset), substitute(select))
  return(tempcorp)
}
