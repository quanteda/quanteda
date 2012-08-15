corpus.append <-
function(corpus1, newtexts, newattribs, ...) {
  # function to add new texts and attributes to an existing corpus
  # should make it also allow an optional corpus2 version where two
  # corpuses could be combined with corpus.append(corp1, corp2)
  # if we can verify the same attribute set.
  tempcorpus <- corpus.create(newtexts, attribs=newattribs)
  corpus1$attribs <- rbind(tempcorpus$attribs, corpus1$attribs)
  # TODO: implement concatenation of any attribs.labels from new corpus
  return(corpus1)
}
