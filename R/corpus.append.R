corpus.append <-
function(corpus1, newtexts, newattribs, ...) {
  # 
  # should make it also allow an optional corpus2 version where two
  # corpuses could be combined with corpus.append(corp1, corp2)
  # if we can verify the same attribute set.
  tempcorpus <- corpus.create(newtexts, attribs=newattribs)
  corpus1$attribs <- rbind(corpus1$attribs, tempcorpus$attribs)
  #corpus1$attribs$texts <- rbind(corpus1$attribs$texts, tempcorpus$attribs$texts)
  # TODO: implement concatenation of any attribs.labels from new corpus
  return(corpus1)
}
