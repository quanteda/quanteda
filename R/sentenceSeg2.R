sentenceSeg2 <-
function(text, sentence.delimiters="[.!?]") {
  # strip out CRs and LFs, tabs
  text <- gsub("\\n\\t", "", text)     
  # split the text into sentences
  # WOULD BE NICE TO PRESERVE SENTENCE PUNCTUATION
  sentences <- unlist(strsplit(text, sentence.delimiters))
  # COULD STRIP LEADING SPACES HERE TOO
  return(sentences)
}
