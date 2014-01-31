create.text <-
function(string, fname, atts=NULL){
  # a text has a list of attribute:value pairs
  if(is.null(atts))
  {
    atts <- list(fname=fname)   
  }
  temp.text <- list(string=string, atts=atts)
  class(temp.text) <- list("text", class(temp.text))
  return(temp.text)
}
