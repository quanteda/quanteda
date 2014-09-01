
# with scan
# profiling shows that using scan is 3x faster than using strsplit
#' @export
tokenizeSingle <- function(s, clean=TRUE){
  if (clean) {
      s <- clean(s)
  }
  s <- unlist(s)
  tokens <- scan(what="char", text=s, quiet=TRUE)
  return(tokens)
}


#' @export
tokenize <- function(x, ...) {
  UseMethod("tokenize")
}

#' @export
tokenize.character <- function(text, clean=TRUE, simplify=FALSE){
    result <- lapply(text, tokenizeSingle, clean=clean)
    if (simplify | length(result)==1) {
        result <- unlist(result)
    }
    return(result)
}


#' @export
tokenize.corpus <- function(corpus, clean=TRUE){
  tokens(corpus) <- tokenize(texts(corpus), clean)
  return(corpus)
}
