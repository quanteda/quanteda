corpus.subset.inner <-
function(corpus, subsetExpr=NULL, selectExpr=NULL, drop=FALSE) {
  # This is the "inner" function to be called by other functions
  # to return a subset directly, use corpus.subset
  
  # The select argument exists only for the methods for data frames and matrices. 
  # It works by first replacing column names in the selection expression with the 
  # corresponding column numbers in the data frame and then using the resulting 
  # integer vector to index the columns. This allows the use of the standard indexing 
  # conventions so that for example ranges of columns can be specified easily, 
  # or single columns can be dropped
  # as in:
  # subset(airquality, Temp > 80, select = c(Ozone, Temp))
  # subset(airquality, Day == 1, select = -Temp)
  # subset(airquality, select = Ozone:Wind)
  #'@export
    
    if (is.null(subsetExpr)) 
      rows <- TRUE
    else {
      rows <- eval(subsetExpr, corpus$attribs, parent.frame())
      if (!is.logical(rows)) 
        stop("'subset' must evaluate to logical")
      rows <- rows & !is.na(rows)
    }
    
    if (is.null(selectExpr)) 
      vars <- TRUE
    else {
      
      nl <- as.list(seq_along(corpus$attribs))
      names(nl) <- names(corpus$attribs)
      vars <- c(1, eval(selectExpr, nl, parent.frame()))
    }
    # implement subset, select, and drop
    corpus$attribs <- corpus$attribs[rows, vars, drop=drop]
    return(corpus)
}
