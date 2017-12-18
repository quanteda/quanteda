
#' @noRd
#' @export
#' @examples
#' period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
#' mydfm <- dfm(data_corpus_inaugural, groups = period)
#' result <- textstat_keyness(mydfm)
#' subset(result, pattern = 'a*')
subset.textstat <- function(x, pattern, 
                            selection = c("keep", "remove"), 
                            valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE) {
        
        selection <- match.arg(selection)
        valuetype <- match.arg(valuetype)
        
        attrs <- attributes(x)
        id <- unlist(quanteda:::regex2id(pattern, rownames(x), valuetype, case_insensitive))
        if (selection == 'keep') {
            x <- x[id ,]
        } else {
            x <- x[id * -1,]
        }
        class(x) <- attrs$class
        return(x)
}