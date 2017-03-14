#' calculate collocation statistics
#' 
#' @rdname textstat_collocations
#' @param x a \link{dfm} containing the features to be examined for keyness
#' @param target the document index (numeric, character or logical) identifying the 
#'   document forming the "target" for computing keyness; all other documents' 
#'   feature frequencies will be combined for use as a reference
#' @param measure (signed) association measure to be used for computing keyness.
#'   Currenly available: \code{"chi2"} (\eqn{chi^2} with Yates correction); 
#'   \code{"exact"} (Fisher's exact test); \code{"lr"} for the likelihood ratio
#'   \eqn{G} statistic with Yates correction.
#' @param sort logical; if \code{TRUE} sort features scored in descending order 
#'   of the measure, otherwise leave in original feature order
#' @export
#' @keywords textstat
#' @examples
#' toks <- tokens(data_char_inaugural, removePunct = TRUE)
#' toks <- tokens_remove(toks, stopwords(), padding = TRUE)
#' coxs <- contexts(toks, "econom*", window = 10, valuetype = "glob")
#' head(textstat_collocations(coxs, toks), 10)
#' 
#' fcm <- fcm(toks, tri = FALSE)
#' pmi <- textstat_collocations(fcm, dfm(toks), "pmi3")
#' pmi[grepl("econom", pmi$collocation),]
#' head(pmi, 20)
#' 
textstat_collocations <- function(x, y, ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.fcm <- function(x, y, measure = c("pmi", "pmi3"), top = 1000, sort = TRUE) {
    
    features <- featnames(x)
    features <- features[features != ""]
    x <- x[features, features] # remove padding
    y <- dfm_select(y, features, valuetype = 'fixed', case_insensitive = FALSE, padding = TRUE)
    
    n <- x
    m <- Matrix::colSums(y, sparseResult = TRUE)
    m <- m + 1 # soomthing
    
    mm <- m %*% t(m)
    if (measure == 'pmi3') {
        mi <- log(((n ^ 3) / mm))
    } else {
        mi <- log((n / mm))
    }
    
    index <- which(mi@x > sort(mi@x, decreasing = TRUE)[top])
    dimnames <- mi@Dimnames[[1]]
    dims <- mi@Dim
        
    result <- data.frame(collocation = paste(dimnames[index %/% dims[1]], dimnames[index %% dims[1]]),
                         pmi = mi@x[index], count = n@x[index], stringsAsFactors = FALSE) 
    if (sort)
        result <- result[order(result$pmi, decreasing = TRUE),]
    
    return(result)
}

#' @noRd
#' @export
textstat_collocations.dfm <- function(x, y, ...) {
    
    y <- dfm_select(y, featnames(x), valuetype = 'fixed', case_insensitive = FALSE, padding = TRUE)
    z <- rbind(Matrix::colSums(x), Matrix::colSums(y))
    z[2,] <- z[2,] - z[1,]
    textstat_keyness(as.dfm(z), target = 1L, ...)
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, y, ...) {
    
    textstat_collocations(dfm(x), dfm(y), ...)
}

