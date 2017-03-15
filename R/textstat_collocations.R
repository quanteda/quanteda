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
#' 
#' toks <- tokens(data_char_inaugural, removePunct = TRUE)
#' toks <- tokens_remove(toks, stopwords(), padding = TRUE)
#' 
#' # use contexts and tokens for target collocation
#' coxs <- contexts(toks, "econom*", window = 10, valuetype = "glob")
#' head(textstat_collocations(coxs, toks), 10)
#' 
#' # use fcm and dfm for all-all collocation 
#' fcm <- fcm(toks, tri = FALSE)
#' col_pmi <- textstat_collocations(fcm, dfm(toks), measure = "pmi", top = 5000)
#' col_pmi[grepl("econom", col_pmi$collocation),]
#' head(col_pmi, 20)
#' 
#' toks <- tokens("A D A C E A D F E B A C E D")
#' col_chi2 <- textstat_collocations(fcm(toks, 'window', window = 2, tri = FALSE), 
#'                                   dfm(toks, tolower = FALSE), measure = "chi2", top = 3)
#' 
#' 
#' 
textstat_collocations <- function(x, y, ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.fcm <- function(x, y, measure = c("pmi", "pmi3", "chi2"), top = 1000, sort = TRUE) {
    
    features <- featnames(x)
    features <- features[features != ""]
    x <- x[features, features] # remove padding
    y <- dfm_select(y, features, valuetype = 'fixed', case_insensitive = FALSE, padding = TRUE)
    
    n <- x
    m <- Matrix::colSums(y, sparseResult = TRUE)
    m <- m + 1 # soomthing
    
    mm <- m %*% t(m)
    if (measure == 'pmi') {
        v <- log((n / mm))
    } else if(measure == 'pmi3') {
        v <- log(((n ^ 3) / mm))
    } else {
        s <- sum(m) # grand sum
        a <- n
        b <- m - n
        c <- m - t(n)
        d <- s - (a + b + c)
        e <- (a + b) * (a + c) / s # expected count
        v = (s * (abs(a * d - b * c) - s / 2) ^ 2) / 
            ((a + b) * (c + d) * (a + c) * (b + d)) * 
            ((a < e) * -1)
        
        print(a)
        print(m)
        print(b)
    }

    index <- which(v@x > sort(v@x, decreasing = TRUE)[top])
    dimnames <- v@Dimnames[[1]]
    dims <- v@Dim
    print(index)
    result <- data.frame(collocation = paste(dimnames[index %/% dims[1]], dimnames[index %% dims[1]]),
                         value = v@x[index], count = n@x[index], stringsAsFactors = FALSE) 
    if (sort)
        result <- result[order(result$value, decreasing = TRUE),]
    
    return(result)
}

#' @noRd
#' @export
textstat_collocations.dfm <- function(x, y, ...) {
    
    features <- featnames(x)
    features <- features[features != ""]
    x <- dfm_select(x, features, valuetype = 'fixed', case_insensitive = FALSE)
    y <- dfm_select(y, features, valuetype = 'fixed', case_insensitive = FALSE, padding = TRUE)
    z <- rbind(Matrix::colSums(x), Matrix::colSums(y))
    z[2,] <- z[2,] - z[1,]
    result <- textstat_keyness(as.dfm(z), target = 1L, ...)
    return(result)
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, y, ...) {
    
    textstat_collocations(dfm(x, tolower=FALSE), dfm(y, tolower=FALSE), ...)
}

