##
## which token data representations are most efficient?
##

require(quanteda)
data(SOTUCorpus, package = "quantedaData")

## ... are passed to tokenize
compareSizes <- function(texts, ...) {
    require(data.table)
    toks <- tokenize(texts, ...)
    toksUnlisted <- unlist(toks, use.names = TRUE)
    toksUnlisted.unnamed <- unlist(toks, use.names = FALSE)
    toksDT <- data.table(docIndex = rep(1:length(toks), times = lengths(toks)), token = toksUnlisted)
    tokSize <- object.size(toks)
    unlistedtokSize <- object.size(toksUnlisted)
    unlistedtokSize.unnamed <- object.size(toksUnlisted.unnamed)
    dtSize <- object.size(toksDT)
    results <- c(tokSize, unlistedtokSize, unlistedtokSize.unnamed, dtSize) / tokSize * 100
    names(results) <- c("tokenizedTexts", "character", "character.unnamed", "data.table")
    results <- round(results, 2)
    cat("Sizes in % relative to tokenizedTexts size:\n")
    print(results)
    invisible(results)
}
    
compareSizes(SOTUCorpus)
## Sizes in % relative to tokenizedTexts size:
##     tokenizedTexts         character character.unnamed        data.table 
##             100.00            429.80             46.48             67.46 

compareSizes(SOTUCorpus, what = "sentence")
## Sizes in % relative to tokenizedTexts size:
##     tokenizedTexts         character character.unnamed        data.table 
##             100.00            129.20             99.28            101.02 

