

testfn <- function(x, ...) {
    UseMethod("testfn")
}

testfn.character <- function(x, verbose=TRUE, ...) {
    if (verbose) cat("VERBOSE was set to TRUE.\n")
    clean(x, ...)    
}

