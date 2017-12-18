#' donlowad RDS file from online locations
#'
#' @param name name of the RDS file
#' @param url user-specified location of the RDS file
#' @param cache if \code{TRUE}, save downloaded file to temporary folder
#' @param ... extra argument passed to \code{download.file}
#' @details Currently only \code{data_corpus_foreignaffairscommittee} and
#' \code{data_corpus_guardian} are available.
#' @return an unserialized R object
#' @export
#'
#' @examples
#' \donotrun{
#' corp <- data_download('data_corpus_guardian')
#' corp <- data_download('data_corpus_foreignaffairscommittee')
#' }
data_download <- function(name = NULL, url = NULL, cache = TRUE, ...) {
    
    # increment the v parameter in the url to force users to reflesh local cache
    location <- list('data_corpus_foreignaffairscommittee' = 
                         'https://www.dropbox.com/s/e1tb76d57oqc79g/data_corpus_foreignaffairscommittee.rds?dl=1&v=1',
                     'data_corpus_guardian' = 
                         'https://www.dropbox.com/s/7mu92jzodpq11zc/data_corpus_guardian.rds?dl=1&v=1')
    
    if (name %in% names(location)) {
        url <- location[[name]]
    } else {
        stop(name, "is not available\n")
    }
    if (is.null(url)) {
        stop("name or url needs to be specified\n")
    }
    temp <- paste0(tempdir(), "/", digest::digest(url, 'md5'))
    if (!file.exists(temp) || !cache) {
        download.file(url, destfile = temp, ...)
        if (!cache)
            file.remove(temp)
    }
    readRDS(temp)
    
}
