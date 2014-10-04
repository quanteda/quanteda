#' Function to declare a connection to a directory (containing files)
#' 
#' Function to declare a connection to a directory, although unlike \link{file}
#' it does not require closing. If the directory does not exist, the function
#' will return an error.
#' 
#' @param path  String describing the full path of the directory or NULL to use
#'   a GUI to choose a directory from disk
#' @export
#' @examples 
#' \dontrun{
#' # name a directory of files
#' mydir <- directory("~/Dropbox/QUANTESS/corpora/ukManRenamed")
#' corpus(mydir)
#' 
#' # choose a directory using a GUI
#' corpus(directory())} 
#' @export
directory <- function(path=NULL) {
    # choose it from a GUI if none exists
    if (is.null(path)) {
        if (require(tcltk2))
            texts <- tk_choose.dir()
        if (is.na(texts)) stop("Directory selection cancelled by user.")
        else
            stop("you need tcltk2 installed to use GUI directory selection.")
    }
    stopifnot(class(path) == "character")
    stopifnot(file.exists(path))
    tempPath <- path
    class(tempPath) <- list("directory", class(tempPath))
    return(tempPath)
}


#' Function to declare a twitter search
#' 
#' Function to declare a connection to a twitter search
#' 
#' @param query  String describing the search query terms
#' @param numResults  Number of tweets to return. Maximum of approximately 1500
#' @param key  Key for twitter API authentication
#' @param cons_secret  for twitter API authentication
#' @param token  String for twitter API authentication
#' @param access_secret  for twitter API authentication
#' @export
#' @return The search results marked as a 'twitter' object for use by 
#'   corpus.twitter()
#' @export
getTweets <- function(query, numResults=50, key, cons_secret, token, access_secret) {
    # choose it from a GUI if none exists
    library('twitteR')
    print('authorizing... ')
    setup_twitter_oauth(key, cons_secret, token, access_secret)
    print('running search... ')
    sea <- (searchTwitter(query, numResults))
    print('returning results.')
    results <-  twListToDF(sea)
    tempRes <- results
    class(tempRes) <- list('twitter', class(results))
    return(tempRes)
}




#' unzip a zipped collection of text files and return the directory
#' 
#' \code{zipfiles} extracts a set of text files in a zip archives, and returns
#' the name of the temporary directory where they are stored.  It can be passed 
#' to \link{corpus.directory} for import.
#' @param zfile a character string specifying the name (including path) of the 
#'   zipped file, or a URL naming the file (see example); or NULL to use a GUI
#'   to choose a file from disk
#' @param ... additional arguments passed to \link{unzip}
#' @return a \link{directory} class object containing the unzipped files
#' @examples
#' \dontrun{
#' # from a zip file on the web
#' myzipcorp <- corpus(zipfiles("http://kenbenoit.net/files/EUcoalsubsidies.zip"),
#'                     notes="From some EP debate about coal mine subsidies")
#' docvars(myzipcorp, "speakername") <- docnames(myzipcorp)
#' summary(myzipcorp)
#' 
#' # call up interactive user input
#' myzipcorp <- corpus(zipfiles())
#' }
#' @export
zipfiles <- function(zfile=NULL, ...) {
    if (is.null(zfile)) {
        zfile <- file.choose()
    }

    # get temp directory name
    tmpdir <- tempdir()

    # if it's a URL
    if (substr(zfile, 1, 7)=="http://") {
        download.file(zfile, destfile=paste(tmpdir, "/tmp.zip", sep=""))
        zfile <- paste(tmpdir, "/tmp.zip", sep="")
    }

    unzip(zfile, exdir=paste(tmpdir, "/tmp/", sep=""), ...)
    return(directory(paste(tmpdir, "/tmp/", sep="")))
}

