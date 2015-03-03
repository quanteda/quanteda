

#'Function to declare a connection to an excel file
#'
#'Function to declare a connection to a excel file.
#'
#'@param path  String describing the full path to the excel file or NULL to use 
#'  a GUI to choose a directory from disk
#'@param sheetIndex  The index of the sheet of the excel file to read (as passed
#'  to read.xlsx2)
#'@export
excel <- function(path=NULL, sheetIndex=1) {
    if(!requireNamespace("xlsx", quietly = TRUE)){
        stop("The xlsx package is needed for this function to work. Please install it.",
             call. = FALSE)
    }
    # choose it from a GUI if none exists
    if (is.null(path)) {
        if (require(tcltk2))
            texts <- tcltk::tk_choose.dir()
        if (is.na(texts)) stop("Directory selection cancelled by user.")
        else
            stop("you need tcltk2 installed to use GUI directory selection.")
    }
    stopifnot(class(path) == "character")
    stopifnot(file.exists(path))
    
    sheet <- xlsx::read.xlsx2(path,  stringsAsFactors=FALSE, sheetIndex=sheetIndex)
    class(sheet) <- (list("excel","data.frame"))
    
    return(sheet)
}


#' @param textCol  The column of the sheet that contains the texts
#'   the docvars from. By defauls, takes everything except the textCol by
#'   \code{sep} or from meta-data embedded in the text file header
#'   (\code{headers}).
#' @rdname corpus
#' @export
#' @examples 
#' \dontrun{
#'} 
#' 
corpus.excel <- function(x, enc=NULL, docnames=row.names(x),
                         textCol=1, docvarsfrom=NULL, 
                         source=NULL, notes=NULL, citation=NULL, ...) {
    if (is.null(docvarsfrom)){
        docvarsfrom <- -textCol
    }
    if (class(x)[1] != "excel") stop("first argument must be an excel sheet")
    x <- data.table(x)
    class(x) <- (list("excel", "data.table", "data.frame")) # data.table unclasses it
    dvars <- NULL    
    txts <- as.character(unlist(x[,textCol,with=FALSE]))
    dvars <- x[, docvarsfrom, with=FALSE]
    tmpCorp <- corpus(txts, enc=enc, docnames=docnames, docvars=dvars,
                      source=source, notes=notes, citation=citation)
    return(tmpCorp)
}

#' @rdname corpus
#' @export
corpus.facebook <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content ("message" of posts)
    texts <- x$message
    atts <- as.data.frame(x[,2:ncol(x)])    
    
    # not sure I'm doing this the right way... What is metadata?
    corpus(texts, docvars=atts,
           source=paste("Converted from posts on Facebook page"),
           enc=enc, ...)
}

#' @rdname corpus
#' @export
corpus.twitter <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content (texts)
    texts <- x$text
    atts <- as.data.frame(x[,2:ncol(x)])    
    
    # using docvars inappropriately here but they show up as docmeta given 
    # the _ in the variable names
    corpus(texts, docvars=atts,
           source=paste("Converted from twitter search results"),
           enc=enc, ...)
}

#' Constructor for corpus objects from urls
#' 
#' Creates a corpus from a url object. 
#' @rdname corpus
#' @export
corpus.url <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content (texts)
    txt <- paste(scan(x, what="character", sep="\n"), collapse="\n")
    dets <- summary(x)
    close(x)
    corpus(txt, source=paste(sprintf("Loaded from url at %s:", dets$description ),
                             enc=enc, ...))
    
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
    if(!requireNamespace("twitteR", quietly = TRUE)){
        stop("The twitteR package is needed for this function to work. Please install it.")
    }
    print('authorizing... ')
    twitteR::setup_twitter_oauth(key, cons_secret, token, access_secret)
    print('running search... ')
    sea <- (twitteR::searchTwitter(query, numResults))
    print('returning results.')
    results <-  twitteR::twListToDF(sea)
    tempRes <- results
    class(tempRes) <- list('twitter', class(results))
    return(tempRes)
}

#' return a time-line of most recent Tweets from a given user
#'
#' Connect to the REST API of Twitter and returns up to
#' 3,200 recent tweets sent by this user.
#'
#' @param screen_name user name of the Twitter user for which tweets
#' will be downloaded
#' @param filename file where tweets will be stored (in json format).
#' If "default", they will be stored in a file whose name is the
#' screen name of the queried user. If \code{NA} or \code{NULL},
#' tweets will be stored in a temporary file that will be deleted.
#' @param numResults number of tweets to be downloaded (maximum is 3,200)
#' @param key  Key for twitter API authentication
#' @param cons_secret  for twitter API authentication
#' @param token  String for twitter API authentication
#' @param access_secret  for twitter API authentication
#' @param df If \code{TRUE}, will return tweets in data frame format.
#' If \code{FALSE}, will only store tweets in json format in disk.
#' @author Pablo Barbera
#' @export
#' @import jsonlite httr streamR
#' @examples \dontrun{
#'  key = 'your consumer key here'
#'  cons_secret = 'your consumer secret here'
#'  token = 'your access token here'
#'  access_secret = 'your access secret here'
#'
#'  # download recent tweets by user "p_barbera"
#'  tweets <- getTimeline(screen_name="p_barbera", numResults=600,
#'      filename='p_barbera.json', key, cons_secret, token, access_secret)
#'
#'  # creating corpus object
#'  twcorpus <- corpus(tweets)
#'  summary(twcorpus)
#'
#'  # viewing the DFM using a word cloud
#'  twDfm <- dfm(twcorpus, gnoredFeatures = c(stopwordsGet(), "rt"))
#'  plot(twDfm)
#' }
#'
getTimeline <- function(screen_name, numResults=200, filename="default", 
                        key, cons_secret, token, access_secret, df=TRUE) {
    
    if (filename=="default"){
        filename <- paste0(getwd(), "/", screen_name, '.json')
    }
    # information about where tweets have been stored
    if (!is.null(filename) && !is.na(filename)){
        cat("tweets will be stored in JSON format in file:", filename, "\n")
    }
    if (is.null(filename) || is.na(filename)){
        filename <- tempfile()
    }
    
    # library(httr); library(rjson); library(streamR)
    cat('authorizing...\n')
    # authorization (manual, not using twitteR package)
    app <- httr::oauth_app("twitter", key = key, secret = cons_secret)
    sig <- httr::sign_oauth1.0(app, token=token, token_secret=access_secret)
    
    # parameters for API query
    url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
    params <- list(count=min(c(numResults, 200)), 
                   screen_name = screen_name, include_rts="true",
                   exclude_replies="false", trim_user="false")
    query <- lapply(params, function(x) URLencode(as.character(x)))
    
    # first query
    url.data <- httr::GET(url, query=query, config(token=sig[["token"]]))
    json.data <- httr::content(url.data)
    
    # writing to disk
    conn <- file(filename, "a", encoding='UTF-8')
    invisible(lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn)))
    close(conn)        
    
    ## max_id
    tweets <- length(json.data)
    max_id <- json.data[[tweets]]$id_str
    cat(tweets, "tweets.\n")
    max_id_old <- "none"
    
    ## rest of queries
    while (tweets < numResults & max_id != max_id_old){
        max_id_old <- max_id
        params <- list(count=200, screen_name = screen_name, max_id=max_id, 
                       include_rts="true", exclude_replies="false", trim_user="false")
        query <- lapply(params, function(x) URLencode(as.character(x)))
        url.data <- httr::GET(url, query=query, config(token=sig[["token"]]))
        json.data <- httr::content(url.data)
        
        # writing to disk
        if (!is.null(filename) && !is.na(filename)){
            conn <- file(filename, "a", encoding='UTF-8')
            invisible(lapply(json.data, function(x) 
                writeLines(jsonlite::toJSON(x, null="null"), con=conn)))
            close(conn)        
        }
        
        ## max_id
        tweets <- tweets + length(json.data)
        max_id <- json.data[[length(json.data)]]$id_str
        cat(tweets, "tweets.\n")
    }
    
    if (df){
        # reading tweets into a data frame
        results <- streamR::parseTweets(filename, verbose=FALSE)
        tempRes <- results
        class(tempRes) <- list('twitter', class(results))
        return(tempRes)        
    }
}

#' Function to read files with tweets in JSON format
#' 
#' @param path  string describing the full path of a directory that
#'  contains files in json format, or a vector of file names in
#'  in json format
#' @param source source of data in JSON format.
#' @param enc encoding of the input json file
#' @param ... additional arguments passed to \code{\link[streamR]{parseTweets}}
#' @import streamR
#' @examples 
#' \dontrun{
#' # name a directory of files in json format
#' tweets <- json("~/Dropbox/QUANTESS/corpora/tweets")
#' corpus(tweets)
#' 
#' # read a single file in json format
#' tweets <- json("~/Dropbox/QUANTESS/corpora/tweets/BarackObama.json")
#' corpus(tweets)
#' } 
#' @export
json <- function(path=NULL, source="twitter", enc = "unknown", ...) {
    stopifnot(file.exists(path))
    # identifying whether it is a folder
    if (!grepl("*.json$", path)){
        # prepare list of files if it's a folder
        fls <- list.files(path, full.names=TRUE)
        fls <- fls[grepl("*.json$", fls)]
    }
    if (grepl("*.json$", path)){
        fls <- path
    }
    # read raw json data
    txt <- unlist(sapply(fls, readLines, encoding = enc))
    
    # parsing into a data frame
    # reading tweets into a data frame
    results <- streamR::parseTweets(txt, verbose=FALSE, ...)
    tempRes <- results
    class(tempRes) <- list('twitter', class(results))
    return(tempRes)  
}


#' @title
#' Extract list of posts from a public Facebook page
#'
#' @description
#' \code{getPage} retrieves information from a public Facebook page. Note that
#' information about users that have turned on the "follow" option on their 
#' profile can also be retrieved with this function. See \code{Rfacebook} package
#' for additional methods to query the Facebook Graph API.
#'
#' @param page A page ID or page name.
#'
#' @param token An access token created at
#' \url{https://developers.facebook.com/tools/explorer}.
#'
#' @param n Number of posts of page to return. Note that number can be sometimes
#' higher or lower, depending on status of API.
#'
#' @param feed If \code{TRUE}, the function will also return posts on the page
#' that were made by others (not only the admin of the page).
#'
#' @param since A UNIX timestamp or strtotime data value that points to
#' the start of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#'
#' @param until A UNIX timestamp or strtotime data value that points to
#' the end of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#' @examples
#' \dontrun{
#' # scraping the 100 most recent posts on Barack Obama's page
#' token <- 'YOUR_FB_TOKEN_HERE'
#' pg <- getFBpage('barackobama', token, n=100)
#' # creating corpus object
#' fbcorpus <- corpus(pg)
#' summary(fbcorpus)
#' # viewing the DFM using a word cloud
#' fbDfm <- dfm(fbcorpus, ignoredFeatures = stopwordsGet())
#' plot(fbDfm)
#' }
#' @author Pablo Barbera
#' @export
getFBpage <- function(page, token, n=100, since=NULL, until=NULL, feed=FALSE){
    if(!requireNamespace("Rfacebook", quietly = TRUE)){
        stop("The Rfacebook packages is needed for this function to work. Please install it.",
             call. = FALSE)
    }
    df <- Rfacebook::getPage(page, token, n, since, until, feed)
    tempRes <- df
    class(tempRes) <- list('facebook', class(df))
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


#' @param textCol  The column of the sheet that contains the texts
#'   the docvars from. By defauls, takes everything except the textCol by
#'   \code{sep} or from meta-data embedded in the text file header
#'   (\code{headers}).
#' @rdname corpus
#' @export
#' @examples 
#' \dontrun{
#'} 
#' 
corpus.excel <- function(x, enc=NULL, docnames=row.names(x),
                         textCol=1, docvarsfrom=NULL, 
                         source=NULL, notes=NULL, citation=NULL, ...) {
    if (is.null(docvarsfrom)){
        docvarsfrom <- -textCol
    }
    if (class(x)[1] != "excel") stop("first argument must be an excel sheet")
    x <- data.table(x)
    class(x) <- (list("excel", "data.table", "data.frame")) # data.table unclasses it
    dvars <- NULL    
    txts <- as.character(unlist(x[,textCol,with=FALSE]))
    dvars <- x[, docvarsfrom, with=FALSE]
    tmpCorp <- corpus(txts, enc=enc, docnames=docnames, docvars=dvars,
                      source=source, notes=notes, citation=citation)
    return(tmpCorp)
}

#' @rdname corpus
#' @export
corpus.facebook <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content ("message" of posts)
    texts <- x$message
    atts <- as.data.frame(x[,2:ncol(x)])    
    
    # not sure I'm doing this the right way... What is metadata?
    corpus(texts, docvars=atts,
           source=paste("Converted from posts on Facebook page"),
           enc=enc, ...)
}

#' @rdname corpus
#' @export
corpus.twitter <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content (texts)
    texts <- x$text
    atts <- as.data.frame(x[,2:ncol(x)])    
    
    # using docvars inappropriately here but they show up as docmeta given 
    # the _ in the variable names
    corpus(texts, docvars=atts,
           source=paste("Converted from twitter search results"),
           enc=enc, ...)
}

#' Constructor for corpus objects from urls
#' 
#' Creates a corpus from a url object. 
#' @rdname corpus
#' @export
corpus.url <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content (texts)
    txt <- paste(scan(x, what="character", sep="\n"), collapse="\n")
    dets <- summary(x)
    close(x)
    corpus(txt, source=paste(sprintf("Loaded from url at %s:", dets$description ),
                             enc=enc, ...))
    
}


