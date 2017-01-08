#' View methods for quanteda
#' 
#' Redefines utils::View() as a generic, so that we can define methods for 
#' kwic to produce a nice viewer.
#' @param x	an R object which can be coerced to a data frame with non-zero numbers of rows and columns
#' @param title for viewer window. Defaults to name of x prefixed by \code{Data:}
#' @seealso \link[utils]{View}
#' @keywords internal View
#' @export
View <- function(x, title) {
    UseMethod("View")
}

#' @rdname View
#' @export
View.default <- function(x, title) {
    mget('View', as.environment('package:utils'))[[1]](x)
}

#' @rdname View
#' @export
#' @examples 
#' \dontrun{
#' ## for kwic
#' View(kwic(data_char_ukimmig2010, "illeg*"))
#' 
#' ## for data.frame
#' View(dfm(data_char_ukimmig2010))
#' }
View.kwic <- function(x, title) {
    if ("DT" %in% installed.packages()[, "Package"]) {
        DT::datatable(x, options = list(pageLength = 20), rownames = FALSE,
                      class = "display",
                      colnames = c("document", "position", "pre", "keyword", "post"))    
    } else if ("xtable" %in% installed.packages()[, "Package"]) {
        filename <- tempfile(, fileext = ".html")
        temp_xtable <- xtable::xtable(x)
        print(temp_xtable, type = "html", sanitize.text.function = identity, file = filename,
              html.table.attributes = getOption("xtable.html.table.attributes", "border=1 cellpadding=5 cellspacing=3 style=\"border-collapse:collapse; font-family:arial;\""))
        utils::browseURL(filename)
    } else {
        stop("You must have xtable or DT installed to view kwic objects.")
    }
}

#' @rdname View
#' @export
View.dfmSparse <- function(x, title) {
    mget('View', as.environment('package:utils'))[[1]](x)
}
