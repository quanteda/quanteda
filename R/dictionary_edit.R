#' Conveniently edit dictionaries
#'
#' Provides convenient editing of dictionaries, using an interactive editor.
#' @param x a [dictionary] or (list of) character elements
#' @return an edited version of the input object
#' @export
#' @examples
#' \dontrun{
#' # edit some supplied stopwords
#' my_posneg_dict <- dictionary_edit(data_dictionary_LSD2015[1:2])
#' }
dictionary_edit <- function(x) {
    UseMethod("dictionary_edit")
}

#' @export
dictionary_edit.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "dictionary_edit"))
}

#' @export
dictionary_edit.dictionary2 <- function(x) {
    x <- as.dictionary(x)
    x_meta <- meta(x)
    x <- dictionary(list_edit(as.list(x)), tolower = FALSE)
    meta(x) <- x_meta
    title <- meta(x, "title")
    if (!is.null(title)) meta(x, "title") <- paste(title, "(edited)")
    x
}
    
#' @rdname dictionary_edit
#' @description
#' `list_edit()` and `char_edit()` provide lower-level convenience functions
#' for interactive editing of (lists of) character objects.  These can be useful
#' for instance in editing stopword lists.
#' @importFrom yaml write_yaml read_yaml
#' @importFrom utils edit
#' @export
list_edit <- function(x) {
    tfile <- tempfile(fileext = ".yml")
    write_yaml(x, file = tfile)
    try(edited <- utils::edit(file = tfile), silent = TRUE)
    read_yaml(tfile)
}

#' @rdname dictionary_edit
#' @param sep a character vector of strings to append after each element,
#'   defaults to `"\n"` so that each word will be on its own line in the editor
#' @export
#' @importFrom data.table fwrite
#' @examples
#' \dontrun{
#' my_stopwords <- stopwords("en", source = "snowball") %>%
#'     char_edit()
#' }
char_edit <- function(x, sep = "\n") {
    if (!is.vector(x) || !is.character(x))
        stop("x must be a character vector")
    tfile <- tempfile(fileext = ".txt")
    fwrite(as.list(x), file = tfile, sep = sep, col.names = FALSE, quote = FALSE)
    try(edited <- utils::edit(file = tfile), silent = TRUE)
    result <- readLines(tfile, encoding = "UTF-8")
    if (result[length(result)] == "")
        result <- result[-length(result)]
    result
}

# possible way to test this:
# https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package