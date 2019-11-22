# utils -----------

is_pre2 <- function(x) {
    (! "meta" %in% names(attributes(x)))
}

# docnames ----------

docnamesv2.corpus <- function(x) {
    get_docvarsv2.corpus(x, "docname_", FALSE, TRUE, TRUE)
}


# docvars -----------

docvarsv2.corpus <- function(x, field = NULL) {
    select_docvarsv2(attr(x, 'docvars'), field, user = TRUE, system = FALSE, drop = TRUE)
}

## internal function to return the docvars for all docvars functions
get_docvarsv2.corpus <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    select_docvarsv2(attr(x, "docvars"), field, user, system, drop)
}


# internal function to check if variables are internal-only
is_system <- function(x) {
    x %in% c("docname_", "docid_", "segid_")
}


# Internal function to select columns of docvars
select_docvarsv2 <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    x <- x[user * !is_system(names(x)) | system * is_system(names(x))]
    if (is.null(field)) {
        return(x)
    } else {
        error <- !field %in% names(x)
        if (any(error))
            stop("field(s) ", paste(field[error], collapse = ", "), " not found")
        if (length(field) == 1 && drop) {
            return(x[[field]])
        } else {
            return(x[field])
        }
    }
}

