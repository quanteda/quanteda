# tokens-only lexical diversity measures replaced by shorter versions ---------

#' Computes the Moving-Average Type-Token Ratio 
#' 
#' Takes a tokens object which should have been preprocessed - removal of
#' punctuation etc. and computes the Moving-Average Type-Token Ratio from
#' Covington & McFall (2010).
#' @param x a \link{tokens} object
#' @param window_size integer; the size of the moving window for computation of
#'   TTR, between 1 and the number of tokens of the document
#' @param all_windows default = FALSE. If TRUE, returns a vector with the MATTR
#'   for each window
#' @param mean_mattr default = TRUE. Returns the mean MATTR, given the window
#'   size, for the tokens object.
#' @return return either a vector with the MATTR for each window, or the mean
#'   MATTR for the whole tokens object
#' @keywords internal tokens
compute_mattr <- function(x, window_size = NULL, all_windows = FALSE, mean_mattr= TRUE) {
    
    # Error Checks
    if (is.null(window_size)) stop(message_error("window_size must be specified"))
    # Get number of tokens across each individual document
    num_tokens <- sum(ntoken(x))
    if (window_size > num_tokens)
        stop(message_error("window_size must be smaller than total ntokens for each document"))
    if (!all_windows && !mean_mattr)
        stop(message_error("at least one MATTR value type to be returned"))
    if (all_windows)
        mean_mattr <- FALSE
    if (mean_mattr)
        all_windows <- FALSE

    # List to Store MATTR Values for each Window
    mattr_list <- list()

    # Initializers
    start <- 1
    end <- window_size

    all_tokens <- unlist(x)
    temp_ls <- all_tokens[start:end]

    while (end <= num_tokens){
        # Each MATTR value is named with the start token number and end token number
        window_name <- paste0("MATTR_tokens", start, "_", end)
        temp_toks <- tokens(paste(unlist(temp_ls), collapse = " "))
        typecount <- ntype(temp_toks)[[1]]
        tokcount <- ntoken(temp_toks)[[1]]
        mattr_list[[window_name]] <- typecount / tokcount
        start <- start + 1
        end <- end + 1
        overlap <- temp_ls[2:length(temp_ls)]
        if (end <= num_tokens)
            temp_ls <- c(overlap, all_tokens[end])
    }

    mattr_list <- unlist(mattr_list)
    ## Checks
    if (length(mattr_list) != (num_tokens - window_size + 1))
        stop("Internal error within compute_mattr")
    else {
        if (!all_windows && mean_mattr) return(mean(mattr_list))
        if (!all_windows && !mean_mattr) return(mattr_list)
    }
}

#' Computes the Mean Segmental Type-Token Ratio
#' 
#' Computes the Mean Segmental Type-Token Ratio (Johnson 1944) for a tokens
#' object.
#' @param x a \link{tokens} object
#' @param segment_size input: the size of the segment. 
#' @param discard_remainder default = TRUE. If TRUE, the final segment of the document not divisible by segment_size is dropped. 
#' @param all_segments default = FALSE. If TRUE, returns a vector with the TTR for each segment. 
#' @param mean_sttr default = TRUE. Returns the Mean TTR across Segments for the tokens object. 
#' @return returns a vector with the MSSTR for each segment
#' @keywords internal tokens
compute_msttr <- function(x, segment_size = NULL, discard_remainder = TRUE, all_segments = FALSE, mean_sttr = TRUE){
    # Error Checks
    if (is.null(segment_size)) stop(message_error("segment_size must be specified"))
    if (!all_segments && !mean_sttr)
        stop(message_error("at least one MSTTR value type to be returned"))
    if (all_segments) mean_sttr <- FALSE
    if (!mean_sttr) all_segments <- FALSE

    # Get number of tokens across all documents
    num_tokens <- sum(ntoken(x))
    if (segment_size > num_tokens) stop(message_error("segment_size must be smaller than total ntokens across all documents"))

    # Checks for divisibility of the tokens object by segment_size
    remainder <- num_tokens %% segment_size
    n_segments <- num_tokens %/% segment_size
    # Warning raiser if not perfectly divisible
    if (remainder != 0) warning(paste("ntokens =", num_tokens, "not perfectly divisible by segment_size.",
                                      n_segments, "segments of size", segment_size,
                                      "and last segment of size", remainder))

    # Warning of discard of remainder
    if (discard_remainder && remainder != 0)
        warning(paste("Last segment of size ", remainder, "will be discarded"))

    # List to Store MSTTR Values for each Window
    msttr_list <- list()

    # Initializers
    start <- 1
    end <- segment_size

    all_tokens <- unlist(x)
    temp_ls <- all_tokens[start:end]

    while (end <= num_tokens){
        # Each MSSTR segment is named with the start token number and end token number
        segment_name <- paste0("MSTTR_tokens", start, "_", end)
        temp_toks <- tokens(paste(unlist(temp_ls), collapse = " "))
        typecount <- ntype(temp_toks)[[1]]
        tokcount <- ntoken(temp_toks)[[1]]
        msttr_list[[segment_name]] <- typecount / tokcount
        start <- start + segment_size
        end <- end + segment_size
        if (end <= num_tokens) {
            temp_ls <- all_tokens[start:end]
        } else {
            if (start < num_tokens && end > num_tokens) {
                if (!discard_remainder) {
                    end <- num_tokens
                    temp_ls <- all_tokens[start:end]
                } else if (discard_remainder) {
                    break
                }
            }
        }
    }

    msttr_list <- unlist(msttr_list)

    if (remainder == 0 && length(msttr_list) != n_segments)
        stop("Internal error within compute_msttr")

    if (remainder != 0  && discard_remainder && length(msttr_list) != n_segments)
        stop("Internal error within compute_msttr")

    if (remainder != 0 && !discard_remainder && length(msttr_list) != (n_segments + 1))
        stop("Internal error within compute_msttr")

    if (!all_segments && mean_sttr) return(mean(msttr_list))
    if (!all_segments && !mean_sttr) return(msttr_list)
}
