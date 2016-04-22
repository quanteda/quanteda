
#' convert phrases into single tokens
#' 
#' Replace multi-word phrases in text(s) with a compound version of the phrases 
#' concatenated with  \code{concatenator} (by default, the "\code{_}" character) to
#' form a single token.  This prevents tokenization of the phrases during 
#' subsequent processing by eliminating the whitespace delimiter.
#' @param object source texts, a character or character vector
#' @param phrases a \code{\link{dictionary}} object that 
#'   contains some phrases, defined as multiple words delimited by whitespace, 
#'   up to 9 words long; or a quanteda collocation object created
#'   by \code{\link{collocations}}
#' @param concatenator the concatenation character that will connect the words 
#'   making up the multi-word phrases.  The default \code{_} is highly 
#'   recommended since it will not be removed during normal cleaning and 
#'   tokenization (while nearly all other punctuation characters, at least those
#'   in the Unicode punctuation class [P] will be removed.
#' @return character or character vector of texts with phrases replaced by 
#'   compound "words" joined by the concatenator
#' @export
#' @author Kenneth Benoit
#' @examples
#' mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised a taxes: an income tax and a sales tax.")
#' mydict <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax")))
#' (cw <- phrasetotoken(mytexts, mydict))
#' dfm(cw, verbose=FALSE)
#' 
#' # when used as a dictionary for dfm creation
#' mydfm2 <- dfm(cw, dictionary = lapply(mydict, function(x) gsub(" ", "_", x)))
#' mydfm2
#' # to pick up "taxes" in the second text, set valuetype = "regex"
#' mydfm3 <- dfm(cw, dictionary = lapply(mydict, phrasetotoken, mydict),
#'               valuetype = "regex")
#' mydfm3
#' ## one more token counted for "tax" than before
setGeneric("phrasetotoken", 
           function(object, phrases, ...) 
               standardGeneric("phrasetotoken"))

#' @rdname phrasetotoken
#' @export
setMethod("phrasetotoken", signature = c("corpus", "ANY"), 
          function(object, phrases, ...) {
              texts(object) <- phrasetotoken(object, phrases, ...)
              object
          })


#' @rdname phrasetotoken
#' @export
#' @examples 
#' # using a dictionary to pre-process multi-word expressions
#' myDict <- dictionary(list(negative = c("bad* word*", "negative", "awful text"),
#'                           postiive = c("good stuff", "like? th??")))
#' txt <- c("I liked this, when we can use bad words, in awful text.",
#'          "Some damn good stuff, like the text, she likes that too.")
#' phrasetotoken(txt, myDict)
#'
setMethod("phrasetotoken", signature = c("character", "dictionary"), 
          function(object, phrases, ...) {
              phraseConcatenator <- phrases@concatenator
              phrasesTmp <- unlist(phrases, use.names = FALSE)
              compoundPhrases <- phrasesTmp[stringi::stri_detect_fixed(phrasesTmp, phraseConcatenator)]
              # replace string concatenator with simple space
              compoundPhrases <- stringi::stri_replace_all_fixed(compoundPhrases, phraseConcatenator, " ")
              phrasetotoken(object, compoundPhrases, ...)
          })


          
# object <- c("Kind of cool, yeah text is cool is kind.",
#          "shit fire, Shitty fire.")
# phrases <- dictionary(list(one = c("kind of"), two = c("shit* fire")), concatenator = " ")
# valuetype <- "glob"
# phrasetotoken(object, phrases)
# 
# stri_replace_all_regex(c("Fuck fire is kind of hot, yeah kind of cool is kind.", "fuck fire, Shit fuck fire."),
#                     c("\\bis\\p{WHITE_SPACE}kind\\b", "\\bfuck\\p{WHITE_SPACE}fire\\b"), c("A_B", "C_D"),
#                     vectorize_all = TRUE)
# 

setClass("collocations", contains = "data.table")

#' @rdname phrasetotoken
#' @export
setMethod("phrasetotoken", signature = c("character", "collocations"), 
          function(object, phrases, ...) {
              word1 <- word2 <- word3 <- NULL
              # concatenate the words                               
              word123 <- phrases[, list(word1, word2, word3)]
              mwes <- apply(word123, 1, paste, collapse=" ")
              # strip trailing white space (if no word 3)
              mwes <- stringi::stri_trim_both(mwes)
              phrasetotoken(object, mwes, ...)
          })

#' @rdname phrasetotoken
#' @param valuetype how to interpret word matching patterns: \code{"glob"} for 
#'   "glob"-style wildcarding, \code{fixed} for words as 
#'   is; \code{"regex"} for regular expressions
#' @param case_insensitive if \code{TRUE}, ignore case when matching
#' @param ... additional arguments passed through to core \code{"character,character"} method
#' @export
#' @examples 
#' # on simple text
#' phrasetotoken("This is a simpler version of multi word expressions.", "multi word expression*")
setMethod("phrasetotoken", signature = c("character", "character"), 
          function(object, phrases, concatenator = "_", valuetype = c("glob", "regex", "fixed"), 
                   case_insensitive = TRUE, ...) {
              valuetype <- match.arg(valuetype)
              if (valuetype == "glob" | valuetype == "fixed")
                  compoundPhrases <- stringi::stri_replace_all_fixed(phrases, c("*", "?"), 
                                                                     c("[^\\s]*", "[^\\s]"), 
                                                                     vectorize_all = FALSE)
              
              compoundPhrasesList <- strsplit(compoundPhrases, "\\s")
              
              for (l in compoundPhrasesList) {
                  re.search <- paste("(\\b", paste(l, collapse = paste0(")\\p{WHITE_SPACE}+(")), "\\b)", sep = "")
                  re.replace <- paste("$", 1:length(l), sep = "", collapse = concatenator)
                  object <- stringi::stri_replace_all_regex(object, re.search, re.replace, case_insensitive = case_insensitive)
              }
              object
          })



# @rdname phrasetotoken
# @export
# phrasetotoken.character <- function(x, dictionary, concatenator="_") {
#     # get the tokenized list of compound phrases from a dictionary (list)
#     phrases <- unlist(dictionary, use.names=FALSE)
#     compoundPhrases <- phrases[grep(" ", phrases)]
#     compoundPhrasesList <- tokenize(compoundPhrases)
#     
#     # contenate the phrases in
#     # gsub("(word1)\\s(word2)", "\\1_\\2", "word1 word2")
#     ## [1] "word1_word2"
#     for (l in compoundPhrasesList) {
#         re.pattern <- paste("(", 
#                             paste(l, collapse=")\\s("),
#                             ")", sep="")
#         re.replace <- paste("\\", 1:length(l), sep="", collapse=concatenator)
#         x <- gsub(re.pattern, re.replace, x, perl=TRUE)
#     }
#     x    
# }

# gapTokenize <- function(txt) {
#     tokenVec <- tokenize(txt, removePunct=FALSE, simplify=TRUE)
#     punctEndIndex <- grep("[])};:,.?!]", tokenVec) # don't pad if last token
#     if (length(punctEndIndex) > 0) {
#         for (i in 1:(length(punctEndIndex))) {
#             if (punctEndIndex[i]+i-1 == length(tokenVec)) break
#             tokenVec <- c(tokenVec[1:(i-1+punctEndIndex[i])], "", tokenVec[(i+punctEndIndex[i]):length(tokenVec)])
#         }
#     }
#     punctBegIndex <- grep("[[({]", tokenVec)
#     if (length(punctBegIndex) > 0) {
#         for (i in 1:(length(punctBegIndex))) {
#             if (punctBegIndex[i] == 1) continue  # don't pad if first token
#             tokenVec <- c(tokenVec[1:(i-2+punctBegIndex[i])], "", tokenVec[(i-1+punctBegIndex[i]):length(tokenVec)])
#         }
#     }
#     # now remove the rest of the stuff not yet cleaned
#     clean(tokenVec, removeDigits = FALSE, toLower = FALSE, removeURL = FALSE)
# }
