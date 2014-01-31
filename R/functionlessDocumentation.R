# Roxygen comments to document stuff like the package and datasets
# that are not associated with specific functions

#' Quantitative Analysis of Textual Data
#'
#' Quanteda provides tools that make it easier to organize, 
#' convert, and analyze textual data.  The package is part of the project
#' QUANTESS: Quantitative Analysis of Textual Data for the Social 
#' Sciences, funded by European Research Council grant ERC-2011-StG 
#' 283794-QUANTESS.
#'
#' Something here
#'
#' Quantess organizes texts into a basic corpus object...
#' Provides tools to converting the corpus into a \emph{feature-value}
#' matrix on which quantitative analysis can be performed.
#' 
#' 
#' @references QUANTESS: Quantitative Analysis of Textual Data for the Social 
#' Sciences, funded by European Research Council grant ERC-2011-StG 
#' 283794-QUANTESS.
#' @import austin
#' @docType package
#' @name quanteda
NULL

#' Swiss speeches from Daniel Schwartz's data project
#' 
#' A dataset containing all speech from the Swiss parliament from 2000 to 2010 
#'  (or something like that - Daniel will need to correct this!)
#'  This data contains the following variables
#' 
#' \itemize{
#'   \item[billID] A numeric code that identifies the bill, using the xxx system
#'   \item[speakerID] The name of the speaker
#'   \item[type] An identifier of the type of the speech act, including...
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A corpus class object from the quanteda package.
#' @name swissdebates
NULL

