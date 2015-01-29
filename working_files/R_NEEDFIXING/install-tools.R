## Re-install \link{quanteda} from github
##
## Refresh the installation from the github repository for the package.  Useful if 
## you need to pull the latest changes.
## @param branch default is \code{"dev"}
## @return Nothing
## @author Kenneth Benoit
## @export
quantedaRefresh <- function(branch=c("dev", "master")) {
    branch <- match.arg(branch)
    if (!require(devtools)) {
        print("Error: You must first install devtools.")
        stop()
    }
    install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE, ref=branch)
}