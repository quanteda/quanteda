<<<<<<< HEAD
## Example: Creating the Irish budget speeches corpus
## 
## Ken Benoit
##

library(quanteda)

# import the first set from the FF-Green government
texts <- getTextDir("./iebudgets_texts/FF-Green_govt/")
# parse the filenames to get variable (attribute) values
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <- data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
# name the new attributes
names(newattribs) <- c("year", "debate", "no", "nameFirst", "nameLast", "party")
# remove the .txt stuck onto the party name
newattribs$party <- gsub(".txt", "", newattribs$party)
# create the corpus, assigning the newattribs as text attributes
iebudgets1 <- corpusCreate(texts, attribs=newattribs)
summary(iebudgets1, nmax=10)
# add a government-opposition attribute
iebudgets1 <- 
  corpusAddAttributes(iebudgets1,
                      factor(newattribs$party %in% c("FF", "Green"), labels=c("Govt", "Opp")),
                      name="govt")
summary(iebudgets1, nmax=10)


## now add a second set to the first corpus
newtexts <- getTextDir("./iebudgets_texts/FG-Lab_govt/")
newparts <- strsplit(getRootFileNames(names(newtexts)), "_")
newattribs2 <- data.frame(matrix(unlist(newparts), nrow=length(newparts), byrow=TRUE))
names(newattribs2) <- c("year", "debate", "no", "fname", "speaker", "party")
newattribs2$party <- gsub(".txt", "", newattribs2$party)
iebudgets2 <- corpusAppend(iebudgets1, newtexts, newattribs2)

=======
## Example: Creating the Irish budget speeches corpus
##
## Highlights issues in assigning variables (attributes) to the documents
## in a text corpus, and in appending new texts to an existing corpus
##
## Ken Benoit
##

library(quanteda)

## import the first set from the FF-Green government
texts <- getTextDir("./iebudgets_texts/FF-Green_govt/")
# parse the filenames to get variable (attribute) values
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <- data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
# name the new attributes
names(newattribs) <- c("year", "debate", "no", "nameFirst", "nameLast", "party")
# remove the .txt stuck onto the party name
newattribs$party <- gsub(".txt", "", newattribs$party)
# create the corpus, assigning the newattribs as text attributes
iebudgets1 <- corpusCreate(texts, attribs=newattribs)
summary(iebudgets1, nmax=10)
# add a government-opposition attribute
iebudgets1 <- 
  corpusAddAttributes(iebudgets1,
                      factor(newattribs$party %in% c("FF", "Green"), labels=c("Govt", "Opp")),
                      name="govt")
summary(iebudgets1, nmax=10)


## now add a second set to the first corpus
newtexts <- getTextDir("./iebudgets_texts/FG-Lab_govt/")
newparts <- strsplit(getRootFileNames(names(newtexts)), "_")
newattribs2 <- data.frame(matrix(unlist(newparts), nrow=length(newparts), byrow=TRUE))
names(newattribs2) <- c("year", "debate", "no", "nameFirst", "nameLast", "party")
newattribs2$party <- gsub(".txt", "", newattribs2$party)
newattribs2$govt <- factor(newattribs2$party %in% c("FG", "Lab"), labels=c("Govt", "Opp"))

## append the first to the second corpus
iebudgets.created <- corpusAppend(iebudgets1, newtexts, newattribs2)
summary(iebudgets.created, nmax=10)



>>>>>>> FETCH_HEAD
