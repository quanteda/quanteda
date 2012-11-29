library(quanteda)

##
## Load in the Swiss debates, create a (new) corpus object for testing
##
load("~/Dropbox/transcripts_rollcalls/data/ts.NS.45_48_lse1_2012-05-11.RData")  # main data object
## select only our bills and the relevant variables
debates <- ts.NS.45_48_lse1
debates$bill <- NA
debates$bill[debates$bill1_id=="00.014"] <- "pension1"
debates$bill[debates$bill1_id=="01.022"] <- "energy1"
debates$bill[debates$bill1_id %in% c("05.093", "05.094", "06.107")] <- "pension2"
debates$bill[debates$bill1_id=="11.9008_NR_19"] <- "energy2"
debates$bill[debates$bill1_id=="02.024"] <- "immigration1"
debates <- subset(debates, 
                  subset = (!is.na(bill) & debates$council_abbr=="NR" & !is.na(speaker_biography_id)),
                  select = c("speaker_biography_id", "speaker_role",
                             "speaker_firstname", "speaker_lastname",
                             "speaker_party_identifier", "bill", "meeting_date", "meeting_number", "number", "speech", "language", "canton", "number", "council_abbr"))
names(debates) <- gsub("speaker_biography_id", "mp_id", names(debates))
names(debates) <- gsub("speaker_", "", names(debates))
names(debates) <- gsub("party_identifier", "party", names(debates))

## merge in Daniel's additional data
speech.data.pension1 <- read.csv("~/Dropbox/transcripts_rollcalls/data/revised_transcriptdata/debate.00.014.addvar.csv")
speech.data.pension2 <- read.csv("~/Dropbox/transcripts_rollcalls/data/revised_transcriptdata/debate.05.093.addvar.csv")
speech.data.energy1 <- read.csv("~/Dropbox/transcripts_rollcalls/data/revised_transcriptdata/debate.01.022.addvar.csv")
speech.data.energy2 <- read.csv("~/Dropbox/transcripts_rollcalls/data/revised_transcriptdata/debate.11.9008.addvar.csv")
speech.data.immigration1 <- read.csv("~/Dropbox/transcripts_rollcalls/data/revised_transcriptdata/debate.02.024.addvar.csv")
selectvars <- c("meeting_date", "meeting_number", "number",
                "general_positiontaking", "procedural", "interaction", "firstreading")
debates.moredata <- rbind(speech.data.pension1[,selectvars], 
                          speech.data.pension2[,selectvars], 
                          speech.data.energy1[,selectvars],
                          speech.data.energy2[,selectvars],
                          speech.data.immigration1[,selectvars])
debates <- merge(debates, debates.moredata, by=c("meeting_date", "meeting_number", "number"), all=TRUE)


# harmonize party names
debates$party[debates$party=="Al"] <- "GPS"
debates$party[debates$party=="GB"] <- "GPS"
debates$party[debates$party=="LPS"] <- "FDP-Liberale"
debates$party[debates$party=="AdG"] <- "PdA"
debates$party[debates$party=="csp-ow"] <- "CVP"






# count words in selected speeches and append wordcount variable
for (b in c("energy1", "energy2", "pension1", "pension2", "immigration1")) {
  for (l in c("de", "fr")) {
    
    speeches.temp <- subset(debates, debates$bill==b & debates$language==l 
                            & debates$role=="" 
                            & debates$procedural==0 
                            & debates$interaction==0)[c(4,9,10,11)]
    
    speeches.temp <- aggregate(speeches.temp[-c(1,2,4)], by=list(speeches.temp$mp_id, speeches.temp$bill, speeches.temp$language), c)
    speeches.temp$speech <- gsub("c(\"", "", speeches.temp$speech, fixed=TRUE)
    speeches.temp$wordcount <- sapply(gregexpr("\\W+", speeches.temp$speech), length) + 1
    assign(paste("speeches", l, b, sep="."), speeches.temp)
  }
  rm(speeches.temp)
}


wordcount <- rbind(speeches.de.energy1, speeches.fr.energy1, speeches.de.immigration1, speeches.fr.immigration1)[-c(4)]
names(wordcount) <- c("mp_id","bill","language","wordcount")
debates <- merge(debates, wordcount, all=FALSE)


## create the corpus object
swissdebates <- corpus.create(debates$speech, 
                                attribs=debates[,-which(names(debates)=="speech")])
names(swissdebates$attribs)[which(names(debates.corpus$attribs)=="number.1")] <- "number"