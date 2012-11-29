
library(ca)

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


##
## Investigate the languages in which speakers spoke
##
## change mixed languages to a single language, following manual inspection
table(debates$bill, debates$language)
#debates$speech[debates$language=="de.fr" & debates$bill=="energy1"]
#debates$speech[debates$language=="de.fr" & debates$bill=="energy2"]
#debates$speech[debates$language=="de.fr" & debates$bill=="pension1"]
# first pension1 speech mostly french
debates$language[which(debates$language=="de.fr" & debates$bill=="pension1")[1]] <- "fr"
#debates$speech[debates$language=="de.fr" & debates$bill=="pension2"]
# second pension1 speech mostly french
debates$language[which(debates$language=="de.fr" & debates$bill=="pension2")[2]] <- "fr"
# change the rest to german
debates$language[which(debates$language=="de.fr")] <- "de"
table(debates$bill, debates$language)

# some speakers spoke in two different languages in the same debate
t <- split(debates$language, list(debates$mp_id, debates$bill))
tu <- lapply(t, unique)
lapply(t[which(sapply(tu, function(x) length(x)>1))], table)
# examine those
debates$speech[debates$mp_id==345 & debates$bill=="energy2"]
debates$speech[debates$mp_id==1127 & debates$bill=="energy2"]
debates$speech[debates$mp_id==1279 & debates$bill=="energy2"]
debates$speech[debates$mp_id==503 & debates$bill=="pension2"]
debates$speech[debates$mp_id==1152 & debates$bill=="immigration1"]



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




# The lists of speakers by debate can be accessed like this:
speakers.de <- split(subset(debates$mp_id, debates$language=="de" ),
                     subset(debates$bill, debates$language=="de"))
speakers.de <- lapply(speakers.de, unique)
German <- sapply(speakers.de, length)

speakers.fr <- split(subset(debates$mp_id, debates$language=="fr"), 
                     subset(debates$bill, debates$language=="fr"))
speakers.fr <- lapply(speakers.fr, unique)
French <- sapply(speakers.fr, length)

#speakers.tr <-  split(subset(debates$mp_id, debates$language %in% c("fr","de","it"
#) & debates$debates$procedural==0 & debates$interaction==0), 
#                  subset(debates$bill, debates$language %in% c("fr","de", "it") & # debates$procedural==0 & debates$interaction==0))
# speakers.tr <- lapply(speakers.tr, unique)
# Translated <- sapply(speakers.tr, length)
# table of total unique speakers
# data.frame(rbind(German, French, Translated))


## create the corpus object
debates.corpus <- corpus.create(debates$speech, 
                                attribs=debates[,-which(names(debates)=="speech")])
names(debates.corpus$attribs)[which(names(debates.corpus$attribs)=="number.1")] <- "number"

## summarize each set of texts
for (b in c("energy1", "energy2", "pension1", "pension2", "immigration1")) {
  for (l in c("de", "fr")) {
  summary(debates.corpus, 
          select=c(firstname,lastname,bill,language),
          subset=(bill==b & language==l 
                  & role=="" 
                  & procedural==0 
                  & interaction==0 
              #    & general_positiontaking==1 
                  & wordcount>=0))
}
}

## merge in the translated texts to the corpus
#load("dcAll.Rdata")

#dcdefren$attribs[which(dcdefren$attribs$tr2de==". . . . . . "),]
#sum(strtrim(dcdefren$attribs$tr2de, 6)==". . . ")

#debates.corpus$attribs <- 
#  merge(debates.corpus$attribs, 
#        dcdefren$attribs[,c("texts","firstname","number", "tr2de", "tr2fr", "tr2en")], 
#        by=c("texts", "firstname", "number"))

# make sure the non-translated text columns get the original text
#debates.corpus$attribs$tr2de[which(is.na(debates.corpus$attribs$tr2de))] <-
#  debates.corpus$attribs$texts[which(is.na(debates.corpus$attribs$tr2de))]
#debates.corpus$attribs$tr2fr[which(is.na(debates.corpus$attribs$tr2fr))] <-
#  debates.corpus$attribs$texts[which(is.na(debates.corpus$attribs$tr2fr))]
#debates.corpus$attribs$tr2en[which(is.na(debates.corpus$attribs$tr2en))] <-
#  debates.corpus$attribs$texts[which(is.na(debates.corpus$attribs$tr2en))]

## extract the term-speaker matrixes for analysis

for (b in c("immigration1")) {
  for (l in c("de")) {
  cat("Processing bill:", b, "\n")
  # original texts only
  assign(paste("fvm", l, "orig.genpos.party", b, sep="."), 
         create.fvm.corpus(debates.corpus, groups="party",
                           subset=(role==""
                                   & language==l
                                   & bill==b 
                                   & procedural==0 
                                   & interaction==0 
                                   & general_positiontaking==1 
                                 #  & firstreading==1
                                 #  & wordcount>=500
                                   & (party=="CVP" | party=="FDP-Liberale" |                                      party=="GPS" | party=="SP" | party=="SVP" ) 
                                   )))
    }
  }







  # translated texts
#  temp.corpus <- debates.corpus
#  temp.corpus$attribs$texts <- temp.corpus$attribs$tr2de
#  cat(paste("fvm", "de.tran", b, "\n", sep="."))
#  assign(paste("fvm", "de.tran", b, sep="."), 
#         create.fvm.corpus(temp.corpus, groups="mp_id",
#                           subset=(role=="" & 
#                             bill==b & procedural==0 & interaction==0)))
#  temp.corpus$attribs$texts <- temp.corpus$attribs$tr2fr
#  assign(paste("fvm", "fr.tran", b, sep="."), 
#         create.fvm.corpus(temp.corpus, groups="mp_id",
#                           subset=(role=="" & 
#                             bill==b & procedural==0 & interaction==0)))
#  temp.corpus$attribs$texts <- temp.corpus$attribs$tr2en
#  assign(paste("fvm", "en.tran", b, sep="."), 
#         create.fvm.corpus(temp.corpus, groups="mp_id",
#                           subset=(role=="" & 
#                             bill==b & procedural==0 & interaction==0)))
#
# }          




## save the results
save(file=paste("~/Dropbox/transcripts_rollcalls/code\ and\ results/debates/debates_", 
                format(Sys.Date(), "%d%b%Y"), ".Rdata", sep=""), 
     list=ls(pattern="^fvm\\.|debates|^speakers"))

