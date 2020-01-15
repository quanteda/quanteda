
##### data_corpus_inaugual

# fix line breaks and escaped quotes in Bush 2005 text
texts(data_corpus_inaugural)["2005-Bush"] <- readtext::readtext("tests/data_creation/inaugural/2005-Bush.txt")[["text"]]

# structure metadata
meta(data_corpus_inaugural) <- list( 
    description = "Transcripts of all inaugural addresses delivered by United States Presidents, from Washington 1789 onward.  Data compiled by Gerhard Peters.",
    source = "Gerhard Peters and John T. Woolley. The American Presidency Project.",
    url = "https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/inaugural-addresses",
    author = "(various US Presidents)",
    keywords = c("political", "US politics", "United States", "presidents", "presidency"),
    title = "US presidential inaugural address speeches"
)

# read in party
docvars(data_corpus_inaugural) <- read.csv("tests/data_creation/inaugural/inaugural_docvars.csv",
                                           stringsAsFactors = FALSE)
data_corpus_inaugural$Party <- factor(data_corpus_inaugural$Party)

data_corpus_inaugural <- quanteda:::add_summary_metadata(data_corpus_inaugural)

usethis::use_data(data_corpus_inaugural, overwrite = TRUE)


##### data_corpus_dailnoconf1991

for (i in docnames(data_corpus_dailnoconf1991)) {
    
    txt <- texts(data_corpus_dailnoconf1991)[i] %>%
        # fix paragraph delimiters
        stringi::stri_replace_all_regex("([^\n])\\n{1}([^\n])", "$1\n\n$2") %>%
        # fix page references such as "[545]"
        stringi::stri_replace_all_regex("\\[\\d+\\]", "")
        
    # fix filename
    fname <- stringi::stri_replace_all_regex(i, "^v*(.*)\\.txt$", "$1") %>%
        stringi::stri_replace_last_regex("_$", "")
    
    # cat(txt, file = paste0("tests/data_creation/dailnoconf/", fname, ".txt"))
}


rt <- readtext::readtext("tests/data_creation/dailnoconf/*.txt")
texts(data_corpus_dailnoconf1991) <- rt[["text"]]
Encoding(texts(data_corpus_dailnoconf1991)) <- "UTF-8"
docnames(data_corpus_dailnoconf1991) <- stringi::stri_replace_last_fixed(rt$doc_id, ".txt", "")

# add metdata
meta(data_corpus_dailnoconf1991) <- list( 
    description = "Texts of speeches from a no-confidence motion debated in the Irish Dáil (parliament) from 16-18 October 1991 over the future of the Fianna Fail-Progressive Democrat coalition.",
    source = "Laver, M. & Benoit, K.R. (2002). Locating TDs in Policy Spaces: Wordscoring Dáil Speeches. Irish Political Studies, 17(1), 59–73.

Laver, M., Benoit, K.R., & Garry, J. (2003). Estimating Policy Positions from Political Text using Words as Data. American Political Science Review, 97(2), 311–331.",
    url = "https://www.oireachtas.ie/en/debates/find/?debateType=dail",
    author = "(various members of the Irish parliament)",
    keywords = c("political", "Irish politics", "Ireland", "no-confidence motion", "legislature"),
    title = "Confidence debate from 1991 Irish Parliament"
)

# add summary
data_corpus_dailnoconf1991 <- quanteda:::add_summary_metadata(data_corpus_dailnoconf1991)

usethis::use_data(data_corpus_dailnoconf1991, overwrite = TRUE)



##### data_corpus_irishbudget2010

texts(data_corpus_irishbudget2010) <- 
    stringi::stri_replace_all_regex(texts(data_corpus_irishbudget2010), "([^\n])\\n{1}([^\n])", "$1\n\n$2")

# structure metadata
meta(data_corpus_irishbudget2010) <- list( 
    description = "Transcripts from a debate in the Irish Dáil (parliament) taking place on 9 December 2009, over the 2010 austerity budget.  Consists of 14 speches.",
    source = "Lowe, W. & Benoit, K.R. (2013). Validating Estimates of Latent Traits From Textual Data Using Human Judgment as a Benchmark. Political Analysis, 21(3), 298–313.",
    url = "https://www.oireachtas.ie/en/debates/debate/dail/2009-12-09/",
    author = "(various members of the Irish parliament)",
    keywords = c("political", "Irish politics", "Ireland", "budget debate", "legislature"),
    title = "Irish budget speeches from 2010"
)

# add summary
data_corpus_irishbudget2010 <- quanteda:::add_summary_metadata(data_corpus_irishbudget2010)

usethis::use_data(data_corpus_irishbudget2010, overwrite = TRUE)

