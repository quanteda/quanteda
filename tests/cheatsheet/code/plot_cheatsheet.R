# Plots for the quanteda cheatsheet

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)

setwd("/Users/smueller/Documents/GitHub/quanteda/tests/cheatsheet")

###
# Wordcloud
###

set.seed(3)
data_corpus_inaugural |>
    corpus_subset(President == "Obama") |>
    tokens() |> 
    tokens_remove(pattern = stopwords("en")) |> 
    dfm() |>  
    textplot_wordcloud() 
ggsave("plots/textplot_wordcloud.pdf", width = 5, height = 5)


###
# X-Ray Plot
###
data_corpus_inaugural |> 
    corpus_subset(Year > 2000) |> 
    tokens() |> 
    kwic(pattern = "american") |> 
    textplot_xray()
ggsave("plots/textplot_xray.pdf", width = 4, height = 3)

###
# Keyness plot
###


data_corpus_inaugural |>   
    corpus_subset(President %in% 
                      c("Obama", "Trump")) |>
    tokens() |> 
    dfm() |>   
    dfm_group(groups = President) |> 
    textstat_keyness(target = "Trump") |>   
    textplot_keyness(n = 10)
ggsave("plots/textplot_keyness.pdf", width = 8, height = 5)


###
# Wordscores plot
###
           
ie_dfm <- dfm(tokens(data_corpus_irishbudget2010))
doclab <- apply(docvars(data_corpus_irishbudget2010, c("name", "party")), 
                1, paste, collapse = " ")

refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
ws <- textmodel_wordscores(ie_dfm, refscores, smooth = 1)
pred <- predict(ws, se.fit = TRUE)

# plot estimated word positions
textplot_scale1d(ws, margin = "features", 
                 highlighted = c("minister", "have", "our", "budget"))

# plot estimated document positions
textplot_scale1d(pred, margin = "documents",
                 doclabels = doclab,
                 groups = docvars(data_corpus_irishbudget2010, "party"))
ggsave("plots/textplot_scale1d.pdf", width = 4.2, height = 3)

