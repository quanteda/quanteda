require(quanteda)
require(colorspace)
# quanteda blue: 0,112,192 (RGB), #0070C0 (RGB), 205,1,0.75 (HSV)

quanteda_palette <- function(n = 1) {
    diverge_hcl(n, h = c(235, 0), c = 80, l = c(60, 80), power = 0.7)
}

quanteda_palette2 <- function(n = 1) {
    rainbow_hcl(n, c = 80, l = 60, start = 235, end = 0)
}

png('tests/misc/palette.png')
par(mfrow = c(2, 2), mar = rep(0, 4))
pie(rep(1,1), col = quanteda_palette(1))
pie(rep(1,2), col = quanteda_palette(2))
pie(rep(1,3), col = quanteda_palette(3))
pie(rep(1,5), col = quanteda_palette(5))
dev.off()

png('tests/misc/palette2.png')
par(mfrow = c(2, 2), mar = rep(0, 4))
pie(rep(1,1), col = quanteda_palette2(1))
pie(rep(1,2), col = quanteda_palette2(2))
pie(rep(1,3), col = quanteda_palette2(3))
pie(rep(1,5), col = quanteda_palette2(5))
dev.off()

choose_palette()

pres_corp <- data_corpus_inaugural %>% corpus_subset(Year > 1960)
pres_dfm <- dfm(pres_corp, remove = stopwords("english"),
                remove_punct = TRUE)

pie(rep(1,5), col = sequential_hcl(5))
pie(rep(1,5), col = diverge_hcl(5))
pie(rep(1,3), col = diverge_hcl(3))
pie(rep(1,2), col = diverge_hcl(2))
pie(rep(1,5), col = diverge_hcl(5, h = c(235, 0), c = 80, l = c(60, 80), power = 0.7))
pie(rep(1,3), col = diverge_hcl(3, h = c(235, 0), c = 80, l = c(60, 80), power = 0.7))
pie(rep(1,3), col = diverge_hcl(3, h = c(235, 0), c = 80, l = c(60, 80), power = 0.7))

pie(rep(1,5), col = quanteda_palette(5))

# keyness 
target <- c("Nixon", "Reagan", "Bush", "Trump")
pres_key <- textstat_keyness(pres_dfm, 
                             docvars(pres_corp, "President") %in% target,
                             measure = "lr")

#textplot_keyness(pres_key, color = diverge_hcl(2))
#textplot_keyness(pres_key, show_reference = FALSE, color = diverge_hcl(1))
textplot_keyness(pres_key, color = quanteda_palette(2))
textplot_keyness(pres_key, show_reference = FALSE, color = quanteda_palette(1))

# network
pres_fcm <- fcm(pres_dfm)
fcm_select(pres_fcm, names(topfeatures(pres_fcm, 50))) %>% 
textplot_network(edge_color =quanteda_palette(1), min_freq = 0.8)

# wordcloud
textplot_wordcloud(pres_dfm, min.freq = 10, color = quanteda_palette(1))
#textplot_wordcloud(pres_dfm, min.freq = 10, color = quanteda_palette(2), comparison = TRUE)
