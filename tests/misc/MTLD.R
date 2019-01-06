# https://stackoverflow.com/questions/15698399/cumulative-count-of-unique-values-in-r

toks <- tokens(c("c b a d e b c a b b", 
                 "a b c d e b c a b b x y z",
                 "my test of this function",
                 "of the people, by the people, for the people",
                 "of the people, by the people, for the people"),
               remove_punct = TRUE)
factor_size <- 0.72
x <- toks

require(data.table)

compute_mtld <- function(x) {
    dt <- data.table(docid = rep(docnames(x), ntoken(x)),
                     token = unlist(x, use.names = FALSE),
                     tokenid = unlist(sapply(ntoken(x), seq_len), use.names = FALSE),
                     factor = 1,
                     factor_bool = FALSE)
    dt <- dt[docid %in% c("text1", "text2")]
    dt <- dt[docid %in% c("text2")]
    
    this_factor <- 0
    while (max(dt$factor) > this_factor) {
        this_factor <- this_factor + 1
        dt[factor >= this_factor, tokenid := seq_len(.N), by = c("docid", "factor")]
        dt[factor >= this_factor, uniqvalue := as.integer(factor(token, levels = unique(token))), 
           by = c("docid", "factor")]
        dt[factor >= this_factor, cumvalue := cummax(uniqvalue), c("docid", "factor")]
        dt[factor >= this_factor, ttr := cumvalue / tokenid]
        # dt[factor >= this_factor, remainder := all(ttr > factor_size), by = c("docid", "factor")]
        dt[factor == this_factor, 
           factor_bool := shift(ttr, 1L, "lag", fill = TRUE) > max(factor_size, min(ttr)),
           by = c("docid", "factor")]
        dt[(factor == this_factor & !(factor_bool)), factor := factor + 1,
           by = c("docid", "factor")]
    }
    data.frame(dt)
}

compute_mtld(x)
 
# identify remainders
# dt[dt[, .I[.N], by = c("docid", "factor")][["V1"]], 
#    remainder := ifelse(ttr > factor_size, TRUE, FALSE)]
# 
# 
# 
# set(dt, i = dt[, .I[.N], by = c("docid", "factor")][["V1"]], j = "remainder", 
#     value = ifelse(dt[, ttr] > factor_size, TRUE, FALSE))
# 
# # dt[, remainder := ifelse(any(remainder), TRUE, FALSE), by = c("docid", "factor")]
# 
# dt[remainder[.N] == TRUE, remainder := TRUE, by = c("docid", "factor")]
# 
# setkey(dt, docid, factor, tokenid)
# dt[, .SD[.N], by = c("docid", "factor")]
# 
# dt[dt[, .I[.N], by = c("docid", "factor")][["V1"]], remainder := TRUE]
