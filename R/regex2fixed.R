

# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed(regex, types, 'fixed', case_insensitive=TRUE)
# regex2fixed(regex, types, 'regex', case_insensitive=TRUE)

regex2fixed <- function(regex, types, valuetype, case_insensitive = FALSE) {
    
    # Initialize
    fixed <- list()
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    for (pat_multi in pats_multi) {
        if(valuetype == 'fixed'){
            if(case_insensitive){
                fixed_multi <- lapply(pat_multi, function(x, y) y[toLower(y) %in% toLower(x)], types)
            }else{
                fixed_multi <- lapply(pat_multi, function(x, y) y[y %in% x], types)
            }
        }else{
            fixed_multi <- lapply(pat_multi, function(x, y) stri_subset_regex(y, x, case_insensitive = case_insensitive), types)
        }
        fixed_comb <- as.matrix(do.call(expand.grid, c(fixed_multi, stringsAsFactors = FALSE))) # create all possible combinations
        fixed <- c(fixed, unname(split(fixed_comb, row(fixed_comb))))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        if (valuetype == 'fixed'){
            if(case_insensitive){
                fixed_single <- as.list(types[toLower(types) %in% toLower(pats_single)])
            }else{
                fixed_single <- as.list(types[types %in% pats_single])
            }
        }else{
            pats_single_all <- paste0(pats_single, collapse="|")
            fixed_single <- as.list(stri_subset_regex(types, pats_single_all, case_insensitive = case_insensitive))
        }
        fixed <- c(fixed, fixed_single)
    }
    return(fixed)
}
