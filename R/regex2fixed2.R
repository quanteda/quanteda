# This function converts regex to fixed patterns. This is one of the coner strones of 
# the new artchitecture, but not really fast. Performance improvement is needed.

# @param regex regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
#
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed2(regex, types, 'fixed', case_insensitive=TRUE)
# regex2fixed2(regex, types, 'regex', case_insensitive=TRUE)

regex2fixed2 <- function(regex, types, valuetype, case_insensitive = FALSE) {
    
    # Initialize
    fixed <- list()
    if(case_insensitive){
        types_lower <- toLower(types)
        regex <- lapply(regex, toLower)
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    for(pat_multi in pats_multi) {
        if(valuetype == 'fixed'){
            if(case_insensitive){
                fixed_multi <- lapply(pat_multi, subset_types_fixed, types, types_lower)
            }else{
                fixed_multi <- lapply(pat_multi, subset_types_fixed, types, types)
            }
        }else{
            if(case_insensitive){
                fixed_multi <- subset_types(pat_multi, types, types_lower)
            }else{
                fixed_multi <- subset_types(pat_multi, types, types)
            }
        }
        print(fixed_multi)
        fixed_comb <- as.matrix(do.call(expand.grid, c(fixed_multi, stringsAsFactors = FALSE))) # create all possible combinations
        fixed <- c(fixed, unname(split(fixed_comb, row(fixed_comb))))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        if (valuetype == 'fixed'){
            if(case_insensitive){
                fixed_single <- as.list(subset_types_fixed(pats_single, types, types_lower))
                #fixed_single <- as.list(types[remove_na(fmatch(pats_single, types_lower))])
            }else{
                fixed_single <- as.list(subset_types_fixed(pats_single, types, types))
                #fixed_single <- as.list(types[remove_na(fmatch(pats_single, types))])
            }
        }else{
            if(case_insensitive){
                fixed_single <- unlist(subset_types(pats_single, types, types_lower), use.names = FALSE)
            }else{
                fixed_single <- unlist(subset_types(pats_single, types, types), use.names = FALSE)
            }
        }
        fixed <- c(fixed, fixed_single)
    }
    return(fixed)
}

# This function subset types avoiding expensive full regular expression matching
subset_types <- function (regex, types, types_search){
    
    subset <- lapply(regex, function(x, y, z){
                c("Search ", x, '\n')
                head <- (stri_sub(x, 1, 1) == '^')
                tail <- (stri_sub(x, -1, -1) == '$')
                if(head & tail & !is_regex((body <- stri_sub(x, 2, -2)))){
                    subset_types_fixed(body, y, z)
                }else if(head & !is_regex((headless <- stri_sub(x, 2, -1)))){
                    y[stri_startswith_fixed(z, headless)]   
                }else if(head & !is_regex((tailless <- stri_sub(x, 1, -2)))){
                    y[stri_endswith_fixed(z, tailless)]
                }else{
                    y[stri_detect_regex(z, x)]
                }
            }, types, types_search)
    return(subset)
}

subset_types_fixed <- function(fixed, types, types_search){
    id <- fmatch(types_search, fixed)
    return(types[!is.na(id)])
}

# This function checks if a string is regular expression
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}
