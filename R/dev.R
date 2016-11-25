# This function generates random texts from English alphabets or any other characters.

# @param n_doc the number of documents generated
# @param n_word the number of words in documents
# @param len_word the size of longest words
# @param n_type the number of tyeps of tokens appear in the documents
# @param fast if true, words are generated from uniform random distribution over characters to speed up
# @param code if true, the function return R code instead of vector
# @param seed a integer passed to set.seed() to generate replicable random texts 
# @param characters a vector of letters for random word generation
#
# texts_random(5, 20, seed=1234)
# texts_random(5, 20, seed=1234, code=TRUE)
# texts_random(5, 20, seed=1234, fast=TRUE)
# 
# hiragana <- 
#     c('あ', 'い', 'う', 'え', 'お', 'か', 'き', 'く', 'け', 'こ', 
#       'さ', 'し', 'す', 'せ', 'そ', 'た', 'ち', 'つ', 'て', 'と', 
#       'な', 'に', 'ぬ', 'ね', 'の', 'は', 'ひ', 'ふ', 'へ', 'ほ', 
#       'ま', 'み', 'む', 'め', 'も', 'ら', 'り', 'る', 'れ', 'ろ', 
#       'よ', 'や', 'ゆ', 'わ', 'を', 'ん')
# texts_random(5, 10, seed=1234, punct=FALSE, characters = hiragana)

texts_random <- function(n_doc=10, 
                         n_word=100, 
                         len_word=5, 
                         n_type=1000, 
                         fast=FALSE, 
                         code=FALSE,
                         seed, characters){
    
    if(!missing(seed)) set.seed(seed)
    if(missing(characters)){
        # Empirical distribution in English (https://en.wikipedia.org/wiki/Letter_frequency)
        chars <- letters
        prob_chars <-c(0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 
                       0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
                       0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 
                       0.00978, 0.02360, 0.00150, 0.01974, 0.00074)
    }else{
        # Log-normal distribution
        chars <- characters
        dist_chars <- rlnorm(length(chars))
        prob_chars <- sort(dist_chars / sum(dist_chars), decreasing = TRUE)
    }
    if(n_type > length(chars) ^ len_word) error('n_type is too large')
    
    # Generate unique types
    type <- c()
    if(fast){
        pat <- stri_flatten(c('[', chars, ']'))
        while(n_type > length(type)){
            type <- unique(c(type, stringi::stri_rand_strings(n_type, 1:len_word, pat)))
        }
    }else{
        while(n_type > length(type)){
            type <- unique(c(type, word_random(chars, sample(len_word, 1), prob_chars)))
        }
    }
    type <- head(type, n_type)
    
    # Generate random text from the types
    texts <- c()
    prob_words <- zipf(n_type)
    texts <- replicate(n_doc, {
        words <- sample(type, size=n_word, replace = TRUE, prob=prob_words)
        stri_c(words, collapse = ' ')
    })
    if(code){
        return(code(texts))
    }else{
        return(texts)
    }
}

word_random <- function(chars, len_word, prob){
    stri_flatten(sample(chars, len_word, replace = TRUE, prob = prob)) 
}

zipf <- function(n_type){
    (1 / 1:n_type) / n_type
}

code <- function(texts){
    len <- length(texts)
    cat(paste0('txt <- c("', texts[1], '",\n'))
    for(text in texts[2:(len-1)]){
        cat(paste0('         "', text, '",\n'))
    }
    cat(paste0('         "', texts[len], '")\n'))
}
