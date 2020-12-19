library(quanteda)
library(data.table)
# creates a condition that looks like .(ngram[1], ngram[2],.., ngram[n])
my_cond <- function(ngram) {
        cond <- ".("
        varname <- deparse(substitute(ngram))
        for (i in 1:length(ngram)) cond <- paste0(cond, varname, "[", i, "],")
        substr(cond, nchar(cond), nchar(cond)) <- ")"
        parse(text=cond)
}

# finds ngram in a frequency tables list
# if res = "bool", returns TRUE/FALSE
# if res = "count", returns absolute frequency
# if res = "perc", returns frequency as percentage (0..1)
find_ngram <- function(ngram, res="count", freqs=dt_ngrams) {
        n <- length(ngram)
        stopifnot(is.character(ngram), n<=length(dt_ngrams), n>0, 
                  res %in% c('bool', 'count', 'perc'))
        
        cond <- my_cond(ngram)
        
        # get count of this ngram; if unobserved, it will be NA
        count <- freqs[[n]][eval(cond), frequency]
        
        # return TRUE/FALSE
        if (res=='bool') return(!is.na(count))
        
        # count and perc should only work with observed ngrams
        stopifnot(!is.na(count))
        
        # return absolute count
        if (res=='count') return(count)
        
        # finally, return count percentage
        count / sum(freqs[[n]][,frequency])
}

# returns a character vector of all unobserved tail words 
# for a given ngram
B <- function(ngram, freqs=dt_ngrams, dict=dict50) {
        n <- length(ngram)+1
        stopifnot(is.character(ngram), n<=length(dt_ngrams), n>=2)
        
        cond <- my_cond(ngram)
        observed_tails <- freqs[[n]][eval(cond)][[n]]
        dict[!dict %in% observed_tails]
}


# calculates alpha: probability mass moved from observed
# (n+1)-grams starting with the given n-gram
# to unobserved (n+1)-grams
# if the given ngram is itself unobserved, returns 1
alpha <- function(ngram, freqs=dt_ngrams, disc=0.5) {
        n <- length(ngram)+1
        stopifnot(is.character(ngram), n<=length(dt_ngrams), n>=2)
        
        cond <- my_cond(ngram)
        if(find_ngram(ngram, "bool", freqs=freqs)) {
                1 - sum((freqs[[n]][eval(cond), frequency]-disc)) / find_ngram(ngram, freqs=freqs)
        } else {
                1
        }
}

# calculates the probability that a ngram has the tail it does
# given a frequency table
# ngram should be a character vector of length 1..max_n 
# already preprocessed to tokens like in freqs (with <UNK>)
# freqs should be a list of observed ngram frequencies
# disc is absolute discount (same for all levels of n)
kbo <- function(ngram, freqs=dt_ngrams, disc=0.5, dict=dict50) {
        n <- length(ngram)
        stopifnot(is.character(ngram), n<=length(dt_ngrams), n>=1)
        
        ### PART 1: unigrams - return MLE
        if(n==1) {
                return(find_ngram(ngram, "perc", freqs))
        }
        
        ### PART 2: 2+grams
        ## 2A: if OBSERVED, return c*(this ngram) / c(the beginning of ngram)
        if(find_ngram(ngram, "bool", freqs)) {
                # print(paste('Observed:', paste(ngram, collapse=' ')))
                return((find_ngram(ngram, "count", freqs)-disc) / find_ngram(ngram[1:(n-1)], "count", freqs))
        }
        
        ## 2B: if UNOBSERVED
        # print(paste("Unobserved:", paste(ngram, collapse = ' ')))
        # find observed ngrams that start with the same n-1 words
        my_alpha <- alpha(ngram[1:(n-1)], freqs, disc)
        my_B <- B(ngram[1:(n-1)], freqs, dict=dict)
        
        # 2Bi: unobserved BIgram: return alpha, distributed over 
        # all B-words according to their MLE (within B)
        if(n==2) {
                # unobserved unigram frequencies corresponding to B-words
                return(my_alpha *  
                               freqs[[1]][.(ngram[n]), frequency] / 
                               sum(freqs[[1]][.(my_B),frequency]))
        }
        
        # 2Bii: unobserved 3+gram: return alpha, distributed over
        # all B-words according to KBO of (n-1)-grams
        
        if(n==3) {
                # my_B are unobserved tails X coming after w1_w2_X
                # they are further subdivided into A2 and B2
                # depending on whether just w2_X is observed
                starter <- ngram[2:(n-1)]
                B2 <- B(starter, freqs, my_B)
                A2 <- my_B[! my_B %in% B2]
                
                # combinations starter_A2 are observed
                # combinations starter_B2 are UNobserved
                # probabilities for both of them are returned
                # by bi_probs2()
                
                # prob = alpha * numerator / denominator
                numerator <- kbo(ngram[2:n], freqs = freqs, disc = disc, dict = dict)
                denominator <- sum(bi_probs(starter, A2, B2, freqs=freqs, disc=disc, dict=dict))
                my_alpha * numerator / denominator
        } else {
                simpleError("Tried to calculate a probability of 4+gram")
                stop()
        }
}

bi_probs <- function (starter, A, B, freqs, disc, dict) {
        # probabilities of observed bigrams A
        cond <- my_cond(starter)
        obs_freqs <- freqs[[length(starter)+1]][eval(cond)]
        a_probs <- (obs_freqs[.(starter, A)]$frequency-disc) / sum(obs_freqs$frequency)
        
        # probabilities of unobserved bigrams B
        bi_alpha <- alpha(starter, freqs, disc)
        tails <- freqs[[1]][.(B)] # unigram frequencies of the tails of unobserved bigrams B
        denom <- sum(tails[,frequency])
        b_probs <- bi_alpha * tails$frequency / denom
        return(c(a_probs, b_probs))
}
          