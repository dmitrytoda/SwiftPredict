# creates a condition that looks like X1=='i' & ... & X3=='you'
my_cond <- function(ngram) {
        cond <- ''
        for (i in 1:length(ngram)) cond <- paste0(cond, ' & X', i, '=="', ngram[i], '"')
        parse(text=substring(cond, 4))
}



# finds ngram in a frequency tables list
# if res = "bool", returns TRUE/FALSE
# if res = "count", returns absolute frequency
# if res = "perc", returns frequency as percentage (0..1)
find_ngram <- function(ngram, res="count", freqs=dt_ngrams) {
        n <- length(ngram)
        stopifnot(is.character(ngram), n<=max_n, n>0, 
                  res %in% c('bool', 'count', 'perc'))
        
        cond <- my_cond(ngram)
        
        # checks if there are rows in the corresponding table
        observed <- freqs[[n]][eval(cond), .N]>0
        
        # return TRUE/FALSE
        if (res=='bool') return(observed)
        
        # count and perc should only work with observed ngrams
        stopifnot(observed)
        
        # return absolute count
        count <- freqs[[n]][eval(cond), frequency]
        if (res=='count') return(count)
        
        # finally, return count percentage
        count / sum(freqs[[n]][,frequency])
}


# calculates the probability of a ngram
# given a frequency table
# ngram should be a character vector of length 2..max_n (no sense in calculating KBO for unigrams)
# already preprocessed to tokens like in freqs (with <UNK>)
# freqs should be a list of observed ngram frequencies
# disc is absolute discount
katz_prob <- function(ngram, freqs=dt_ngrams, disc=0.5) {
        n <- length(ngram)
        stopifnot(is.character(ngram), n<=max_n, n>=2)
        
        # if OBSERVED, return c*(this ngram) / c(the beginning of ngram)
        if(find_ngram(ngram, "bool"))
                return((find_ngram(ngram)-disc) / find_ngram(ngram[1:n-1]))
        
        # if UNOBSERVED
        
        # find observed ngrams that start with the same n-1 words
        cond <- my_cond(ngram[1:n-1])
        
        # alpha is the probability mass moved by discounting 
        # from observed to unobserved ngrams starting with the same n-1 words
        alpha <- 1 - sum(
                (freqs[[n]][eval(cond), frequency]-disc) / find_ngram(ngram[1:n-1])) 
        
        # alpha has to be distributed between all possible unobserved tails
        # in proportion to tail frequency (among unobserved tails only)
        alpha * find_ngram(ngram[n]) / 
                sum(freqs[[1]][!.(freqs[[n]][eval(cond), ..n]), frequency])
        
}

katz_prob(c("i", "hui"))

katz_prob(c("<UNK>", "123"))



katz_prob("<UNK>")

find_ngram(c("i", "love", "you"), res="count", freqs = dt_ngrams)
find_ngram(c("i", "love"), res="count", freqs = dt_ngrams)
