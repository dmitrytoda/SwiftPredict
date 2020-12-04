# calculates the probability of a ngram
# given a frequency table
# ngram should be a character vector (6 elements or less)
# already preprocessed to tokens like in freqs (with <UNK>)
# freqs should be a list of observed ngram frequencies
# disc is absolute discount
katz_prob <- function(ngram, freqs=tidy_ngrams, disc=0.5) {
        stopifnot(is.character(ngram), length(ngram)<=6, length(ngram)>0)
        
        # for unigram, just  return its frequency percentage
        if(length(ngram)==1) {
                unigrams <- freqs[[1]]
                return(unigrams[unigrams$X1==ngram,]$frequency / sum(unigrams$frequency))
        }

        # if observed, return c*(this ngram) / c(the beginning of ngram)
        
}


katz_prob(c("<UNK>", "123"))

katz_prob(c("i", "love", "you"))
