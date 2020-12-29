# function to create n-gram frequency table
nFreq <- function(n, my_tokens) {
        my_ngrams <- tokens_ngrams(my_tokens, n, concatenator = " ")
        my_ngrams <- dfm(my_ngrams)
        my_ngrams <- textstat_frequency(my_ngrams)
        my_ngrams[,1:2]
}

