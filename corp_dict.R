train.corpus <- files2sentences(c('./data/en_US/sample.twitter.txt'
                                  , './data/en_US/sample.blogs.txt'
                                  , './data/en_US/sample.news.txt'))
train.tokens <- str2tokens(train.corpus)

# function to create n-gram frequency table
nFreq <- function(n, my_tokens) {
        print(Sys.time())
        print(paste("Calculating", n, "grams"))
        my_ngrams <- tokens_ngrams(my_tokens, n, concatenator = " ")
        my_ngrams <- dfm(my_ngrams)
        my_ngrams <- textstat_frequency(my_ngrams)
        my_ngrams[,1:2]
}

# create unigram frequency table
words <- nFreq(1, train.tokens)
words$perc <- words$frequency / sum(words$frequency)
words$cumperc <- cumsum(words$perc)

# create dictionary
dict_size <- 50000
dict <- words[1:dict_size,]$feature

