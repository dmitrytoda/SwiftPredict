train.corpus <- files2sentences(c('./data/en_US/sample.twitter.txt'
                                  , './data/en_US/sample.blogs.txt'
                                  , './data/en_US/sample.news.txt'))
train.tokens <- tokens(
        train.corpus,
        what = "word",
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE,
        remove_separators = TRUE,
        split_hyphens = FALSE,
        include_docvars = FALSE,
        padding = FALSE
)

# keep only tokens that contain at least one letter
train.tokens <- tokens_select(
        train.tokens, 
        "[a-z]+", 
        valuetype = "regex", 
        selection = "keep",  
        case_insensitive = TRUE)

# remove tokens that contain weird characters,
# i.e. anything but letters, digits and #'.-’ signs
train.tokens <- tokens_select(
        train.tokens, 
        "[^\\#a-z0-9\\'\\.\\-]+", 
        valuetype = "regex", 
        selection = "remove",  
        case_insensitive = TRUE)

# function to create n-gram frequency table
nFreq <- function(n, my_tokens) {
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

