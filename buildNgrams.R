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
        padding = FALSE,
        verbose = quanteda_options("verbose")
)

nFreq <- function(n, my_tokens) {
        print(paste("Calculating", n, "grams"))
        my_ngrams <- tokens_ngrams(my_tokens, n, concatenator = " ")
        my_ngrams <- dfm(my_ngrams)
        my_ngrams <- textstat_frequency(my_ngrams)
        my_ngrams[,1:2]
}

ngrams <- lapply(1:6, nFreq, train.tokens)

