# Build word frequency table

library(dplyr)

# creates a table of words with their frequencies
wordFreq <- function(tokens, lower=TRUE) {
        # tokens - list of tokenized texts (twits, news etc)
        
        # transform list to vector (mix all twits/news/etc together)
        if(lower) 
                tokens <- tolower(unlist(tokens))
        else 
                tokens <- unlist(tokens)
        
        # remove names (they were full phrases initially)
        names(tokens) <- NULL
        
        # create a dataframe with word counts and sort it
        result <- as.data.frame(table(tokens))
        names(result) <- c('word', 'freq')
        result <- arrange(result, desc(freq))
        
        result
}

# creates a table of 2-grams with their frequencies
twoGramFreq <- function(tokens, lower=TRUE) {
        
}
