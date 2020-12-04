ngrams <- lapply(1:6, nFreq, train.tokens)

# input: n-gram frequency table as data frame
# output: same data frame with out-of-dictionary words
# replaced by <UNK> and collapsed together, with each word in its own column as factor
library(dplyr)
removeOOD <- function(df) {
        print("Calling removeOOD")
        
        # get a separate data frame with one column per word (as factors)
        split_words <- data.frame(do.call('rbind', strsplit(df$feature,' ',fixed=TRUE)))
        
        # loop over columns
        for (i in colnames(split_words)) {
                cur_col <- split_words[,i] 
                levels(cur_col) <- c(levels(cur_col), '<UNK>') # add <UNK> level
                cur_col[! cur_col %in% dict] <- '<UNK>' # replace OOD words with <UNK>
                cur_col <- droplevels(cur_col)
                cur_col -> split_words[,i]
        }
        
        # a DF with each word in its column and frequency as the last one
        result <- cbind(split_words, df$frequency)
        colnames(result)[ncol(result)] <- 'frequency'
        
        # remove n-grams where last word in <UNK>
        # except in unigrams
        if(ncol(result) > 2)
                result <- result[result[,ncol(result)-1] != '<UNK>',]
        
        # collapse all rows that have the same n-gram (as many OOD words were mapped to the same <UNK> token)
        result %>% 
                group_by_at(colnames(split_words)) %>% 
                summarise(frequency=sum(frequency)) %>%
                arrange(desc(frequency))
}

tidy_ngrams <- lapply(ngrams, removeOOD)
colnames(tidy_ngrams[[1]])[1] <- 'X1' # removeOOD produces a weird column name for unigrams


       
        
