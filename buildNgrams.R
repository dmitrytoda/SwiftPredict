# input: n-gram frequency table as data frame
# output: same data frame with out-of-dictionary words
# replaced by <UNK> and collapsed together, with each word in its own column as factor
library(dplyr)
library(data.table)
removeOOD <- function(df, dict) {
        print("Calling removeOOD")
        
        # get a separate data frame with one column per word (as factors)
        split_words <- data.frame(do.call('rbind', strsplit(df$feature,' ',fixed=TRUE)))
        
        # remove the unsplit feature from the original DF to free up memory
        df$feature <- NULL
        gc(full=TRUE)
        
        # loop over columns
        for (i in colnames(split_words)) {
                # cur_col <- split_words[,i] 
                levels(split_words[,i]) <- c(levels(split_words[,i]), '<UNK>') # add <UNK> level
                split_words[,i][! split_words[,i] %in% dict] <- '<UNK>' # replace OOD words with <UNK>
                split_words[,i] <- droplevels(split_words[,i])
                # cur_col -> split_words[,i]
        }
        
        # a DF with each word in its column and frequency as the last one
        df <- cbind(split_words, df$frequency)
        rm(split_words)
        gc(full=TRUE)
        colnames(df)[ncol(df)] <- 'frequency'
        
        # remove n-grams where last word in <UNK>
        # except in unigrams
        if(ncol(df) > 2)
                df <- df[df[,ncol(df)-1] != '<UNK>',]
        
        # collapse all rows that have the same n-gram (as many OOD words were mapped to the same <UNK> token)
        df %>% 
                group_by_at(colnames(blogs_ngrams5)[1:ncol(blogs_ngrams5)-1]) %>% 
                summarise(frequency=sum(frequency)) %>%
                arrange(desc(frequency))
}

# max_n <- 6
# ngrams <- lapply(1:max_n, nFreq, train.tokens)
# 
# 
# tidy_ngrams <- lapply(ngrams, removeOOD)
# colnames(tidy_ngrams[[1]])[1] <- 'X1' # removeOOD produces a weird column name for unigrams
# 
# # create data.table instead of data.frame
# dt_ngrams <- lapply(tidy_ngrams, setDT)
# 
# # for each n-gram data.table, set X1, X2.. as keys
# lapply(dt_ngrams, function(x) setkeyv(x, colnames(x)[1:length(colnames(x))-1]))
# 
# 
#        
        
