# input: n-gram frequency table as data frame
# output: same data frame with out-of-dictionary words
# replaced by <UNK> and collapsed together, with each word in its own column as factor
removeOOD <- function(df, dict) {
        require(dplyr, data.table, quanteda)
        
        # get a separate data frame with one column per word (as factors)
        split_words <- data.frame(do.call('rbind', strsplit(df$feature,' ',fixed=TRUE)))
        
        # remove the unsplit feature from the original DF to free up memory
        df$feature <- NULL
        gc(full=TRUE)
        
        # loop over columns
        for (i in colnames(split_words)) {
                levels(split_words[,i]) <- c(levels(split_words[,i]), '<UNK>') # add <UNK> level
                split_words[,i][! split_words[,i] %in% dict] <- '<UNK>' # replace OOD words with <UNK>
                split_words[,i] <- droplevels(split_words[,i])
        }
        
        # a DF with each word in its column and frequency as the last one
        df <- cbind(split_words, df$frequency)
        rm(split_words)
        gc(full=TRUE)
        colnames(df)[ncol(df)] <- 'frequency'
        
        # remove n-grams where last word in <UNK>
        df <- df[df[,ncol(df)-1] != '<UNK>',]
        
        # collapse all rows that have the same n-gram (as many OOD words were mapped to the same <UNK> token)
        df %>% 
                group_by_at(colnames(df)[1:ncol(df)-1]) %>% 
                summarise(frequency=sum(frequency)) -> df
        
        df <- setDT(df)
        # setkeyv(df, cols=colnames(df)[1:ncol(df)-1])
        df
}

# same as previous but receives a data.table with all words already
# in separate columns
removeOODdt <- function(dt, dict) {
        n <- ncol(dt)-1
        for(i in 1:n) {
                levels(dt[[i]]) <- c(levels(dt[[i]]), '<UNK>') # add <UNK> level
                dt[[i]][! dt[[i]] %in% dict] <- '<UNK>' # replace OOD words with <UNK>
                dt[[i]] <- droplevels(dt[[i]]) 
        }
        # remove ngrams where last word = <UNK> (not gonna predict it)
        dt <- dt[dt[[ncol(dt)-1]]!='<UNK>']
        collapse_ngrams(dt)
}
        
