# keeps top-3 prediction for each starter, replacing frequencies
# with a 1-2-3 factor rank
keep3 <- function(ngrams) {
        # set keys to everything except Xn, which equals
        # X1, X2..Xn-1, frequency
        n <- ncol(ngrams)-1
        setkeyv(ngrams, cols = colnames(ngrams)[-n])
        
        # keep top 3 predictions (which are at the bottom) 
        # for each starter = X1..Xi-1 combination
        ngrams <- ngrams[
                ,
                tail(.SD,3), 
                by=eval(if(n>1) colnames(ngrams)[1:(n-1)] else NULL)
                ]
        
        # add rank column, largest count = rank 1
        ngrams[
                ,
                rank:=.N:1,
                by=eval(if(n>1) colnames(ngrams)[1:(n-1)] else NULL)
                ]
        ngrams$rank <- as.factor(ngrams$rank)
        ngrams$frequency <- NULL           
        
        # set new keys: starter + rank
        setkeyv(ngrams, colnames(ngrams)[-n])
        ngrams
}