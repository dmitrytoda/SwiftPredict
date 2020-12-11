# based on starter ngram, calculates probabilities for all observed
# completions of it; if that's not enough to find the top probability,
# calculates unobserved probabilites one by one until a champion is found
partial_complete <- function(starter, freqs, disc, dict) {
        cond <- my_cond(starter)
        n <- length(starter)+1
        observed_tails <- as.character(freqs[[n]][eval(cond)][[n]])
        if(!is.na(observed_tails)) {
                observed_probs <- sapply(observed_tails, function(x) kbo(c(starter, x), freqs, disc, dict))
                res <- data.table(tail=observed_tails, prob=observed_probs)
                if(max(res$prob) > 1 - sum(res$prob)) return(res[order(-prob)])
        } else res <- NULL

        unobserved_tails <- freqs[[1]][!observed_tails][order(-frequency)]
        i <- 1
        while(is.null(res) | max(res$prob) < 1 - sum(res$prob)) {
                this_tail <- as.character(unobserved_tails[i, X1])
                this_prob <- data.table(
                        tail=this_tail, 
                        prob=kbo(c(starter, this_tail), freqs, disc, dict))
                i <- i+1
                res <- rbind(res, this_prob)
                
                if(i %% 100 == 0) sprintf("%d: max_prob = %f, remaining prob = %f", i, max(res$prob), 1 - sum(res$prob))
        }
        return(res[order(-prob)])
        
}


# predicts next words based on any input string
next_word <- function(str) {
        input <- str2tokens(str, "vector")
        l <- length(input)
        
        # take only max_n-1 last words for prediction
        if (l>max_n-1) input <- input[(l-max_n+2):l]
        
        res <- data.frame(X6=dict)
        #res$p <- katz_prob(c(input, res$X6))
        #res
        input
}
