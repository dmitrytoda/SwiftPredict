# based on starter ngram, calculates probabilities for all observed
# completions of it; if that's not enough to find the top probability,
# calculates unobserved probabilites one by one until a champion is found
partial_complete <- function(starter, freqs, disc, dict, verbose=FALSE) {
        # if starter is unobserved, shorten it by one word from the left until
        # an observed one is found
        while(!find_ngram(starter, res='bool', freqs=freqs)) {
                if(verbose) print(sprintf("Starter '%s' is UNobserved", paste(starter, collapse = ' ')))
                starter <- starter[2:length(starter)]
        }
        
        if(verbose) print(sprintf("Starter '%s' is observed", paste(starter, collapse = ' ')))
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
                
                if(verbose & i %% 100 == 0) print(sprintf("%d: max_prob = %f, remaining prob = %f", i, max(res$prob), 1 - sum(res$prob)))
        }
        return(res[order(-prob)])
        
}

# a VERY stupid backoff: neither true probabilities nor scores are calulated
stupid_predict <- function(starter, freqs, dict) {
        max_n <- length(freqs)
        input <- str2tokens(starter, "vector", dict)
        
        # for empty starter, predict the 3 most common words
        if(length(input)==0) return(c('the', 'to', 'and'))
        
        # take only max_n-1 last words for prediction
        if (length(input)>max_n-1) input <- input[(length(input)-max_n+2):length(input)]
        
        # shorten the starter until it is found in the ngram tables
        while(!find_ngram(input, res="bool", freqs=freqs)) {
                input <- input[2:length(input)]
        }
        
        cond <- my_cond(input)
        X <- length(input)+1
        top3 <- freqs[[length(input)+1]][eval(cond)][order(-frequency)][1:3,..X]
        as.character(unlist(top3))
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
