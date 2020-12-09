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

temp <- next_word("I love you, my dear Lily! moar")
head(temp)
