combined_predict <- function(starter, model, dict, verbose=FALSE) {
        max_n <- length(model)
        starter <- str2tokens(starter, "vector", dict)
        
        # for empty starter, predict the unigrams
        if(length(starter)==0) return(as.character(model[[1]]$X1))
        
        # take only max_n-1 last words for prediction
        if (length(starter)>max_n-1) starter <- starter[(length(starter)-max_n+2):length(starter)]
        
        res <- vector(mode='character')
        path <- list()
        
        while(length(unique(res))<3) {
                n <- length(starter)+1
                cond <- my_cond(starter)
                curr <- model[[n]][eval(cond)]
                if(!is.na(curr$rank)) res <- c(res, as.character(curr[[n]]))
                path[[n]] <- curr
                starter <- starter[2:(n-1)]
        }
        
        if(verbose) 
                list(unique(res)[1:3], path)
        else unique(res)[1:3]
}