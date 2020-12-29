combined_predict <- function(starter, model, dict, verbose=FALSE) {
        # starter - any string
        # model - list of ngram data.tables, with either frequency or rank as last column
        # dict - character vector with dictionary
        # verbose - whether to include path into output
        
        require(data.table)
        require(quanteda)
        source("my_cond.R")
        source("str2tokens.R")
        stopifnot(sapply(model, is.data.table))
        max_n <- length(model)
        input <- starter
        starter <- str2tokens(starter, "vector", dict)
        
        
        
        # for empty starter, predict the unigrams
        if(length(starter)==0) {
                if(verbose) {
                        return(list(as.character(model[[1]]$X1), model[[1]]))
                } else return(as.character(model[[1]]$X1))
        }
        
        # take only max_n-1 last words for prediction
        if (length(starter)>max_n-1) starter <- starter[(length(starter)-max_n+2):length(starter)]
        
        res <- vector(mode='character')
        path <- list()
        
        while(length(unique(res))<3) {
                n <- length(starter)+1
                cond <- my_cond(starter)
                curr <- model[[n]][eval(cond)]
                if(!is.na(curr[[n+1]])) { # if something was found at current n
                        if(colnames(curr)[n+1]=='rank') {
                                # rank models are already sorted
                                res <- c(res, as.character(curr[[n]])) 
                                path[[n]] <- curr
                        } else if(colnames(curr)[n+1]=='frequency') {
                                # frequency models need to be sorted by desc frequency
                                res <- c(res, as.character(curr[order(-frequency)][[n]])) 
                                path[[n]] <- curr[order(-frequency)]
                        } else stop("Predict error: last column is neither rank nor frequency")
                        
                }
                if(n>2) { # if currently looking in 3+grams, chop off one word
                        starter <- starter[2:(n-1)]
                } else { # if currently looking in 2-grams, add unigram predictions
                        if(colnames(curr)[n+1]=='rank') {
                                # rank models are already sorted
                                res <- c(res, as.character(model[[1]]$X1)) 
                                path[[1]] <- model[[1]]
                        } else if(colnames(curr)[n+1]=='frequency') {
                                # frequency models need to be sorted by desc frequency
                                res <- c(res, as.character(model[[1]][order(-frequency)]$X1)) 
                                path[[1]] <- model[[1]][order(-frequency)]
                        } else stop("Predict error: last column is neither rank nor frequency")   
                }
        }
        
        if(verbose) 
                list(unique(res)[1:3], rev(path[!sapply(path, is.null)]))
        else unique(res)[1:3]
}