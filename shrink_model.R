# keeps top-3 prediction for each starter, replacing frequencies
# with a 1-2-3 factor rank
keep3 <- function(model) {
        max_n <- length(model)
        
        for(i in 1:max_n) {
                # set keys to everything except Xi, which equals
                # X1, X2..Xi-1, frequency
                setkeyv(model[[i]], cols = colnames(model[[i]])[-i])
                
                # keep top 3 predictions (which are at the bottom) 
                # for each starter = X1..Xi-1 combination
                model[[i]] <- model[[i]][
                        ,
                        tail(.SD,3), 
                        by=eval(ifelse(
                                i>1,
                                colnames(model[[i]])[1:(i-1)],
                                '')
                                )
                        ]
                
                # add rank column, largest count = rank 1
                model[[i]][
                        ,
                        rank:=.N:1,
                        by=eval(ifelse(
                                i>1,
                                colnames(model[[i]])[1:(i-1)],
                                '')
                                )
                        ]
                model[[i]]$rank <- as.factor(model[[i]]$rank)
                model[[i]]$frequency <- NULL           
                
                # set new keys: starter + rank
                setkeyv(model[[i]], colnames(model[[i]])[-i])
        }
        model
}

shrink_model <- function(model, dict, min_count=0) {
        model <- lapply(model, removeOODdt, dict)
        if(min_count>0)
                model <- lapply(model, function(x) x[frequency>=min_count])
        keep3(model)
}