collapse_ngrams <- function(dt) {
        require(dplyr, data.table)
        dt[, .(frequency=sum(frequency)), by=eval(colnames(dt)[1:(ncol(dt)-1)])]
}


# test <- data.table(X1=c('a', 'a', 'a', 'a'),
#                    X2=c('b', 'b', 'b', 'c'),
#                    X3=c('b', 'b', 'c', 'c'),
#                    frequency=c(10,20,1,2))
# 
# collapse_ngrams(test)
