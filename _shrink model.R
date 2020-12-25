for(i in 1:6) {
        print(paste('Loading', i, 'grams'))
        print(Sys.time())
        varname <- paste0('ngrams',i)
        load(paste0('data/',varname))
        
        print(paste('Shrinking', i, 'grams'))
        print(Sys.time())
        assign(varname, shrink_ngrams(eval(parse(text=varname)), dict20, prune=1))
        
        print(paste('Saving', i, 'grams'))
        print(Sys.time())
        save(list=varname, file=paste0('data/prune1_top3model_n', i, '_dict20'))
        rm(list=varname)
        gc(full=TRUE)
}

for(i in 1:6) {
        load(paste0('data/prune1_top3model_n',i,'_dict20'))
}

model20 <- list(ngrams1, ngrams2, ngrams3, ngrams4, ngrams5, ngrams6)
rm(ngrams1, ngrams2, ngrams3, ngrams4, ngrams5, ngrams6)

model20 <- lapply(model20, function(x) x)

combined_predict("Hello mother fucker", model20, dict20)
