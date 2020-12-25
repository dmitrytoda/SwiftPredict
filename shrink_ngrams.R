# receives ngrams data.table with frequencies
# removes OOD words and collapses duplicates
# prunes ngrams with frequencies < min_count
# keeps top3 predictions, replacing frequencies with ranks
shrink_ngrams <- function(ngrams, dict, prune=0) {
        ngrams <- removeOODdt(ngrams, dict)
        if(prune>0)
                ngrams <- ngrams[frequency>prune]
        keep3(ngrams)
}