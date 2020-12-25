### stupid prediction ###
source("next_word.R")

stupid_predict("screw that", dt_ngrams, dict50)

temp <- stupid_predict("Jerry was", dt_ngrams, dict50)
head(temp, 10)

### katz prediction ###
test_disc <- 0.6

temp <- partial_complete(c('sell', 'the'), freqs = toy.ngrams, disc = test_disc, dict=toy.dict)

temp <- partial_complete(c('i', 'love'), freqs = dt_ngrams, disc = 0.5, dict=dict50, verbose=TRUE)

temp <- partial_complete(c('i', 'hui'), freqs = dt_ngrams, disc = 0.5, dict=dict50, verbose=TRUE)

temp <- partial_complete(c('i', '<UNK>'), freqs = dt_ngrams, disc = 0.5, dict=dict50, verbose=TRUE)

### Katz ###

source('Katz.R')



# observed
kbo(c('sell', 'the', 'book'), freqs = toy.ngrams, disc = test_disc, dict=toy.dict)

# unobserved
kbo(c('sell', 'the', 'house'), freqs = toy.ngrams, disc = test_disc, dict=toy.dict)
kbo(c('sell', 'the', 'buy'), freqs = toy.ngrams, disc = test_disc, dict=toy.dict)

### big dict

system.time(kbo(c('i', 'love', 'you'), freqs = dt_ngrams, disc = 0.5, dict=dict50))
system.time(kbo(c('i', 'love', 'hui'), freqs = dt_ngrams, disc = 0.5, dict=dict50))

 

